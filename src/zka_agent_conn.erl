%%
%% Copyright 2023 The University of Queensland
%% Author: Alex Wilson <alex@uq.edu.au>
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
%% IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
%% NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
%% THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%

-module(zka_agent_conn).

-include_lib("public_key/include/public_key.hrl").
-include_lib("ssh_agent/include/SSHSignature.hrl").

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

-export([
    start_link/4,
    stop/1
    ]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_info/2,
    handle_cast/2
    ]).

start_link(Agent, Zone, LSock, Keys) ->
    gen_server:start_link(?MODULE, [Agent, Zone, LSock, Keys], []).

stop(Pid) ->
    case (catch gen_server:call(Pid, stop, 1000)) of
        ok -> ok;
        _ -> exit(Pid, kill), ok
    end.

-define(SSH_AGENTC_REQUEST_IDENTITIES, 11).
-define(SSH_AGENTC_SIGN_REQUEST, 13).
-define(SSH_AGENTC_LOCK, 22).
-define(SSH_AGENTC_UNLOCK, 23).
-define(SSH_AGENTC_EXTENSION, 27).

-define(SSH_AGENT_SUCCESS, 6).
-define(SSH_AGENT_FAILURE, 5).
-define(SSH_AGENT_EXT_FAILURE, 28).
-define(SSH_AGENT_IDENTITIES_ANSWER, 12).
-define(SSH_AGENT_SIGN_RESPONSE, 14).

-define(SSH_AGENT_RSA_SHA2_256, 2).
-define(SSH_AGENT_RSA_SHA2_512, 4).

-record(?MODULE, {
    agent :: pid(),
    zone :: binary(),
    lsock :: gen_tcp:socket(),
    sock :: undefined | gen_tcp:socket(),
    keys :: [{public_key:public_key(), public_key:private_key()}]
    }).

init([Agent, Zone, LSock, Keys]) ->
    self() ! accept,
    {ok, #?MODULE{agent = Agent, zone = Zone, lsock = LSock, keys = Keys}}.

%% @private
terminate(_Why, #?MODULE{}) ->
    ok.

%% @private
handle_call(stop, From, S0 = #?MODULE{}) ->
    gen_server:reply(From, ok),
    {stop, normal, S0}.

%% @private
handle_info(accept, S0 = #?MODULE{agent = Agent, lsock = LSock}) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    zka_agent:expand(Agent),
    {noreply, S0#?MODULE{sock = Sock}};

handle_info({tcp, Sock, Data}, S0 = #?MODULE{sock = Sock}) ->
    case Data of
        <<?SSH_AGENTC_REQUEST_IDENTITIES>> ->
            handle_request_identities(S0);
        <<?SSH_AGENTC_SIGN_REQUEST,
          KBlobSize:32/big, KBlob:KBlobSize/binary,
          SDataSize:32/big, SData:SDataSize/binary,
          Flags:32/big>> ->
            handle_sign_request(KBlob, SData, Flags, S0);
        <<?SSH_AGENTC_EXTENSION,
          ExtLen:32/big, Ext:ExtLen/binary,
          Rest/binary>> ->
            handle_extension(Ext, Rest, S0);
        <<Op, _/binary>> ->
            lager:debug("unsupported operation: ~B", [Op]),
            gen_tcp:send(Sock, <<?SSH_AGENT_FAILURE>>),
            {noreply, S0};
        Else ->
            gen_tcp:send(Sock, <<?SSH_AGENT_FAILURE>>),
            {noreply, S0}
    end;

handle_info({tcp_closed, Sock}, S0 = #?MODULE{sock = Sock}) ->
    {stop, normal, S0};

handle_info(Msg, S = #?MODULE{}) ->
    lager:debug("got unknown message: ~p", [Msg]),
    {noreply, S}.

encode_identity(PubKey, Comment) ->
    KeyBlob = ssh_file:encode(PubKey, ssh2_pubkey),
    CommentBin = iolist_to_binary(Comment),
    [<<(byte_size(KeyBlob)):32/big>>, KeyBlob, <<(byte_size(CommentBin)):32/big>>,
     CommentBin].

handle_request_identities(S0 = #?MODULE{zone = Z, sock = Sock, keys = Keys}) ->
    lager:debug("~s: request identities", [Z]),
    gen_tcp:send(Sock, [<<?SSH_AGENT_IDENTITIES_ANSWER, (length(Keys)):32/big>>,
        [encode_identity(Pub, Name) || {Pub, _Priv, Name} <- Keys]]),
    {noreply, S0}.

handle_sign_request(KeyBlob, Data, Flags, S0 = #?MODULE{zone = Z, sock = Sock, keys = Keys}) ->
    PubKey = ssh_file:decode(KeyBlob, ssh2_pubkey),
    case lists:keyfind(PubKey, 1, Keys) of
        false ->
            lager:debug("~s: sign request for non-existent key", [Z]),
            gen_tcp:send(Sock, <<?SSH_AGENT_FAILURE>>);
        {PubKey, PrivKey, Name} ->
            lager:debug("~s: sign request for key ~s", [Z, Name]),
            {Digest, Alg} = case PubKey of
                {#'ECPoint'{}, {namedCurve, ?'secp256r1'}} ->
                    {sha256, <<"ecdsa-sha2-nistp256">>};
                {#'ECPoint'{}, {namedCurve, ?'secp384r1'}} ->
                    {sha384, <<"ecdsa-sha2-nistp384">>};
                {#'ECPoint'{}, {namedCurve, ?'secp521r1'}} ->
                    {sha512, <<"ecdsa-sha2-nistp521">>};
                #'RSAPublicKey'{} ->
                    case Flags of
                        ?SSH_AGENT_RSA_SHA2_256 ->
                            {sha256, <<"rsa-sha2-256">>};
                        ?SSH_AGENT_RSA_SHA2_512 ->
                            {sha512, <<"rsa-sha2-512">>};
                        _ ->
                            {sha, <<"ssh-rsa">>}
                    end;
                {ed_pub, ed25519, _} ->
                    {none, <<"ssh-ed25519">>}
            end,
            Asn1Sig = public_key:sign(Data, Digest, PrivKey),
            SshSig = case Alg of
                <<"ecdsa-sha2-", Curve/binary>> ->
                    {ok, #'ECDSASignature'{r = RI, s = SI}, <<>>} =
                        'SSHSignature':decode('ECDSASignature', Asn1Sig),
                    R = <<0, (binary:encode_unsigned(RI))/binary>>,
                    S = <<0, (binary:encode_unsigned(SI))/binary>>,
                    <<(byte_size(R)):32/big, R/binary,
                      (byte_size(S)):32/big, S/binary>>;
                _ ->
                    Asn1Sig
            end,
            Sig = <<(byte_size(Alg)):32/big, Alg/binary,
                    (byte_size(SshSig)):32/big, SshSig/binary>>,
            gen_tcp:send(Sock, [<<?SSH_AGENT_SIGN_RESPONSE,
                (byte_size(Sig)):32/big>>, Sig])
    end,
    {noreply, S0}.

handle_extension(<<"query">>, _, S0 = #?MODULE{zone = Z, sock = Sock}) ->
    lager:debug("~s: extension query", [Z]),
    Exts = [<<"query">>],
    gen_tcp:send(Sock, [<<?SSH_AGENT_SUCCESS, (length(Exts)):32/big>>,
        [[<<(byte_size(X)):32/big>>, X] || X <- Exts]]),
    {noreply, S0};

handle_extension(Other, _, S0 = #?MODULE{sock = Sock, zone = Z}) ->
    lager:debug("~s: unsupported extension ~s", [Z, Other]),
    gen_tcp:send(Sock, <<?SSH_AGENT_EXT_FAILURE>>),
    {noreply, S0}.

%% @private
handle_cast(Msg, S0 = #?MODULE{}) ->
    {stop, {bad_cast, Msg}, S0}.
