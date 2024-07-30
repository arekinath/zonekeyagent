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

-module(zka_agent).

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

-export([
    start_link/3,
    stop/1,
    expand/1
    ]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_info/2,
    handle_cast/2
    ]).

start_link(Zone, SockPath, KeyDir) ->
    gen_server:start_link(?MODULE, [Zone, SockPath, KeyDir], [{timeout, 30000}]).

expand(Pid) ->
    gen_server:call(Pid, expand, infinity).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

-record(?MODULE, {
    zone :: binary(),
    sockpath :: binary(),
    lsock :: gen_tcp:socket(),
    kids = [] :: [pid()],
    keys :: [{public_key:public_key(), public_key:private_key(), Name :: iolist()}]
    }).

init([Zone, SockPath, KeyDir]) ->
    {ok, Fnames} = file:list_dir(KeyDir),
    Keys = lists:foldl(fun (Name, Acc) ->
        NameBin = unicode:characters_to_binary(Name, utf8),
        case binary:split(NameBin, [<<".">>], [global]) of
            [KeyName, <<"pem">>] ->
                {ok, D} = file:read_file(iolist_to_binary([KeyDir, "/", Name])),
                [Ent] = public_key:pem_decode(D),
                PrivKey = public_key:pem_entry_decode(Ent),
                PubKey = ssh_file:extract_public_key(PrivKey),
                [{PubKey, PrivKey, [KeyName, $-, Zone]} | Acc];
            _ ->
                Acc
        end
    end, [], Fnames),
    file:delete(SockPath),
    {ok, LSock} = gen_tcp:listen(0, [{ip, {local, SockPath}}, binary,
        {backlog, 100}, {packet, 4}, {active, true}]),
    process_flag(trap_exit, true),
    Kids = lists:foldl(fun (_I, Acc) ->
        {ok, Kid} = zka_agent_conn:start_link(self(), Zone, LSock, Keys),
        [Kid | Acc]
    end, [], lists:seq(1, 8)),
    self() ! claim,
    {ok, #?MODULE{zone = Zone, sockpath = SockPath, lsock = LSock,
        kids = Kids, keys = Keys}}.

%% @private
terminate(_Why, #?MODULE{}) ->
    ok.

%% @private
handle_call(expand, From, S0 = #?MODULE{kids = K0, zone = Zone, lsock = LSock, keys = Keys}) ->
    {ok, NewKid} = zka_agent_conn:start_link(self(), Zone, LSock, Keys),
    K1 = [NewKid | K0],
    {reply, ok, S0#?MODULE{kids = K1}};
handle_call(stop, From, S0 = #?MODULE{kids = K0}) ->
    [zka_agent_conn:stop(Pid) || Pid <- K0],
    gen_server:reply(From, ok),
    {stop, normal, S0}.

%% @private
handle_info(claim, S0 = #?MODULE{zone = Zone}) ->
    zka_agents:claim(Zone),
    {noreply, S0};
handle_info({'EXIT', Pid, normal}, S0 = #?MODULE{kids = K0}) ->
    K1 = K0 -- [Pid],
    {noreply, S0#?MODULE{kids = K1}};
handle_info({'EXIT', Pid, Why}, S0 = #?MODULE{kids = K0, lsock = LSock, zone = Zone, keys = Keys}) ->
    K1 = K0 -- [Pid],
    {ok, NewKid} = zka_agent_conn:start_link(self(), Zone, LSock, Keys),
    K2 = [NewKid | K1],
    {noreply, S0#?MODULE{kids = K2}};

handle_info(Msg, S = #?MODULE{}) ->
    lager:debug("got unknown message: ~p", [Msg]),
    {noreply, S}.

%% @private
handle_cast(Msg, S0 = #?MODULE{}) ->
    {stop, {bad_cast, Msg}, S0}.
