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

-module(zka_keygen).

-compile([{parse_transform, lager_transform}]).

-include_lib("public_key/include/public_key.hrl").

-export([
    generate_all/2
    ]).

generate_all(Zone, KDir) ->
    Path = iolist_to_binary([KDir, "/auth.pem"]),
    case file:read_file(Path) of
        {ok, KD} ->
            [Ent] = public_key:pem_decode(KD),
            _PrivKey = public_key:pem_entry_decode(Ent),
            ok = file:change_mode(Path, 8#0600);
        _ ->
            lager:debug("generating new auth key for ~p", [Zone]),
            PrivKey = public_key:generate_key({namedCurve, secp256r1}),
            Ent = public_key:pem_entry_encode('ECPrivateKey', PrivKey),
            KD = public_key:pem_encode([Ent]),
            ok = file:write_file(Path, KD),
            ok = file:change_mode(Path, 8#0600),
            PubKey = ssh_file:extract_public_key(PrivKey),
            SSHKey = ssh_file:encode([{PubKey, []}], auth_keys),
            zka_metadata:set_property(Zone, auth_key, SSHKey)
    end,
    ok.
