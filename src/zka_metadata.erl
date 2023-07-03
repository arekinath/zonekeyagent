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

-module(zka_metadata).

-export([
    set_property/3
    ]).

-spec set_property(Zone :: iolist(), Property :: atom(), Value :: binary()) -> ok | {error, term()}.
set_property(Zone, Property, Value) ->
    MetaFile = iolist_to_binary([<<"/zones/">>, Zone, <<"/config/metadata.json">>]),
    Rand = binary:encode_hex(crypto:strong_rand_bytes(4)),
    TempFile = iolist_to_binary([<<"/zones/">>, Zone, <<"/config/.metadata.json.">>, Rand]),
    case file:read_file(MetaFile) of
        {ok, D0} ->
            M0 = jsx:decode(D0, [return_maps, {labels, atom}]),
            #{internal_metadata := IM0} = M0,
            IM1 = IM0#{Property => Value},
            M1 = M0#{internal_metadata => IM1},
            D1 = jsx:encode(M1, [space, indent]),
            case file:write_file(TempFile, D1) of
                ok ->
                    case file:rename(TempFile, MetaFile) of
                        ok -> ok;
                        Err ->
                            file:delete(TempFile),
                            Err
                    end;
                Err ->
                    Err
            end;
        Err -> Err
    end.

