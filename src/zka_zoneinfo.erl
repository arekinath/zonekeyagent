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

-module(zka_zoneinfo).

-export([
    get/1,
    list/0
    ]).

-on_load(init/0).

try_paths([Last], BaseName) ->
    filename:join([Last, BaseName]);
try_paths([Path | Next], BaseName) ->
    case filelib:is_dir(Path) of
        true ->
            WCard = filename:join([Path, "{lib,}" ++ BaseName ++ ".*"]),
            case filelib:wildcard(WCard) of
                [] -> try_paths(Next, BaseName);
                _ -> filename:join([Path, BaseName])
            end;
        false -> try_paths(Next, BaseName)
    end.

%% @private
init() ->
    Paths0 = [
        filename:join(["..", lib, zonekeyagent, priv]),
        filename:join(["..", priv]),
        filename:join([priv])
    ],
    Paths1 = case code:priv_dir(zonekeyagent) of
        {error, bad_name} -> Paths0;
        Dir -> [Dir | Paths0]
    end,
    SoName = try_paths(Paths1, "zka_zoneinfo_nif"),
    erlang:load_nif(SoName, 0).

-type errno() :: integer().
%% System errno value

-type func() :: atom().

-type err_result() :: {error, {func(), errno(), string()}} | {error, term()}.

-type zonename() :: iolist().

-type zonestatus() :: uninitialized | initialized | ready | booting | running |
    shutting_down | empty | down | dying | dead | unknown.

-type zoneid() :: integer().

-type zoneinfo() :: #{
    id => zoneid(),
    name => binary(),
    root => binary(),
    brand => binary(),
    status => zonestatus()
    }.

-spec get_async(zonename() | zoneid()) -> reference().
get_async(_ZoneName) -> error(no_nif).

-spec get(zonename() | zoneid()) -> {ok, zoneinfo()} | err_result().
get(ZoneName) ->
    Ref = get_async(ZoneName),
    receive {Ref, Result} -> Result end.

-spec list_async() -> reference().
list_async() -> error(no_nif).

-spec list() -> {ok, [zoneid()]} | err_result().
list() ->
    Ref = list_async(),
    receive {Ref, Result} -> Result end.
