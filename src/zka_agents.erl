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

-module(zka_agents).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

-export([
    start_link/0,
    get/1,
    get_or_start/3,
    claim/1
    ]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_info/2,
    handle_cast/2
    ]).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get(binary()) -> {ok, pid()} | {error, term()}.
get(Zone) ->
    gen_server:call(?MODULE, {get, Zone}, infinity).

-spec get_or_start(binary(), binary(), binary()) -> {ok, pid()} | {error, term()}.
get_or_start(Zone, SockDir, KeyDir) ->
    gen_server:call(?MODULE, {get_or_start, Zone, SockDir, KeyDir}, infinity).

-spec claim(binary()) -> ok | {error, term()}.
claim(Zone) ->
    gen_server:call(?MODULE, {claim, Zone, self()}, infinity).

-record(?MODULE, {
    zs = #{} :: #{binary() => {pid(), reference()}},
    mons = #{} :: #{reference() => binary()}
    }).

%% @private
init([]) ->
    {ok, #?MODULE{}}.

%% @private
terminate(_Why, #?MODULE{}) ->
    ok.

%% @private
handle_call({get, Zone}, _From, S0 = #?MODULE{zs = Z0}) ->
    case Z0 of
        #{Zone := {Pid, _}} ->
            {reply, {ok, Pid}, S0};
        _ ->
            {reply, {error, not_found}, S0}
    end;
handle_call({get_or_start, Zone, SDir, KDir}, _From, S0 = #?MODULE{zs = Z0, mons = M0}) ->
    case Z0 of
        #{Zone := {Pid, _}} ->
            {reply, {ok, Pid}, S0};
        _ ->
            lager:debug("starting agent for zone ~p", [Zone]),
            case supervisor:start_child(zka_agent_sup, [Zone, SDir, KDir]) of
                {ok, Pid} ->
                    MRef = monitor(process, Pid),
                    Z1 = Z0#{Zone => {Pid, MRef}},
                    M1 = M0#{MRef => Zone},
                    {reply, {ok, Pid}, S0#?MODULE{zs = Z1, mons = M1}};
                Err ->
                    lager:debug("agent start error: ~p", [Err]),
                    {reply, Err, S0}
            end
    end;
handle_call({claim, Zone, Pid}, _From, S0 = #?MODULE{zs = Z0, mons = M0}) ->
    case Z0 of
        #{Zone := {Pid, _}} ->
            {reply, ok, S0};
        #{Zone := {OtherPid, OtherMRef}} ->
            lager:warning("agent for zone ~p changed to ~p", [Zone, Pid]),
            MRef = monitor(process, Pid),
            demonitor(OtherMRef, [flush]),
            M1 = maps:remove(OtherMRef, M0),
            M2 = M1#{MRef => Zone},
            Z1 = Z0#{Zone => {Pid, MRef}},
            {reply, ok, S0#?MODULE{zs = Z1, mons = M2}};
        _ ->
            MRef = monitor(process, Pid),
            M1 = M0#{MRef => Zone},
            Z1 = Z0#{Zone => {Pid, MRef}},
            {reply, ok, S0#?MODULE{zs = Z1, mons = M1}}
    end.

%% @private
handle_info({'DOWN', MRef, process, Pid, _Why}, S0 = #?MODULE{zs = Z0, mons = M0}) ->
    case M0 of
        #{MRef := Zone} ->
            M1 = maps:remove(MRef, M0),
            Z1 = maps:remove(Zone, Z0),
            {noreply, S0#?MODULE{zs = Z1, mons = M1}};
        _ ->
            lager:warning("got down message from already removed agent ~p", [Pid]),
            {noreply, S0}
    end;
handle_info(Msg, S = #?MODULE{}) ->
    lager:debug("got unknown message: ~p", [Msg]),
    {noreply, S}.

%% @private
handle_cast(Msg, S0 = #?MODULE{}) ->
    {stop, {bad_cast, Msg}, S0}.
