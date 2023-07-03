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

-module(zka_zone_fsm).

-behaviour(gen_statem).

-compile([{parse_transform, lager_transform}]).

-include_lib("kernel/include/file.hrl").

-export([
    start_link/1,
    state_change/2
    ]).

-export([
    callback_mode/0,
    init/1,
    terminate/3,
    await_dirs/3,
    await_running/3,
    generate_keys/3,
    agent_running/3
    ]).

-spec start_link(binary()) -> {ok, pid()} | {error, term()}.
start_link(Zone) ->
    gen_statem:start_link(?MODULE, [Zone], []).

-spec state_change(pid(), atom()) -> ok | {error, term()}.
state_change(Pid, State) ->
    Pid ! {state_change, State}.

-record(?MODULE, {
    zone :: binary(),
    zdir :: binary(),
    cdir :: binary(),
    kdir :: binary(),
    sdir :: binary()
    }).

callback_mode() -> [state_functions, state_enter].

init([Zone]) ->
    {ok, Info} = zka_zoneinfo:get(Zone),
    #{root := RDir} = Info,
    <<ZDir:(byte_size(RDir) - 5)/binary, "/root">> = RDir,
    CDir = iolist_to_binary([ZDir, "/config"]),
    KDir = iolist_to_binary([ZDir, "/keys"]),
    SDir = iolist_to_binary([<<"/var/zonecontrol/">>, Zone]),
    {ok, await_dirs, #?MODULE{zone = Zone, zdir = ZDir, cdir = CDir,
        kdir = KDir, sdir = SDir}}.

terminate(_Why, _State, #?MODULE{}) ->
    ok.

await_dirs(enter, _PrevState, #?MODULE{zone = Zone}) ->
    lager:debug("waiting for zone ~s to have dirs", [Zone]),
    zka_zones:claim(Zone),
    {keep_state_and_data, [{state_timeout, 200, check}]};
await_dirs(state_timeout, check, S0 = #?MODULE{cdir = CDir, sdir = SDir}) ->
    case {file:read_file_info(CDir), file:read_file_info(SDir)} of
        {{ok, #file_info{type = directory}},
         {ok, #file_info{type = directory}}} ->
            MFile = iolist_to_binary([CDir, "/metadata.json"]),
            case file:read_file_info(MFile) of
                {ok, #file_info{type = regular}} ->
                    {next_state, await_running, S0};
                _ ->
                    {keep_state_and_data, [{state_timeout, 1000, check}]}
            end;
        _ ->
            {keep_state_and_data, [{state_timeout, 1000, check}]}
    end;
await_dirs(info, _, #?MODULE{}) ->
    {keep_state_and_data, [postpone]}.

await_running(enter, _PrevState, #?MODULE{zone = Z}) ->
    lager:debug("waiting for zone ~s to start", [Z]),
    keep_state_and_data;
await_running(info, {state_change, running}, S = #?MODULE{}) ->
    {next_state, generate_keys, S};
await_running(info, {state_change, Other}, #?MODULE{}) ->
    keep_state_and_data;
await_running({call, _}, _, #?MODULE{}) ->
    {keep_state_and_data, [postpone]}.

generate_keys(enter, _PrevState, S = #?MODULE{}) ->
    {keep_state_and_data, [{state_timeout, 100, generate}]};
generate_keys(state_timeout, generate, S0 = #?MODULE{zone = Zone, kdir = KDir}) ->
    _ = file:make_dir(KDir),
    ok = zka_keygen:generate_all(Zone, KDir),
    {next_state, agent_running, S0};
generate_keys(info, _, #?MODULE{}) ->
    {keep_state_and_data, [postpone]}.

agent_running(enter, _PrevState, S = #?MODULE{zone = Zone, sdir = SDir, kdir = KDir}) ->
    SPath = iolist_to_binary([SDir, "/agent.sock"]),
    {ok, Pid} = zka_agents:get_or_start(Zone, SPath, KDir),
    keep_state_and_data;
agent_running(info, {state_change, running}, #?MODULE{}) ->
    keep_state_and_data;
agent_running(info, {state_change, State}, S0 = #?MODULE{zone = Zone}) ->
    lager:debug("zone ~s now in state ~s", [Zone, State]),
    case zka_agents:get(Zone) of
        {ok, Pid} ->
            lager:debug("stopping agent"),
            zka_agent:stop(Pid);
        _ ->
            ok
    end,
    {next_state, await_running, S0}.
