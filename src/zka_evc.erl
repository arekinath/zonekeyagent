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

-module(zka_evc).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

-export([
    start_link/0,
    refresh/0
    ]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_info/2,
    handle_cast/2
    ]).

-record(?MODULE, {
    hdl :: reference(),
    msgref :: reference()
    }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

refresh() ->
    ?MODULE ! refresh_zones.

%% @private
init([]) ->
    {ok, Hdl, MsgRef} = esysevent:evc_subscribe("com.sun:zones:status",
        [], "zonekeyagent", all),
    self() ! refresh_zones,
    {ok, #?MODULE{hdl = Hdl, msgref = MsgRef}}.

%% @private
terminate(_Why, #?MODULE{}) ->
    ok.

%% @private
handle_call(What, From, S = #?MODULE{}) ->
    gen_server:reply(From, {error, bad_cast}),
    {stop, {bad_call, From, What}, S}.

%% @private
handle_cast(Msg, S = #?MODULE{}) ->
    {stop, {bad_cast, Msg}, S}.

%% @private
handle_info({sysevent, MsgRef, Info, Attrs}, S = #?MODULE{msgref = MsgRef}) ->
    case Info of
        #{class := <<"status">>, subclass := <<"change">>} ->
            #{<<"zonename">> := {string, Zone},
              <<"newstate">> := {string, StateBin}} = Attrs,
            State = binary_to_atom(StateBin, utf8),
            lager:debug("zone ~s going to state ~s", [Zone, State]),
            case zka_zones:get_or_start(Zone) of
                {ok, Pid} ->
                    zka_zone_fsm:state_change(Pid, State);
                Err ->
                    lager:error("error getting zone fsm for ~s: ~p",
                        [Zone, Err])
            end,
            {noreply, S};
        _ ->
            lager:debug("ignored sysevent: ~p", [Info]),
            {noreply, S}
    end;

handle_info(refresh_zones, S = #?MODULE{}) ->
    {ok, Zids} = zka_zoneinfo:list(),
    lists:foreach(fun (Zid) ->
        {ok, Info} = zka_zoneinfo:get(Zid),
        case Info of
            #{name := <<"global">>} -> ok;
            #{name := ZoneName, status := State} ->
                {ok, Pid} = zka_zones:get_or_start(ZoneName),
                zka_zone_fsm:state_change(Pid, State)
        end
    end, Zids),
    {noreply, S}.
