%% -*- coding: utf-8 -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Nathan Fiedler
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%%
%% This is all just a placeholder for the time being.
%% The eventual application will need to wake up from time to time
%% and check for new assets in the 'incoming' directory, and process
%% them as the scripts/incoming/incoming.py is doing.
%%

-module(tanuki_incoming_fsm).
-behaviour(gen_fsm).

-record(state, {fake=0,
                pid}).

-export([start_link/0]).
-export([init/1, terminate/3, code_change/4, % setup/teardown/upgrade
         handle_event/3, handle_sync_event/4, handle_info/3, % global events
         %% only async events
         phony/2]).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

%%
%% gen_fsm callbacks
%%
init([]) ->
    {ok, phony, #state{}}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.

handle_info(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%
%% async events
%%
phony(_, State) ->
    {next_state, phony, State}.
