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
-module(tanuki_backend_db).
-behavior(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {server, database}).

%%
%% Client API
%%
start_link() ->
    gen_server:start_link({local, tanuki_backend_db}, ?MODULE, [], []).

%%
%% gen_server callbacks
%%
init([]) ->
    {ok, Url} = application:get_env(tanuki_backend, couchdb_url),
    Server = couchbeam:server_connection(Url, []),
    {ok, DbName} = application:get_env(tanuki_backend, database),
    {ok, Db} = couchbeam:open_or_create_db(Server, DbName, []),
    State = #state{server=Server, database=Db},
    {ok, State}.

handle_call({fetch_document, DocId}, _From, S = #state{}) ->
    {ok, Doc} = couchbeam:open_doc(S#state.database, DocId),
    {reply, {document, Doc}, S};
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
