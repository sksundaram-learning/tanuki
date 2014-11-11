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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%% gen_server callbacks
%%
init([]) ->
    {ok, Url} = application:get_env(tanuki_backend, couchdb_url),
    Server = couchbeam:server_connection(Url, []),
    {ok, DbName} = application:get_env(tanuki_backend, database),
    {ok, Db} = couchbeam:open_or_create_db(Server, DbName, []),
    install_designs(Db),
    State = #state{server=Server, database=Db},
    {ok, State}.

% TODO: add a call to get all documents within specific periods of time (e.g. year, month)
% TODO: add a call to get all documents with specific tags
handle_call({fetch_document, DocId}, _From, #state{database=Db}=State) ->
    {ok, Doc} = couchbeam:open_doc(Db, DocId),
    {reply, {document, Doc}, State};
handle_call(all_tags, _From, #state{database=Db}=State) ->
    DesignName = "tanuki",
    ViewName = "tags",
    Options = [{group_level, 1}],
    {ok, Rows} = couchbeam_view:fetch(Db, {DesignName, ViewName}, Options),
    {reply, Rows, State};
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    error_logger:info_msg("unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% internal functions
%%

install_designs(Db) ->
    {ok, PrivPath} = application:get_env(tanuki_backend, priv_path),
    ViewsDir = filename:join(PrivPath ++ ["views"]),
    InsertDocument = fun(Filename) ->
        Filepath = filename:join([ViewsDir, Filename]),
        {ok, Binary} = file:read_file(Filepath),
        Json = couchbeam_ejson:decode(Binary),
        % TODO: seems like couchbeam:doc_exists/2 is broken (see issue #116)
        % DocId = bitstring_to_list(couchbeam_doc:get_id(Json)),
        % case couchbeam:doc_exists(Db, DocId) of
        %     true -> ok;
        %     false -> {ok, _Doc1} = couchbeam:save_doc(Db, Json)
        % end
        {ok, _Doc1} = couchbeam:save_doc(Db, Json)
    end,
    ViewPath = filename:absname(ViewsDir),
    {ok, Filenames} = file:list_dir(ViewPath),
    [InsertDocument(Filename) || Filename <- Filenames],
    ok.
