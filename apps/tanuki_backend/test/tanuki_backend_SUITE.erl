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
-module(tanuki_backend_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TESTDB, "tanuki_test").

init_per_suite(Config) ->
    % load the application so we can read and modify the environment
    ok = application:load(tanuki_backend),
    ok = application:set_env(tanuki_backend, database, ?TESTDB),
    ok = couchbeam:start(),
    {ok, Url} = application:get_env(tanuki_backend, couchdb_url),
    S = couchbeam:server_connection(Url, []),
    % clean up any mess from a previously failed test
    {ok, _Wat} = case couchbeam:db_exists(S, ?TESTDB) of
        true  -> couchbeam:delete_db(S, ?TESTDB);
        false -> {ok, foo}
    end,
    {ok, Db} = couchbeam:create_db(S, ?TESTDB, []),
    add_test_docs(Db, Config),
    % start the application(s)
    ok = application:set_env(cowboy, bind_address, "0.0.0.0"),
    ok = application:set_env(cowboy, port, 8000),
    ok = application:set_env(cowboy, server_name, nitrogen),
    ok = application:set_env(cowboy, document_root, "./priv/static"),
        {},
    ok = application:set_env(cowboy, static_paths,
        ["/js/", "/images/", "/css/", "/nitrogen/", "/favicon.ico"]),
    {ok, _Started} = application:ensure_all_started(tanuki_backend),
    [{url, Url} | Config].

add_test_docs(Db, Config) ->
    % populate test database from json files in data_dir
    DataDir = ?config(data_dir, Config),
    InsertDocument = fun(Filename) ->
        Filepath = filename:join([DataDir, Filename]),
        {ok, Binary} = file:read_file(Filepath),
        Json = couchbeam_ejson:decode(Binary),
        {ok, _Doc1} = couchbeam:save_doc(Db, Json)
    end,
    {ok, Filenames} = file:list_dir(DataDir),
    [InsertDocument(Filename) || Filename <- Filenames],
    ok.

end_per_suite(Config) ->
    Url = ?config(url, Config),
    S = couchbeam:server_connection(Url, []),
    couchbeam:delete_db(S, ?TESTDB),
    couchbeam:stop(),
    gen_server:call(tanuki_backend_db, terminate).

all() ->
    % TODO: add a bunch more tests
    [fetch_document].

fetch_document(_Config) ->
    {Result, Document} = tanuki_backend:fetch_document("test"),
    ?assertEqual(document, Result),
    % retrieve the 'basic' document
    Content = couchbeam_doc:get_value(<<"content">>, Document),
    ?assertEqual(<<"some text">>, Content),
    ok.
