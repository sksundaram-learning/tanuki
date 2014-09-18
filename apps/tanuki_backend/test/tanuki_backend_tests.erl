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
-module(tanuki_backend_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-define(TESTDB, "tanuki_test").

% TODO: consider using Common Test to prep database with test data
% TODO: consider a Common Test suite to perform a series of tests with the same data

start() ->
    % send logging to a file to keep the test output clean
    ok = error_logger:logfile({open, "eunit.log"}),
    ok = error_logger:tty(false),
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
    Doc = {[
        {<<"_id">>, <<"test">>},
        {<<"content">>, <<"some text">>}
    ]},
    {ok, _Doc1} = couchbeam:save_doc(Db, Doc),
    {ok, Pid} = tanuki_backend_db:start_link(),
    Pid.

stop(Pid) ->
    {ok, Url} = application:get_env(tanuki_backend, couchdb_url),
    S = couchbeam:server_connection(Url, []),
    couchbeam:delete_db(S, ?TESTDB),
    couchbeam:stop(),
    gen_server:call(Pid, terminate).

fetch_document_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun fetch_document/1}.

fetch_document(_Pid) ->
    {Result, Document} = tanuki_backend:fetch_document("test"),
    Content = couchbeam_doc:get_value(<<"content">>, Document),
    [?_assertEqual(document, Result),
     ?_assertEqual(<<"some text">>, Content)].
