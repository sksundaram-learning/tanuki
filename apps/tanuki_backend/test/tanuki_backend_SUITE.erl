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
    % set up mnesia
    Priv = ?config(priv_dir, Config),
    ok = application:set_env(mnesia, dir, Priv),
    tanuki_backend_app:ensure_schema([node()]),
    ok = application:start(mnesia),
    % start the application(s)
    ok = application:set_env(cowboy, bind_address, "0.0.0.0"),
    ok = application:set_env(cowboy, port, 8000),
    ok = application:set_env(cowboy, server_name, nitrogen),
    ok = application:set_env(cowboy, document_root, "./priv/static"),
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
    gen_server:call(tanuki_backend_db, terminate),
    Url = ?config(url, Config),
    S = couchbeam:server_connection(Url, []),
    couchbeam:delete_db(S, ?TESTDB),
    couchbeam:stop(),
    application:stop(mnesia),
    ok.

all() ->
    [
        fetch_document,
        all_tags,
        by_checksum,
        by_tag,
        by_tags,
        by_tags_unique,
        by_year,
        by_month,
        date_formatter,
        path_to_mimes,
        generate_etag,
        get_best_date
    ].

fetch_document(_Config) ->
    {Result1, Document1} = tanuki_backend:fetch_document("test_AA"),
    ?assertEqual(document, Result1),
    Content1 = couchbeam_doc:get_value(<<"file_owner">>, Document1),
    ?assertEqual(<<"akwok">>, Content1),
    % negative case, not found
    ?assertEqual({error, not_found}, tanuki_backend:fetch_document("foobar")),
    ok.

all_tags(_Config) ->
    Rows = tanuki_backend:all_tags(),
    ?assertEqual(4, length(Rows)),
    Validate = fun(Row, Key, Value) ->
        ?assertEqual(Key, couchbeam_doc:get_value(<<"key">>, Row)),
        ?assertEqual(Value, couchbeam_doc:get_value(<<"value">>, Row))
    end,
    Keys = [<<"cat">>, <<"cheeseburger">>, <<"dog">>, <<"picnic">>],
    Values = [2, 1, 1, 2],
    Expected = lists:zip3(Rows, Keys, Values),
    [Validate(Row, Key, Value) || {Row, Key, Value} <- Expected],
    ok.

by_checksum(_Config) ->
    Checksum = "39092991d6dde424191780ea7eac2f323accc5686075e3150cbb8fc5da331100",
    Rows = tanuki_backend:by_checksum(Checksum),
    ?assertEqual(1, length(Rows)),
    Validate = fun(Row, Id, Value) ->
        % view has "id" but documents have "_id"? weird
        ?assertEqual(Id, couchbeam_doc:get_value(<<"id">>, Row)),
        ?assertEqual(Value, couchbeam_doc:get_value(<<"value">>, Row))
    end,
    Ids = [<<"test_AA">>],
    Values = [<<"image/jpeg">>],
    [Validate(Row, Id, Value) || {Row, Id, Value} <- lists:zip3(Rows, Ids, Values)],
    % negative case, no matching checksum
    ?assertEqual([], tanuki_backend:by_checksum("foobar")),
    ok.

by_tag(_Config) ->
    Rows = tanuki_backend:by_tag("cat"),
    ?assertEqual(2, length(Rows)),
    Validate = fun(Row, Id) ->
        % view has "id" but documents have "_id"? weird
        ?assertEqual(Id, couchbeam_doc:get_value(<<"id">>, Row))
    end,
    Keys = [<<"test_AA">>, <<"test_AC">>],
    [Validate(Row, Key) || {Row, Key} <- lists:zip(Rows, Keys)],
    % negative case, no such tag
    ?assertEqual([], tanuki_backend:by_tag("foobar")),
    ok.

by_tags(_Config) ->
    Rows = tanuki_backend:by_tags(["cat", "picnic"]),
    ?assertEqual(4, length(Rows)),
    Validate = fun(Row, Id, Key) ->
        % view has "id" but documents have "_id"? weird
        ?assertEqual(Id, couchbeam_doc:get_value(<<"id">>, Row)),
        ?assertEqual(Key, couchbeam_doc:get_value(<<"key">>, Row))
    end,
    Ids = [<<"test_AA">>, <<"test_AC">>, <<"test_AB">>, <<"test_AC">>],
    Keys = [<<"cat">>, <<"cat">>, <<"picnic">>, <<"picnic">>],
    [Validate(Row, Id, Key) || {Row, Id, Key} <- lists:zip3(Rows, Ids, Keys)],
    % negative case, no such tags
    ?assertEqual([], tanuki_backend:by_tags(["foo", "bar"])),
    ok.

by_tags_unique(_Config) ->
    Rows = tanuki_backend:by_tags(["cat", "picnic"], unique),
    ?assertEqual(3, length(Rows)),
    Validate = fun(Row, Id, Key) ->
        % view has "id" but documents have "_id"? weird
        ?assertEqual(Id, couchbeam_doc:get_value(<<"id">>, Row)),
        ?assertEqual(Key, couchbeam_doc:get_value(<<"key">>, Row))
    end,
    Ids = [<<"test_AA">>, <<"test_AB">>, <<"test_AC">>],
    Keys = [<<"cat">>, <<"picnic">>, <<"cat">>],
    [Validate(Row, Id, Key) || {Row, Id, Key} <- lists:zip3(Rows, Ids, Keys)],
    % negative case, no such tags
    ?assertEqual([], tanuki_backend:by_tags(["foo", "bar"], unique)),
    ok.

by_year(_Config) ->
    Validate = fun(Input, Count, ExpectedIds) ->
        Rows = tanuki_backend:by_date(Input),
        ?assertEqual(Count, length(Rows)),
        [?assertEqual(Id, couchbeam_doc:get_value(<<"id">>, Row)) ||
            {Id, Row} <- lists:zip(ExpectedIds, Rows) ]
    end,
    Inputs = [2013, 2014],
    Counts = [1, 2],
    Ids = [[<<"test_AA">>], [<<"test_AC">>, <<"test_AB">>]],
    [Validate(I, C, E) || {I, C, E} <- lists:zip3(Inputs, Counts, Ids)],
    ok.

by_month(_Config) ->
    Validate = fun({Year, Month}, Count, ExpectedIds) ->
        Rows = tanuki_backend:by_date(Year, Month),
        ?assertEqual(Count, length(Rows)),
        [?assertEqual(Id, couchbeam_doc:get_value(<<"id">>, Row)) ||
            {Id, Row} <- lists:zip(ExpectedIds, Rows) ]
    end,
    Inputs = [{2014, 7}, {2014, 10}],
    Counts = [1, 1],
    Ids = [[<<"test_AC">>], [<<"test_AB">>]],
    [Validate(I, C, E) || {I, C, E} <- lists:zip3(Inputs, Counts, Ids)],
    ok.

date_formatter(_Config) ->
    Result1 = tanuki_backend:date_list_to_string([2014, 12, 23, 22, 28]),
    ?assertEqual("2014/12/23 22:28", Result1),
    Result2 = tanuki_backend:date_list_to_string([2014, 12, 23, 22, 28], date_only),
    ?assertEqual("2014/12/23", Result2),
    ok.

path_to_mimes(_Config) ->
    % success case leading path
    Filepath1 = "foo/39/09/2991d6dde424191780ea7eac2f323accc5686075e3150cbb8fc5da331100",
    Result1 = tanuki_backend:path_to_mimes(Filepath1, a113),
    ?assertEqual(1, length(Result1)),
    ?assertEqual(<<"image/jpeg">>, hd(Result1)),
    % success case exact path
    Filepath2 = "39/09/2991d6dde424191780ea7eac2f323accc5686075e3150cbb8fc5da331100",
    Result2 = tanuki_backend:path_to_mimes(Filepath2, a113),
    ?assertEqual(1, length(Result2)),
    ?assertEqual(<<"image/jpeg">>, hd(Result2)),
    % success case binary path
    Filepath3 = <<"39/09/2991d6dde424191780ea7eac2f323accc5686075e3150cbb8fc5da331100">>,
    Result3 = tanuki_backend:path_to_mimes(Filepath3, a113),
    ?assertEqual(1, length(Result3)),
    ?assertEqual(<<"image/jpeg">>, hd(Result3)),
    % unknown checksum case
    Filepath4 = "foo/11/22/34567890abcdef",
    Result4 = tanuki_backend:path_to_mimes(Filepath4, a113),
    ?assertEqual(1, length(Result4)),
    ?assertEqual(<<"application/octet-stream">>, hd(Result4)),
    % fail case no slashes
    ?assertError(function_clause, tanuki_backend:path_to_mimes("abcdef", a113)),
    % fail case short path
    ?assertError(function_clause, tanuki_backend:path_to_mimes("abc/def", a113)),
    ok.

generate_etag(_Config) ->
    % success case leading path
    Input1 = [{filepath, "foo/39/09/2991d6dde424191780ea7eac2f323accc5686075e3150cbb8fc5da331100"}],
    {strong, Result1} = tanuki_backend:generate_etag(Input1, strong_etag_extra),
    ?assertEqual(<<"39092991d6dde424191780ea7eac2f323accc5686075e3150cbb8fc5da331100">>, Result1),
    % success case exact path
    Input2 = [{filepath, "39/09/2991d6dde424191780ea7eac2f323accc5686075e3150cbb8fc5da331100"}],
    {strong, Result2} = tanuki_backend:generate_etag(Input2, strong_etag_extra),
    ?assertEqual(<<"39092991d6dde424191780ea7eac2f323accc5686075e3150cbb8fc5da331100">>, Result2),
    % success case binary path
    Input3 = [{filepath, <<"39/09/2991d6dde424191780ea7eac2f323accc5686075e3150cbb8fc5da331100">>}],
    {strong, Result3} = tanuki_backend:generate_etag(Input3, strong_etag_extra),
    ?assertEqual(<<"39092991d6dde424191780ea7eac2f323accc5686075e3150cbb8fc5da331100">>, Result3),
    % fail case no slashes
    ?assertError(function_clause, tanuki_backend:generate_etag([{filepath, "abcdef"}], a113)),
    % fail case short path
    ?assertError(function_clause, tanuki_backend:generate_etag([{filepath, "abc/def"}], a113)),
    ok.

get_best_date(_Config) ->
    {document, Document1} = tanuki_backend:fetch_document("test_AA"),
    ?assertEqual([2013, 1, 31, 5, 26], tanuki_backend:get_best_date(Document1)),
    {document, Document2} = tanuki_backend:fetch_document("test_AB"),
    ?assertEqual([2014, 10, 24, 15, 9], tanuki_backend:get_best_date(Document2)),
    {document, Document3} = tanuki_backend:fetch_document("test_AC"),
    ?assertEqual([2014, 7, 15, 3, 13], tanuki_backend:get_best_date(Document3)),
    ok.
