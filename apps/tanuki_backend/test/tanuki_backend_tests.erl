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

start() ->
    {ok, Pid} = tanuki_backend_db:start_link(),
    Pid.

stop(Pid) ->
    gen_server:call(Pid, terminate).

fetch_document_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun fetch_document/1}.

% TODO: replace with real test functions for the yet-to-be-written code in tanuki_backend
fetch_document(_Pid) ->
    {Result, Document} = tanuki_backend:fetch_document(foobar),
    [?_assertEqual(document, Result)].
