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
-module(nitrogen_cowboy).
-include_lib("nitrogen_core/include/wf.hrl").

-export([init/3, handle/2, terminate/3]).

-record(state, {headers, body}).

init(_Transport, Req, Opts) ->
    Headers = proplists:get_value(headers, Opts, []),

    Body = proplists:get_value(body, Opts, "http_handler"),
    {ok, Req, #state{headers=Headers, body=Body}}.


handle(Req,_Opts) ->
    {ok, DocRoot} = application:get_env(cowboy, document_root),
    RequestBridge = simple_bridge:make_request(cowboy_request_bridge,
                                               {Req, DocRoot}),

    %% Because Cowboy uses the same "Req" record, we can pass the
    %% previously made RequestBridge to make_response, and it'll
    %% parse out the relevant bits to keep both parts (request and
    %% response) using the same "Req"
    ResponseBridge = simple_bridge:make_response(cowboy_response_bridge,
                                                 RequestBridge),

    %% Establishes the context with the Request and Response Bridges
    nitrogen:init_request(RequestBridge, ResponseBridge),

    {ok, NewReq} = nitrogen:run(),

    %% This will be returned back to cowboy
    {ok, NewReq, _Opts}.

terminate(_Reason, _Req, _State) ->
    ok.
