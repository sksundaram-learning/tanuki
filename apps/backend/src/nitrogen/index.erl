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
-module(index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
    {ok, PrivPath} = application:get_env(backend, priv_path),
    PrivPathStr = filename:join(PrivPath),
    #template { file=PrivPathStr ++ "/priv/templates/bare.html" }.

title() -> "Welcome to Nitrogen".

body() ->
    #container_12 { body=[
        #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body() }
    ]}.

inner_body() ->
    [
        #h1 { text="Welcome to Nitrogen" },
        #p{},
        "
        If you can see this page, then your Nitrogen server is up and
        running. Click the button below to test postbacks.
        ",
        #p{},
        #button { id=button, text="Click me!", postback=click },
		#p{},
        "
        Run <b>./bin/dev help</b> to see some useful developer commands.
        ",
		#p{},
		"
		<b>Want to see the ",#link{text="Sample Nitrogen jQuery Mobile Page",url="/mobile"},"?</b>
		"
    ].

event(click) ->
    wf:replace(button, #panel {
        body="You clicked the button!",
        actions=#effect { effect=highlight }
    }).
