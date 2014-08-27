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
-module(mobile).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).
-author("Jesse Gumm (gumm@sigma-star.com)").

main() -> #template{file="./priv/templates/mobile.html"}.

title() -> "Nitrogen Web Framework - Mobile Sample".

body() ->
    [
        "If you can see this, then your Nitrogen installation is working.",
        #p{},
        "Go ahead and enable the sample menu below to test postbacks and links",
        #p{},
        #mobile_toggle{
            on_text="Menu Visible",
            off_text="Menu Hidden",
            selected="off",
            postback=toggle_menu,
            id=menu_on,
            width=200
        },
        #p{},
        #mobile_list{
            id=menu,
            theme=a,
            inset=true,
            style="display:none",
            body=[
                #mobile_list_divider{class=c, text="Sample Mobile Menu"},
                mobile_list_link("Non-mobile Sample Page","/"),
                mobile_list_link("Nitrogen Home","http://nitrogenproject.com"),
                mobile_list_link("jQuery Mobile Home","http://jquerymobile.com"),
                mobile_list_link("Erlang Home","http://erlang.org")
            ]
        }
    ].

mobile_list_link(Text,URL) ->
    #mobile_listitem{
        theme=c,
        body=#link{text=Text, url=URL, mobile_target=false}
    }.

event(toggle_menu) ->
    ShowMenu = wf:q(menu_on),
    case ShowMenu of
        "on" -> wf:wire(menu,#appear{});
        "off" -> wf:wire(menu,#fade{})
    end.
