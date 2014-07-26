-module(incoming_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    incoming_sup:start_link().

stop(_) ->
    ok.
