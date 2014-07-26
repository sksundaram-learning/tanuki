-module(backend_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    backend_sup:start_link().

stop(_) ->
    ok.
