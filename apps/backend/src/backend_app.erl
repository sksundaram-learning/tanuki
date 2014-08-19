-module(backend_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    nitrogen_sup:start_link(),
    backend_sup:start_link().

stop(_) ->
    ok.
