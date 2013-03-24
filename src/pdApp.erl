-module(pdApp).
-behaviour(application).

% application callbacks
-export([start/2, stop/1]).

start(_Type, _Args) ->
    pdServerSup:start_link().

stop(_State) -> ok.
