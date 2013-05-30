-module(pdApp).
-behaviour(application).

% application callbacks
-export([start/2, stop/1]).


start(_Type, Args) ->
    pdServerSup:start_link(Args).


stop(_State) -> ok.
