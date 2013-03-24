-module(paxosd).

% API
-export([add/1, minus/1, getValue/0, raise/0, joinCluster/0, makeBallot/1,
         propose/2, ballotInp/1]).

% API implementation
add(Value) ->
    pdServer:add(pdServer:getPdServer(), Value).

minus(Value) ->
    pdServer:minus(pdServer:getPdServer(), Value).

getValue() ->
    pdServer:getValue(pdServer:getPdServer()).

raise() ->
    pdServer:raise(pdServer:getPdServer()).

joinCluster() ->
    pdServer:joinCluster(pdServer:getPdServer()).

makeBallot(ID) ->
    pdServer:makeBallot(pdServer:getPdServer(), ID).

propose(ID, Proposal) ->
    pdServer:propose(pdServer:getPdServer(), ID, Proposal).

ballotInp(ID) ->
    pdServer:ballotInp(pdServer:getPdServer(), ID).
