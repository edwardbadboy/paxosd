-module(pdUtils).
-export([nfoldl/3, nfoldlTest/0, incBalNum/1, incBalNumTo/1,
         etsInitDefault/3, etsLookup/3]).
-include("ballotState.hrl").


nfoldl(_Funs, Acc, []) -> Acc;

nfoldl(Funs, Acc0, [H|T]) ->
    Acc1 = lists:zipwith(fun(Fun, X) -> Fun(H, X) end, Funs, Acc0),
    nfoldl(Funs, Acc1, T).


nfoldlTest() ->
    F1 = fun(X, Sum) -> X + Sum end,
    F2 = fun(X, Product) -> X * Product end,
    [15, 120] = nfoldl([F1, F2], [0, 1], [1, 2, 3, 4, 5]).


incBalNum(B=#balNum{n=N}) ->
    B#balNum{n = N + 1}.


incBalNumTo(B) ->
    B#balNum{nodeName = node()}.


etsInitDefault(Table, Key, DefaultV) ->
    case ets:lookup(Table, Key) of
        [] ->
            ets:insert(Table, DefaultV),
            [DefaultV];
        V = [_|_] -> V
    end.


etsLookup(Table, Key, DefaultV) ->
    case ets:lookup(Table, Key) of
        [] -> ets:insert(Table, DefaultV), [DefaultV];
        R = [_|_] -> R
    end.