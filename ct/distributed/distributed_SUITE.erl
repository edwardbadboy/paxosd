-module(distributed_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("../../src/ballotState.hrl").
-export([all/0, groups/0, init_per_group/2, end_per_group/2]).
-export([test1/1, test2/1, testLease/1]).


all() -> [test1,test2, {group, testLeaseGroup}].


groups() -> [{testLeaseGroup, [], [{group, testLeaseGroupRun}]},
             {testLeaseGroupRun, [sequence, {repeat, 8}], [testLease]}].


test1(_Config) ->
    1 = 1.


test2(_Config) ->
    application:start(paxosd),
    io:format("paxosd started~n"),
    paxosd:joinCluster(),
    timer:sleep(3000),
    io:format("nodes: ~w~n", [nodes()]),
    application:stop(paxosd),
    io:format("paxosd stopped~n").


init_per_group(testLeaseGroup, Config) ->
    io:format("init group~n"),
    {A1, A2, A3} = os:timestamp(),
    random:seed(A1, A2, A3 + erlang:phash2(self(), 32768)),
    application:start(paxosd),
    io:format("paxosd started~n"),
    paxosd:joinCluster(),
    true = paxosd:waitNodes(all),
    io:format("nodes: ~w~n", [nodes()]),
    Config;

init_per_group(_, Config) ->
    Config.


end_per_group(testLeaseGroup, Config) ->
    io:format("end group~n"),

    io:format("lease wait~n"),
    paxosd:leaseWait(30),
    Done = paxosdLearnDefault(40, 0),
    io:format("done count ~w~n", [Done]),

    io:format("update~n"),
    paxosd:unsafeUpdate(40, Done + 1),

    io:format("lease release~n"),
    paxosd:leaseRelease(30),

    io:format("wait for all nodes to finish~n"),
    R = waitAcc(40, length(pdUtils:clusterNodes()), 1200000),
    io:format("wait nodes result ~w~n", [R]),

    Done1 = paxosd:learn(40),
    io:format("done count ~w~n", [Done1]),
    Acc = paxosd:learn(20),
    io:format("Acc count ~w~n", [Acc]),

    MCount = paxosd:configGet(memberCount),
    % wait for other nodes to learn the final Done value
    timer:sleep(60000),
    application:stop(paxosd),
    Acc = 64,
    Done1 = MCount,

    Config;

end_per_group(_, Config) ->
    Config.


testLease(_Config) ->
    Me = self(),

    io:format("lease wait~n"),
    paxosd:leaseWait(10),
    Whose1 = paxosd:leaseWhose(10),
    io:format("Me ~w lease ~w~n", [Me, Whose1]),
    {Me, _} = Whose1,

    Acc = paxosdLearnDefault(20, 0),
    io:format("Acc count ~w~n", [Acc]),
    io:format("Update~n"),
    paxosd:unsafeUpdate(20, Acc + 1),

    io:format("lease refresh~n"),
    paxosd:leaseRefresh(10),
    Whose2 = paxosd:leaseWhose(10),
    io:format("Me ~w lease ~w~n", [Me, Whose2]),
    {Me, _} = Whose2,

    Acc2 = paxosd:learn(20),
    io:format("Acc count ~w~n", [Acc2]),
    io:format("update~n"),
    paxosd:unsafeUpdate(20, Acc2 + 1),

    io:format("lease release~n"),
    paxosd:leaseRelease(10),

    timer:sleep(random:uniform(?CALMTIME)).


paxosdLearnDefault(ID, Default) ->
    case paxosd:learn(ID) of
        undefined ->
            Default;
        V ->
            V
    end.


waitAcc(_AccID, _AccDone, Timeout) when Timeout =< 0 ->
    false;
waitAcc(AccID, AccDone, Timeout) when Timeout > 0 ->
    try paxosdLearnDefault(AccID, 0) of
        Acc when Acc >= AccDone ->
            true;
        _ ->
            timer:sleep(10000),
            waitAcc(AccID, AccDone, Timeout - 10000)
    catch
        Class:E -> io:format("wait acc exception ~w:~w~n", [Class, E])
    end.
