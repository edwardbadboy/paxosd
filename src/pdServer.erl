-module(pdServer).
-behaviour(gen_server).

% API
-export([start_link/0, stop/1,
         add/2, minus/2, getValue/1, raise/1,
         joinCluster/1, makeBallot/2, propose/3, ballotInp/2,
         castPrepare/2, castAccept/2]).

% module level inter-node calls
-export([prepare/2, accept/2]).

% utilities
-export([getPdServer/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
         code_change/3]).

-record(state, {value=0, acceptorStore, proposerStore, proposerIDStore}).
-include("ballotState.hrl").

-define(JOINTIMEOUT, 20000).

% TODO: keep the ets table when pdServer is restarted
% TODO: add recover mechanism

% module local util functions
getPdServerAt(Node) ->
    [{pdServer, P, _, _}] = supervisor:which_children({pdServerSup, Node}),
    P.

getPdServer() ->
    getPdServerAt(node()).

clusterNodes() ->
    lists:subtract([node()|nodes()], ?JOKERS).

abcast(FunSpec, Paras) ->
    rpc:eval_everywhere(clusterNodes(),
                        pdServer, FunSpec, Paras).

% API functions
start_link() ->
    gen_server:start_link(?MODULE, [], []).

stop(Server) ->
    gen_server:cast(Server, stop).

add(Server, Value) ->
    gen_server:call(Server, {add, Value}).

minus(Server, Value) ->
    gen_server:call(Server, {minus, Value}).

getValue(Server) ->
    gen_server:call(Server, {getValue}).

raise(Server) ->
    gen_server:call(Server, {raise}).

joinCluster(Server) ->
    gen_server:call(Server, {joinCluster}, ?JOINTIMEOUT).

makeBallot(Server, ID) ->
    gen_server:call(Server, {makeBallot, ID}).

propose(Server, ID, OurProposal) ->
    gen_server:call(Server, {propose, ID, OurProposal}).

ballotInp(Server, ID) ->
    gen_server:call(Server, {ballotInp, ID}).

castPrepare(From, Msg) ->
    abcast(prepare, [From, Msg]).

castAccept(From, Msg) ->
    abcast(accept, [From, Msg]).

% functions will be casted from other nodes
prepare(From, Msg) ->
    gen_server:call(getPdServer(), {prepare, From, Msg}).

accept(From, Msg) ->
    gen_server:call(getPdServer(), {accept, From, Msg}).


init([]) ->
    A = ets:new(acceptorStore, [set, {keypos, #acceptorState.ballotid}, public]),
    P = ets:new(proposerStore, [set, {keypos, #proposerState.ballotid}, public]),
    I = ets:new(proposers, [set, {keypos, #proposer.pID}, public]),
    {ok, #state{acceptorStore=A, proposerStore=P, proposerIDStore=I}}.

handle_call({getValue}, _From, State) ->
    {reply, State#state.value, State};
handle_call({add, Value}, _From, State) ->
    OldValue = State#state.value,
    {reply, ok, State#state{value = OldValue + Value}};
handle_call({minus, Value}, _From, State) ->
    OldValue = State#state.value,
    {reply, ok, State#state{value = OldValue - Value}};
handle_call({raise}, _From, State) ->
    1 = 2,
    {reply, ok, State};
handle_call({joinCluster}, _From, State) ->
    case lists:any(fun(Node) ->
                     net_adm:ping(Node) =:= pong
                   end, ?JOKERS) of
      false -> {reply, error, State};
      true -> {reply, ok, State}
    end;
handle_call({makeBallot, ID}, _From,
            State=#state{acceptorStore=AStore, proposerStore=PStore}) ->
    case ets:lookup(AStore, ID) of
        [] ->
            ets:insert(AStore, #acceptorState{ballotid=ID});
        [#acceptorState{ballotid=ID}] -> ok
    end,
    case ets:lookup(PStore, ID) of
        [] ->
            ets:insert(PStore, #proposerState{ballotid=ID});
        [#proposerState{ballotid=ID}] -> ok
    end,
    {reply, ok, State};
handle_call({propose, ID, OurProposal}, _From,
            State=#state{proposerStore=PStore, proposerIDStore=IStore}) ->
    case ets:lookup(PStore, ID) of
        [#proposerState{ballotid=ID, proposer=undefined}] ->
            P = pdProposer:proposer(ID, PStore, length(clusterNodes()), OurProposal),
            ets:update_element(PStore, ID, {#proposerState.proposer, P}),
            ets:insert(IStore, #proposer{pID=P, ballotid=ID}),
            pdProposer:fire(P),
            P;
        [#proposerState{ballotid=ID, proposer=P}] -> P
    end,
    {reply, ok, State};
handle_call({ballotInp, ID}, _From, State=#state{acceptorStore=AStore}) ->
    Ballot = case ets:lookup(AStore, ID) of
        [R] -> R;
        [] -> undefined
    end,
    {reply, Ballot, State};
handle_call({prepare, ReplyTo, _Msg=#prepare{ballotid=BallotID, msgID=MsgID,
                                             mbal=MBal}},
            _From, State=#state{acceptorStore=Store})->
            case ets:lookup(Store, BallotID) of
                [] ->
                    io:format("prepare ok, empty ballot~n"),
                    ets:insert(Store,
                               #acceptorState{ballotid=BallotID, mbal=MBal,
                                              bal=#balNum{}, inp=undefined}),
                    R = #promise{ballotid=BallotID, msgID=MsgID, bal=#balNum{},
                                 inp=undefined},
                    ReplyTo!R;
                [#acceptorState{mbal=OurMBal, bal=Bal, inp=Inp}]
                when OurMBal < MBal ->
                    io:format("prepare ok, bigger mbal, inp ~w~n", [Inp]),
                    ets:update_element(Store, BallotID, {#acceptorState.mbal, MBal}),
                    R = #promise{ballotid=BallotID, msgID=MsgID, bal=Bal,
                                 inp=Inp},
                    ReplyTo!R;
                [#acceptorState{mbal=OurMBal}] when OurMBal >= MBal ->
                    io:format("prepare fail, less mbal ~w ~w~n", [OurMBal, MBal]),
                    % TODO: send reject message
                    ok
            end,
            {reply, ok, State};
handle_call({accept, ReplyTo, _Msg=#accept{ballotid=BallotID, msgID=MsgID,
                                           mbal=MBal, inp=Inp}},
            _From, State=#state{acceptorStore=Store})->
            io:format("accept called from ~w~n", [ReplyTo]),
            case ets:lookup(Store, BallotID) of
                [] ->
                    io:format("accept ok, empty ballot inp ~w~n", [Inp]),
                    ets:insert(Store,
                               #acceptorState{ballotid=BallotID, mbal=MBal,
                                              bal=MBal, inp=Inp}),
                    R = #accepted{ballotid=BallotID, msgID=MsgID},
                    ReplyTo!R;
                [#acceptorState{mbal=OurMBal}] when OurMBal =< MBal ->
                    io:format("prepare ok, bigger mbal, inp ~w~n", [Inp]),
                    ets:update_element(Store, BallotID,
                                       [{#acceptorState.mbal, MBal},
                                        {#acceptorState.bal, MBal},
                                        {#acceptorState.inp, Inp}]),
                    R = #accepted{ballotid=BallotID, msgID=MsgID},
                    ReplyTo!R;
                [#acceptorState{mbal=OurMBal}] when OurMBal > MBal ->
                    io:format("accept fail, less mbal ~w ~w~n", [OurMBal, MBal]),
                    % TODO: send reject message
                    ok
            end,
            {reply, ok, State}.

handle_cast(stop, State=#state{proposerIDStore=Store}) ->
    ets:foldl(
        fun(#proposer{pID=PID}, ok) ->
            exit(PID, kill), ok
        end,
        ok, Store),
    {stop, normal, State};
handle_cast({msg, From, Msg}, State) ->
    io:format("Get message from ~p: ~p~n", [From, Msg]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _Reason}, State=#state{proposerStore=PStore,
                                                 proposerIDStore=IStore}) ->
    case ets:lookup(IStore, Pid) of
        [#proposer{ballotid=BallotID}] ->
            ets:update_element(PStore, BallotID, {#proposerState.proposer, undefined}),
            ets:delete(IStore, Pid);
        _ -> ok
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{acceptorStore=AStore, proposerStore=PStore,
                                                proposerIDStore=IStore}) ->
    lists:foreach(fun ets:delete/1, [AStore, PStore, IStore]),
    ok.

code_change(_OldVer, State, _Extra) ->
    {ok, State}.
