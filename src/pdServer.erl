-module(pdServer).
-behaviour(gen_server).

% API
-export([start_link/0, stop/1,
         add/2, minus/2, getValue/1, raise/1,
         joinCluster/1, makeBallot/2, propose/3, acceptorState/2, learnerInp/2,
         learn/2, invalidateInp/2,
         castPrepare/2, castAccept/2, castCommit/1]).

% module level inter-node calls
-export([prepare/2, accept/2, commit/1, learnerInp/1]).

% utilities
-export([getPdServer/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
         code_change/3]).

% self callbacks
-export([learnRemoteValue/3]).

-record(state, {value=0, acceptorStore, proposerStore, proposerIDStore, learnerStore}).
-include("ballotState.hrl").

% TODO: keep the ets table when pdServer is restarted
% TODO: add recover mechanism

getPdServerAt(Node) ->
    pdUtils:getServerAt(pdServer, Node).


getPdServer() ->
    getPdServerAt(node()).


% module local util functions
clusterNodes() ->
    lists:subtract([node()|nodes()], ?JOKERS).


abcast(FunSpec, Paras) ->
    rpc:eval_everywhere(clusterNodes(),
                        pdServer, FunSpec, Paras).


ballotInitDefault(ID, #state{acceptorStore=AStore, proposerStore=PStore,
                             learnerStore=LStore}) ->
    pdUtils:etsInitDefault(AStore, ID, #acceptorState{ballotid=ID}),
    pdUtils:etsInitDefault(PStore, ID, #proposerState{ballotid=ID}),
    pdUtils:etsInitDefault(LStore, ID, #learnerState{ballotid=ID}).

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
    gen_server:call(Server, {propose, ID, OurProposal}, ?PROPOSETIMEOUT).


acceptorState(Server, ID) ->
    gen_server:call(Server, {acceptorState, ID}).


learnerInp(Server, ID) ->
    gen_server:call(Server, {learnerInp, ID}).


learn(Server, ID) ->
    gen_server:call(Server, {learn, ID}, ?LEARNTIMEOUT).


invalidateInp(Server, ID) ->
    gen_server:call(Server, {invalidateInp, ID}).


castPrepare(From, Msg) ->
    abcast(prepare, [From, Msg]).


castAccept(From, Msg) ->
    abcast(accept, [From, Msg]).


castCommit(Msg) ->
    abcast(commit, [Msg]).


% functions will be casted from other nodes
prepare(From, Msg) ->
    gen_server:call(getPdServer(), {prepare, From, Msg}).


accept(From, Msg) ->
    gen_server:call(getPdServer(), {accept, From, Msg}).


commit(Msg) ->
    gen_server:call(getPdServer(), {commit, Msg}).


learnerInp(ID) ->
    ?MODULE:learnerInp(getPdServer(), ID).


init([]) ->
    AFileName = filename:join([
        code:priv_dir(paxosd), erlang:atom_to_list(node()) ++ "_acceptorStore"]),
    A = case ets:file2tab(AFileName) of
        {ok, TabA} -> TabA;
        {error, _} ->
            ets:new(acceptorStore,
                [set, {keypos, #acceptorState.ballotid}, public])
    end,
    LFileName = filename:join([
        code:priv_dir(paxosd), erlang:atom_to_list(node()) ++ "_learnerStore"]),
    L = case ets:file2tab(LFileName) of
        {ok, TabB} -> TabB;
        {error, _} ->
            ets:new(learnerStore,
                [set, {keypos, #learnerState.ballotid}, public])
    end,
    P = ets:new(proposerStore, [set, {keypos, #proposerState.ballotid}, public]),
    I = ets:new(proposers, [set, {keypos, #proposer.pID}, public]),
    {ok, #state{acceptorStore=A, proposerStore=P, proposerIDStore=I, learnerStore=L}}.


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

handle_call({makeBallot, ID}, _From, State) ->
    ballotInitDefault(ID, State),
    {reply, ok, State};

handle_call({propose, ID, OurProposal}, From,
            State=#state{proposerStore=PStore, proposerIDStore=IStore}) ->
    [ProposerState] = pdUtils:etsLookup(PStore, ID, #proposerState{ballotid=ID}),
    Q1 = ProposerState#proposerState.reqQueue,
    Q2 = queue:in({From, OurProposal}, Q1),
    ets:update_element(PStore, ID, {#proposerState.reqQueue, Q2}),
    case ProposerState of
        #proposerState{ballotid=ID, proposer=undefined} ->
            startProposer(ID, PStore, IStore),
            ok;
        #proposerState{ballotid=ID, proposer=_P} -> ok
    end,
    {noreply, State};

handle_call({learn, ID}, From, State=#state{learnerStore=LStore}) ->
    spawn_link(?MODULE, learnRemoteValue, [From, ID, LStore]),
    {noreply, State};

handle_call({invalidateInp, ID}, _From, State=#state{acceptorStore=AStore,
                                                     learnerStore=LStore}) ->
    ets:update_element(AStore, ID, {#acceptorState.inp, undefined}),
    ets:update_element(LStore, ID, {#learnerState.inp, undefined}),
    {reply, ok, State};

handle_call({acceptorState, ID}, _From, State=#state{acceptorStore=AStore}) ->
    [AccState] = pdUtils:etsLookup(AStore, ID, #acceptorState{ballotid=ID}),
    {reply, AccState, State};

handle_call({learnerInp, ID}, _From, State=#state{learnerStore=LStore}) ->
    [#learnerState{inp=Inp}] = pdUtils:etsLookup(LStore, ID, #learnerState{ballotid=ID}),
    {reply, Inp, State};

handle_call({prepare, ReplyTo,
             _Msg=#prepare{ballotid=BallotID, msgID=MsgID, mbal=MBal}},
            _From, State=#state{acceptorStore=Store})->
            [AccState] = pdUtils:etsLookup(Store, BallotID,
                                         #acceptorState{ballotid=BallotID}),
            case AccState of
                #acceptorState{mbal=OurMBal, bal=Bal, inp=Inp}
                when OurMBal < MBal ->
                    io:format("prepare ok, bigger mbal, inp ~w~n", [Inp]),
                    ets:update_element(Store, BallotID,
                                       {#acceptorState.mbal, MBal}),
                    R = #promise{ballotid=BallotID, msgID=MsgID, bal=Bal,
                                 inp=Inp},
                    ReplyTo!R;
                #acceptorState{mbal=OurMBal} when OurMBal >= MBal ->
                    io:format("prepare fail, less mbal ~w ~w~n", [OurMBal, MBal]),
                    R = #reject{ballotid=BallotID, msgID=MsgID, mbal=OurMBal},
                    ReplyTo!R
            end,
            {reply, ok, State};

handle_call({accept, ReplyTo,
             _Msg=#accept{ballotid=BallotID, msgID=MsgID, mbal=MBal, inp=Inp}},
            _From, State=#state{acceptorStore=Store})->
            [AccState] = pdUtils:etsLookup(Store, BallotID,
                                         #acceptorState{ballotid=BallotID}),
            case AccState of
                #acceptorState{mbal=OurMBal} when OurMBal =< MBal ->
                    io:format("accept ok, bigger mbal, inp ~w~n", [Inp]),
                    ets:update_element(Store, BallotID,
                                       [{#acceptorState.mbal, MBal},
                                        {#acceptorState.bal, MBal},
                                        {#acceptorState.inp, Inp}]),
                    R = #accepted{ballotid=BallotID, msgID=MsgID},
                    ReplyTo!R;
                #acceptorState{mbal=OurMBal} when OurMBal > MBal ->
                    io:format("accept fail, less mbal ~w ~w~n", [OurMBal, MBal]),
                    R = #reject{ballotid=BallotID, msgID=MsgID, mbal=OurMBal},
                    ReplyTo!R
            end,
            {reply, ok, State};

handle_call({commit, _Msg=#commit{ballotid=ID, bal=Bal, inp=Inp}},
            _From, State=#state{acceptorStore=AStore, learnerStore=LStore}) ->
    case ets:update_element(LStore, ID, {#learnerState.inp, Inp}) of
        false -> ets:insert(LStore, #learnerState{ballotid=ID, inp=Inp});
        _ -> ok
    end,
    [OldAcc] = pdUtils:etsLookup(AStore, ID, #acceptorState{ballotid=ID}),
    NewMBal = erlang:max(OldAcc#acceptorState.mbal, Bal),
    NewBal = erlang:max(OldAcc#acceptorState.bal, Bal),
    NewAcc = OldAcc#acceptorState{mbal=NewMBal, bal=NewBal, inp=Inp},
    ets:insert(AStore, NewAcc),
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
            ets:delete(IStore, Pid),
            case ets:lookup(PStore, BallotID) of
                [PState = #proposerState{reqQueue=Q1}] ->
                    {{value, {From, _Proposal}}, Q2} = queue:out(Q1),
                    ets:insert(PStore, PState#proposerState{proposer=undefined,
                                                            reqQueue=Q2}),
                    gen_server:reply(From, ok),
                    startProposer(BallotID, PStore, IStore),
                    ok;
                _ -> ok
            end;
        _ -> ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, #state{acceptorStore=AStore, proposerStore=PStore,
                          proposerIDStore=IStore, learnerStore=LStore}) ->
    lists:foreach(
        fun(Tab) ->
            F = filename:join([
                    code:priv_dir(paxosd),
                    erlang:atom_to_list(node()) ++ "_" ++
                    erlang:atom_to_list(ets:info(Tab, name))]),
            ets:tab2file(Tab, F)
        end, [AStore, LStore]),
    lists:foreach(fun ets:delete/1, [AStore, PStore, IStore, LStore]),
    ok.


code_change(_OldVer, State, _Extra) ->
    {ok, State}.


startProposer(BallotID, PStore, PidStore) ->
    case ets:lookup(PStore, BallotID) of
        [#proposerState{reqQueue=Q1}] ->
            case queue:peek(Q1) of
                {value, {_From, OurProposal}} ->
                    % TODO: set member count in conf file
                    P = pdProposer:proposer(BallotID, PStore,
                                            length(clusterNodes()), OurProposal),
                    ets:update_element(PStore, BallotID,
                                       {#proposerState.proposer, P}),
                    ets:insert(PidStore, #proposer{pID=P, ballotid=BallotID}),
                    pdProposer:fire(P);
                empty -> ok
            end;
        _ -> ok
    end,
    ok.


learnRemoteValue(ReplyTo, BallotID, LStore) ->
    case lists:subtract(clusterNodes(), [node()]) of
        [] -> gen_server:reply(ReplyTo, {error, no_nodes});
        [N|_] ->
            case rpc:call(N, pdServer, learnerInp, [BallotID], ?RPCTIMEOUT) of
                {badrpc, Reason} ->
                    gen_server:reply(ReplyTo, {error, {badrpc, Reason}});
                undefined ->
                    case paxosd:propose(BallotID, undefined) of
                        ok ->
                            case ets:lookup(LStore, BallotID) of
                                [#learnerState{inp=Inp}] ->
                                    gen_server:reply(ReplyTo, {ok, Inp});
                                _ ->
                                    gen_server:reply(ReplyTo, {error, propose_failed})
                            end;
                        R -> gen_server:reply(ReplyTo, {error, R})
                    end;
                Inp ->
                    ets:update_element(LStore, BallotID, {#learnerState.inp, Inp}),
                    gen_server:reply(ReplyTo, {ok, Inp})
            end
    end.
