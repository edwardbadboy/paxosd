-module(pdProposer).

% API
-export([getPdProposalHandlerAt/1, getPdProposalHandler/0, propose/3, propose/4]).

% pdReqRouter callbacks
-export([proposerWorker/0, doWork/2, reqHash/1, workerStateInit/0,
         workerStateClean/1]).

% callback for module itself.
-export([sessionStart/1, determineInp/2]).

-include("ballotState.hrl").
-include("pdReqRouter.hrl").

-record(session, {ballotid, proposerStore, proposal, memberCount=0, override,
                  proposeTimeoutRef}).


% API
getPdProposalHandlerAt(Node) ->
    pdUtils:getServerAt(pdProposalHandler, Node).


getPdProposalHandler() ->
    getPdProposalHandlerAt(node()).


propose(Server, ID, OurProposal) ->
    propose(Server, ID, OurProposal, #proposerOverride{}).


propose(Server, ID, OurProposal, undefined) ->
    propose(Server, ID, OurProposal, #proposerOverride{});

propose(Server, ID, OurProposal, Override) ->
    case pdReqRouter:request(Server, {ID, OurProposal, Override}, ?PROPOSETIMEOUT) of
        {ok, R} -> R;
        Error -> Error
    end.


% pdReqRouter callbacks
proposerWorker() ->
    #workerCallback{
        doWork=fun ?MODULE:doWork/2, reqHash=fun ?MODULE:reqHash/1,
        workerStateInit=fun ?MODULE:workerStateInit/0,
        workerStateClean=fun ?MODULE:workerStateClean/1}.


doWork({ID, OurProposal, Override}, Store) ->
    sessionStart(
        #session{ballotid=ID, proposerStore=Store, proposal=OurProposal,
                 override=Override,
                 memberCount=length(pdUtils:clusterNodes())}),
    pdReqRouter:workerReturn(Store, ok).


reqHash({ID, _OurProposal, _Override}) -> ID.


workerStateInit() ->
    ets:new(proposerStore,
        [set, {keypos, #proposerState.ballotid}, public]).


workerStateClean(Store) ->
    ets:delete(Store).


% proposer logic
sessionStart(Session=#session{ballotid=ID, proposerStore=Store}) ->
    {A1, A2, A3} = os:timestamp(),
    random:seed(A1, A2, A3),
    TRef = pdUtils:timeout(?PROPOSETIMEOUT),
    pdUtils:etsInitDefault(Store, ID, #proposerState{ballotid=ID}),
    sessionLoop(Session#session{proposeTimeoutRef=TRef}).


calmDown() ->
    timer:sleep(erlang:round(?CALMTIME * random:uniform())),
    ok.


nextRound(Session=#session{proposeTimeoutRef=TRef}) ->
    case pdUtils:isTimeout(TRef) of
        true ->
            ok;
        _ ->
            calmDown(),
            sessionLoop(Session)
    end.


sessionLoop(Session=#session{ballotid=BallotID, proposerStore=Store,
                             proposal=OurProposal, override=Override}) ->
    [ProposerState] = ets:lookup(Store, BallotID),
    MBal =  ProposerState#proposerState.mbal,
    ets:update_element(Store, BallotID, {#proposerState.mbal, pdUtils:incBalNum(MBal)}),
    case runStage(Session, prepare) of
        {ok, Responses} ->
            io:format("prepare ok ~w~n", [Responses]),
            DetermineInp = getOverride(Override, determineInp),
            Inp = DetermineInp(OurProposal, Responses),
            io:format("Inp ~w~n", [Inp]),
            ets:update_element(Store, BallotID, {#proposerState.inp, Inp}),
            case runStage(Session, accept) of
                {ok, _R} ->
                    io:format("accept ok~n"),
                    ok;
                _ ->
                    io:format("accept fail~n"),
                    nextRound(Session)
            end;
        _ ->
            io:format("prepare fail~n"),
            nextRound(Session)
    end.


getOverride(#proposerOverride{determineInp=F}, determineInp) -> F.


determineInp(OurProposal, Responses) ->
    FindInp =
        fun(#promise{inp=undefined}, Acc) ->
                Acc;
            (R, _Acc=undefined) when is_record(R, promise) ->
               R;
           (R=#promise{bal=Bal},
            Acc=#promise{bal=BalAcc}) ->
               if
                   Bal > BalAcc -> R;
                   true -> Acc
               end
        end,
    case lists:foldl(FindInp, undefined, Responses) of
        undefined -> OurProposal;
        #promise{inp=Inp} -> Inp
    end.


runStage(_Session=#session{ballotid=BallotID,
                           proposerStore=Store,
         memberCount=M}, Stage) ->
    MsgID = make_ref(),
    [ProposerState] = ets:lookup(Store, BallotID),
    MBal = ProposerState#proposerState.mbal,
    Inp = ProposerState#proposerState.inp,
    pdUtils:flushMsg(),
    case Stage of
        prepare -> pdServer:castPrepare(self(),
                                  #prepare{ballotid=BallotID,
                                           msgID=MsgID, mbal=MBal});
        accept ->
            pdServer:castAccept(self(),
                                #accept{ballotid=BallotID,
                                        msgID=MsgID, mbal=MBal, inp=Inp})
    end,
    R = collectResp(Stage, ?COLLECTTIMEOUT, M, MsgID),
    case {R, Stage} of
        {{error, {rejected, HigherMBal}}, _} ->
            NewMBal = pdUtils:incBalNumTo(HigherMBal),
            ets:update_element(Store, BallotID, {#proposerState.mbal, NewMBal});
        {{ok, _}, accept} ->
            pdServer:castCommit(#commit{ballotid=BallotID, bal=MBal,
                                        inp=Inp});
        {_, _} -> ok
    end,
    R.


collectResp(Stage, Timeout, MemberCount, MsgID) ->
    TRef = make_ref(),
    timer:send_after(Timeout, {collectTimeout, TRef}),
    collectRespAcc(Stage, MemberCount, MsgID, TRef, []).


collectRespAcc(Stage, MemberCount, MsgID, TimerRef, Response) ->
    receive
        R when Stage == prepare, is_record(R, promise),
                   R#promise.msgID == MsgID;
               Stage == accept, is_record(R, accepted),
                   R#accepted.msgID == MsgID ->
            io:format("get response ~w~n", [R]),
            Rs = [R|Response],
            case isMajority(Rs, MemberCount) of
                true ->
                    io:format("majority ok~n"),
                    {ok, Rs};
                _ ->
                    collectRespAcc(Stage, MemberCount, MsgID, TimerRef, Rs)
            end;
        #reject{msgID=MsgID, mbal=MBal} ->
            io:format("Rejectd by accepter mbal ~w~n", [MBal]),
            {error, {rejected, MBal}};
        {collectTimeout, TimerRef} ->
            {error, timeout};
        _Msg ->
            io:format("unknown msg ~w~n", [_Msg]),
            collectRespAcc(Stage, MemberCount, MsgID, TimerRef, Response)
    after ?COLLECTTIMEOUT ->
        {error, timeout}
    end.


isMajority(R, MemberCount) -> length(R) * 2 > MemberCount.
