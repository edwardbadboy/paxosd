-module(pdProposer).

% API
-export([proposer/4, fire/1]).

% callback for module itself.
-export([sessionStart/1]).

-include("ballotState.hrl").

-record(session, {ballotid=undefined, proposerStore=undefined, proposal=undefined,
                  memberCount=0}).

-define(CALMTIME, 3000).
-define(COLLECTTIMEOUT, 20000).
-define(WAITSTART, 5000).


proposer(BallotID, ProposerStore, MemberCount, OurProposal) ->
    process_flag(trap_exit, true),
    spawn_link(?MODULE, sessionStart,
               [#session{ballotid=BallotID, proposerStore=ProposerStore,
                         proposal=OurProposal, memberCount=MemberCount}]).


fire(Proposer) -> Proposer!{startSession}.


sessionStart(Session) ->
    {A1, A2, A3} = os:timestamp(),
    random:seed(A1, A2, A3),
    receive
        {startSession} ->
            sessionLoop(Session)
    after ?WAITSTART ->
        error
    end.


sessionLoop(Session=#session{ballotid=BallotID, proposerStore=Store,
                             proposal=OurProposal}) ->
    [ProposerState] = ets:lookup(Store, BallotID),
    MBal =  ProposerState#proposerState.mbal,
    ets:update_element(Store, BallotID, {#proposerState.mbal, pdUtils:incBalNum(MBal)}),
    case runStage(Session, prepare) of
        {ok, Responses} ->
            io:format('prepare ok ~w~n', [Responses]),
            Inp = determineInp(OurProposal, Responses),
            io:format('Inp ~w~n', [Inp]),
            ets:update_element(Store, BallotID, {#proposerState.inp, Inp}),
            case runStage(Session, accept) of
                {ok, _R} ->
                    io:format('accept ok~n'),
                    ok;
                _ ->
                    io:format('accept fail~n'),
                    timer:sleep(erlang:round(?CALMTIME * random:uniform())),
                    sessionLoop(Session)
            end;
        _ ->
            io:format('prepare fail~n'),
            timer:sleep(erlang:round(?CALMTIME * random:uniform())),
            sessionLoop(Session)
    end.


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
    flushMsg(),
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
        {{error, {rejected, MBal}}, _} ->
            NewMBal = pdUtils:incBalNumTo(MBal),
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


flushMsg() ->
    receive
        _ -> flushMsg()
    after 0 ->
        ok
    end.
