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
    timer:sleep(erlang:round(?CALMTIME * random:uniform())),
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
                    sessionLoop(Session)
            end;
        _ ->
            io:format('prepare fail~n'),
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
    flushMsg(),
    case Stage of
        prepare -> pdServer:castPrepare(self(),
                                  #prepare{ballotid=BallotID,
                                           msgID=MsgID, mbal=MBal});
        accept ->
            Inp = ProposerState#proposerState.inp,
            pdServer:castAccept(self(),
                                #accept{ballotid=BallotID,
                                        msgID=MsgID, mbal=MBal, inp=Inp})
    end,
    collectResp(Stage, ?COLLECTTIMEOUT, M, MsgID).

collectResp(Stage, Timeout, MemberCount, MsgID) ->
    TRef = make_ref(),
    timer:send_after(Timeout, {collectTimeout, TRef}),
    R = collectRespAcc(Stage, MemberCount, MsgID, TRef, []),
    case isMajority(R, MemberCount) of
        true ->
            io:format("collect resp ok ~w~n", [R]),
            {ok, R};
        _ ->
            io:format("collect resp timeout, MemberCount ~w, Result ~w~n", [MemberCount, R]),
            {error, timeout}
    end.

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
                    Rs;
                _ ->
                    collectRespAcc(Stage, MemberCount, MsgID, TimerRef, Rs)
            end;
        % TODO: handle reject messages
        %R = {ballot, reject, MsgID, _Bal} ->
            %true;
        {collectTimeout, TimerRef} ->
            Response;
        _Msg ->
            io:format("unknown msg ~w~n", [_Msg]),
            collectRespAcc(Stage, MemberCount, MsgID, TimerRef, Response)
    after ?COLLECTTIMEOUT ->
        Response
    end.

isMajority(R, MemberCount) -> length(R) * 2 > MemberCount.

flushMsg() ->
    receive
        _ -> flushMsg()
    after 0 ->
        ok
    end.
