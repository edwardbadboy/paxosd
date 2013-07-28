-module(pdLease).

-include("ballotState.hrl").

% API
-export([leaseWhose/1, leaseGet/1, leaseWait/1, leaseRefresh/1, leaseRelease/1]).

% callback
-export([determineInp/2]).


leaseWhose(ID) ->
    paxosd:learn(ID).


leaseGet(ID) ->
    Me = self(),
    paxosd:propose(
        ID, {Me, pdUtils:nowUTC()},
        #proposerOverride{determineInp=fun ?MODULE:determineInp/2}),
    case paxosd:learnerInp(ID) of
        {Me, LeaseStartTime} -> {ok, LeaseStartTime};
        Inp -> {error, Inp}
    end.


leaseWait(ID) ->
    case leaseGet(ID) of
        {ok, R} -> {ok, R};
        {error, Inp} ->
            SecondsLeft =
                case Inp of
                    {_, When} ->
                        erlang:max(
                            erlang:round((?LEASETIMEOUT - (pdUtils:nowUTC() - When)) / 2 *
                                         random:uniform()),
                            1);
                    undefined ->
                        erlang:round(?LEASETIMEOUT / 2 * random:uniform())
                end,
            io:format("Get lease fail, try again in ~ws~n", [SecondsLeft]),
            timer:sleep(SecondsLeft * 1000),
            leaseWait(ID)
    end.


leaseRefresh(ID) -> leaseGet(ID).


leaseRelease(ID) ->
    % Safe to call leaseRelease even if the lease is not held by us. If the
    % lease is held by other node and it's not timeout, determineInp will
    % select the original lease owner to commit, so does not change anything
    % except the mbal.
    paxosd:propose(
        ID, {self(), 0},
        #proposerOverride{determineInp=fun ?MODULE:determineInp/2}).


isInpTimeout({_Who, When}) ->
    (pdUtils:nowUTC() - When) > ?LEASETIMEOUT.


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
        undefined -> OurProposal; % first blood of the lease
        #promise{inp=Inp} ->
            case isInpTimeout(Inp) of
                true -> OurProposal; % lease timeout
                _ ->
                    {Me, _MyStartTime} = OurProposal,
                    case Inp of
                        {Me, _} -> OurProposal; % lease refresh
                        _ -> Inp % lease held by other process
                    end
            end
    end.
