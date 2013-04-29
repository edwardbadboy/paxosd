-module(paxosd).

% API
-export([add/1, minus/1, getValue/0, raise/0, joinCluster/0, makeBallot/1,
         propose/2, propose/3, acceptorState/1, learnerInp/1, learn/1, invalidateInp/1]).

-export([leaseWhose/1, leaseGet/1, leaseWait/1, leaseRefresh/1, leaseRelease/1]).


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
    pdProposer:propose(pdProposer:getPdProposalHandler(), ID, Proposal).


propose(ID, Proposal, Override) ->
    pdProposer:propose(pdProposer:getPdProposalHandler(), ID, Proposal, Override).


acceptorState(ID) ->
    pdServer:acceptorState(pdServer:getPdServer(), ID).


learnerInp(ID) ->
    pdServer:learnerInp(pdServer:getPdServer(), ID).


learn(ID) ->
    pdServer:learn(pdServer:getPdServer(), ID).


invalidateInp(ID) ->
    pdServer:invalidateInp(pdServer:getPdServer(), ID).


leaseWhose(ID) ->
    pdLease:leaseWhose(ID).


leaseGet(ID) ->
    pdLease:leaseGet(ID).


leaseWait(ID) ->
    pdLease:leaseWait(ID).


leaseRefresh(ID) ->
    pdLease:leaseRefresh(ID).


leaseRelease(ID) ->
    pdLease:leaseRelease(ID).
