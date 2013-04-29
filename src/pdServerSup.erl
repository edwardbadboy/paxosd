-module(pdServerSup).
-behaviour(supervisor).

% API
-export([start_link/0]).

% supervisor callbacks
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    Server = {pdServer,
              {pdServer, start_link, []},
              permanent, 2000, worker, [pdServer]},
    ProposalHandler =
        {pdProposalHandler,
         {pdReqRouter, start_link, [pdProposer:proposerWorker()]},
         permanent, 2000, worker, [pdReqRouter]},
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, [Server, ProposalHandler]}}.
