-module(pdServerSup).
-behaviour(supervisor).

% API
-export([start_link/0, start_link/1]).

% supervisor callbacks
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).


init([]) ->
    init([pdConfig:defaultCommonConfPath()]);

init([ConfPath]) ->
    Server = {pdServer,
              {pdServer, start_link, []},
              permanent, 2000, worker, [pdServer]},
    ProposalHandler =
        {pdProposalHandler,
         {pdReqRouter, start_link, [pdProposer:proposerWorker()]},
         permanent, 2000, worker, [pdReqRouter]},
    ConfigServer = {pdConfig,
                    {pdConfig, start_link, [ConfPath]},
                    permanent, 2000, worker, [pdConfig]},
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, [Server, ProposalHandler, ConfigServer]}}.
