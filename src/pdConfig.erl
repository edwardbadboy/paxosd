-module(pdConfig).

-behaviour(gen_server).

% API
-export([start_link/0, start_link/1, stop/1,
         reloadConfig/2, reloadConfig/1, lookup/2]).

-export([defaultNodeConfPath/0, defaultCommonConfPath/0]).

% utilities
-export([getPdConfigServer/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {config=dict:new()}).


getPdConfigServerAt(Node) ->
    pdUtils:getServerAt(pdConfig, Node).


getPdConfigServer() ->
    getPdConfigServerAt(node()).


defaultNodeConfPath() ->
    filename:join([
        code:priv_dir(paxosd),
        erlang:atom_to_list(node()) ++ "_paxosd.conf"]).


defaultCommonConfPath() ->
    filename:join([
        code:priv_dir(paxosd), "paxosd.conf"]).


loadConfigFile(ConfPath) ->
    {ok, [ConfList]} = file:consult(ConfPath),
    dict:from_list(ConfList).


% API functions
start_link() ->
    start_link(defaultCommonConfPath()).


start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


stop(Server) ->
    gen_server:cast(Server, stop).


reloadConfig(Server) ->
    reloadConfig(Server, defaultCommonConfPath()).


reloadConfig(Server, ConfPath) ->
    gen_server:call(Server, {reloadConfig, ConfPath}).


lookup(Server, Key) ->
    gen_server:call(Server, {lookup, Key}).


% gen_server callback implementation
init(ConfPath) ->
    {ok, #state{config = loadConfigFile(ConfPath)}}.


handle_call({reloadConfig, ConfPath}, _From, State) ->
    {reply, ok, State#state{config=loadConfigFile(ConfPath)}};

handle_call({lookup, Key}, _From, State) ->
    ConfDict = State#state.config,
    {reply, dict:fetch(Key, ConfDict), State}.


handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({msg, From, Msg}, State) ->
    io:format("pdConfig Get message from ~p: ~p~n", [From, Msg]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVer, State, _Extra) ->
    {ok, State}.
