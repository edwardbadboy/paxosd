-module(pdLeaseServer).
-behaviour(gen_server).

% API
-export([getServerAt/1, getServer/0, start_link/0, stop/1,
         test/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% TODO: insert a callback in proposer for determining timeouted lease as undefined inp


% API functions
getServerAt(Node) ->
    pdUtils:getServerAt(pdLeaseServer, Node).


getServer() ->
    getServerAt(node()).


start_link() ->
    gen_server:start_link(?MODULE, [], []).


stop(Server) ->
    gen_server:cast(Server, stop).


test(Server) ->
    gen_server:call(Server, test).


init([]) ->
    {ok, undefined}.


handle_call(test, _From, State) ->
    io:format("test~n"),
    {reply, ok, State};

handle_call(testReply, From, State) ->
    io:format("testReply ~w~n", [From]),
    spawn(?MODULE, sendReply, [From, 123]),
    {noreply, State}.


handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVer, State, _Extra) ->
    {ok, State}.
