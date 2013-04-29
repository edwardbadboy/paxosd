-module(pdReqRouter).
-behaviour(gen_server).

-include("pdReqRouter.hrl").

% API
-export([start_link/1, stop/1,
         test/1, request/2, request/3,
         workerReturn/2]).

-export([testDispatch/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).


% called by worker process to return the new task State
workerReturn(WorkerState, R) ->
    exit({return, WorkerState, R}).


start_link(WorkerCallback) ->
    gen_server:start_link(?MODULE, [WorkerCallback], []).


stop(Server) ->
    gen_server:cast(Server, stop).


test(Server) ->
    gen_server:call(Server, test).


request(Server, Req, Timeout) ->
    gen_server:call(Server, {request, Req}, Timeout).


request(Server, Req) ->
    request(Server, Req, infinity).


init([WorkerCallback]) ->
    process_flag(trap_exit, true),
    T = ets:new(taskStore, [set, {keypos, #task.taskID}, public]),
    W = ets:new(workerStore, [set, {keypos, #worker.workerID}, public]),
    {ok, #routes{tasks=T, workers=W, workerCallback=WorkerCallback}}.


handle_call(test, _From, State) ->
    io:format("test~n"),
    {reply, ok, State};

handle_call({request, Req}, From,
            State=#routes{tasks=T, workers=W, workerCallback=WCB}) ->
    io:format("Server got request ~w~n", [Req]),
    ReqHash = WCB#workerCallback.reqHash,
    WorkerInit = WCB#workerCallback.workerStateInit,
    TID = ReqHash(Req),
    [Task] = pdUtils:etsLookup(T, TID, #task{taskID=TID}),
    case Task#task.workerState of
        undefined ->
            ets:update_element(T, TID, {#task.workerState, WorkerInit()});
        _ -> ok
    end,
    Q1 = Task#task.reqQueue,
    Q2 = queue:in({From, Req}, Q1),
    ets:update_element(T, TID, {#task.reqQueue, Q2}),
    case Task#task.workerID of
        undefined ->
            startWorker(TID, T, W, WCB), ok;
        WID when is_pid(WID) -> ok
    end,
    {noreply, State}.


handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'EXIT', Pid, Reason},
            State=#routes{tasks=TStore, workers=WStore, workerCallback=WCB}) ->
    io:format("exit signal ~w~n", [Reason]),
    case ets:lookup(WStore, Pid) of
        [#worker{taskID=TID}] ->
            ets:delete(WStore, Pid),
            case ets:lookup(TStore, TID) of
                [Task = #task{reqQueue=Q1, workerState=WorkerState}] ->
                    {{value, {From, _Req}}, Q2} = queue:out(Q1),
                    WorkerState1 = case Reason of
                        {return, NewWorkerState, R} ->
                            gen_server:reply(From, {ok, R}),
                            NewWorkerState;
                        _ ->
                            gen_server:reply(From, {error, Reason}),
                            WorkerState
                    end,
                    ets:insert(TStore,
                               Task#task{workerID=undefined, reqQueue=Q2,
                                         workerState=WorkerState1}),
                    startWorker(TID, TStore, WStore, WCB),
                    ok;
                _ -> ok
            end;
        _ -> ok
    end,
    {noreply, State};

handle_info(Info, State) ->
    io:format("unknown message ~w~n", [Info]),
    {noreply, State}.


terminate(_Reason, #routes{tasks=T, workers=W, workerCallback=WCB}) ->
    CleanWorkerState = fun(#task{workerState=WorkerState}, AccIn) ->
            Clean = WCB#workerCallback.workerStateClean,
            Clean(WorkerState),
            AccIn
    end,
    ets:foldl(CleanWorkerState, ok, T),
    lists:foreach(fun ets:delete/1, [T, W]),
    ok.


code_change(_OldVer, State, _Extra) ->
    {ok, State}.


startWorker(TID, TaskStore, WorkerStore,
            #workerCallback{doWork=DoWork}) ->
    case ets:lookup(TaskStore, TID) of
        [#task{reqQueue=Q1, workerState=WorkerState}] ->
            case queue:peek(Q1) of
                {value, {_From, Req}} ->
                    W = spawn_link(fun() -> DoWork(Req, WorkerState) end),
                    ets:update_element(TaskStore, TID, {#task.workerID, W}),
                    ets:insert(WorkerStore, #worker{workerID=W, taskID=TID});
                empty -> ok
            end;
        _ -> ok
    end,
    ok.


testWorker() ->
    DoWork = fun(Req={_RID, SleepTime}, WorkerState) ->
            io:format("Worker pid ~w req ~w state ~w~n",
                      [self(), Req, WorkerState]),
            receive
            after SleepTime ->
                    SleepTime
            end,
            pdReqRouter:workerReturn(SleepTime + WorkerState, ok)
    end,
    ReqHash = fun({RID, _SleepTime}) -> RID end,
    WorkerStateInit = fun() -> 0 end,
    WorkerStateClean = fun(State) ->
            io:format("final worker state ~w~n", [State]) end,
    #workerCallback{
        doWork=DoWork, reqHash=ReqHash,
        workerStateInit=WorkerStateInit, workerStateClean=WorkerStateClean}.


testDispatch() ->
    {ok, S} = start_link(testWorker()),
    spawn(fun() ->
            io:format("req result ~w~n", [request(S, {10, 5000})]),
            io:format("req result ~w~n", [request(S, {10, 1000})])
          end),
    spawn(fun() ->
            io:format("req result ~w~n", [request(S, {10, 2000})]),
            io:format("req result ~w~n", [request(S, {10, 1000})])
          end),
    spawn(fun() ->
            io:format("req result ~w~n", [request(S, {20, 5000})]),
            io:format("req result ~w~n", [request(S, {20, 1000})])
          end),
    spawn(fun() ->
            io:format("req result ~w~n", [request(S, {20, 2000})]),
            io:format("req result ~w~n", [request(S, {20, 1000})])
          end),
    receive
    after 20000 ->
            ok
    end,
    stop(S).
