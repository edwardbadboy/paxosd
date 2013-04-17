-record(task, {taskID, workerID, reqQueue=queue:new(), workerState}).
-record(worker, {workerID, taskID}).

% doWork(Req, WorkerState)
% reqHash(Req)
% workerStateInit()
% workerStateClean(State)
-record(workerCallback, {doWork, reqHash,
                         workerStateInit, workerStateClean}).
-record(routes, {tasks, workers, workerCallback}).
