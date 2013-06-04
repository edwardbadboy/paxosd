% the custom field is the second field
% for mbal and bal number
-record(balNum, {n=0, nodeName=node()}).

% mbal: highest ballot number ever seen
% bal: highest ballot number ever accepted
% inp: chosen ballot value
-record(acceptorState, {ballotid=0, mbal=#balNum{}, bal=#balNum{}, inp}).

% mbal: next ballot number to send
% inp: value to be proposed
% proposer: pdProposer process id
-record(proposerState, {ballotid=0, mbal=#balNum{}, inp}).

% inp: value learned
-record(learnerState, {ballotid=0, inp}).

% pID: proposer process id
-record(proposer, {pID, ballotid=0}).


% ballot messages
-record(prepare, {ballotid=0, msgID, mbal=#balNum{}}).
-record(promise, {ballotid=0, msgID, bal=#balNum{}, inp}).

-record(accept, {ballotid=0, msgID, mbal=#balNum{}, inp}).
-record(accepted, {ballotid=0, msgID}).

-record(reject, {ballotid=0, msgID, mbal=#balNum{}}).
-record(learn, {ballotid=0, bal=#balNum{}, inp}).
-record(commit, {ballotid=0, bal=#balNum{}, inp}).

-record(proposerOverride, {determineInp=fun pdProposer:determineInp/2}).

% TODO start jokers and link them together automatically
% TODO read jokers from configuration file
-define(JOKERS, ['joker1@zhshzhouf17', 'joker2@zhshzhouf17']).
-define(CTMASTER, 'testMaster@zhshzhouf17').

% millisecond
-define(JOINTIMEOUT, 20000).
-define(RPCTIMEOUT, 5000).
-define(PROPOSETIMEOUT, 60000).
-define(LEARNTIMEOUT, ?RPCTIMEOUT + ?PROPOSETIMEOUT).

-define(CALMTIME, 3000).
-define(COLLECTTIMEOUT, 20000).
-define(WAITSTART, 5000).

-define(LEASETIMEOUT, 30). % second
