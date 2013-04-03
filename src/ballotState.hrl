% for mbal and bal number
-record(balNum, {n=0, nodeName=node()}).

% mbal: highest ballot number ever seen
% bal: highest ballot number ever accepted
% inp: chosen ballot value
-record(acceptorState, {ballotid=0, mbal=#balNum{}, bal=#balNum{}, inp}).

% mbal: next ballot number to send
% inp: value to be proposed
% proposer: pdProposer process id
-record(proposerState, {ballotid=0, mbal=#balNum{}, inp, proposer}).

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

% TODO start jokers and link them together automatically
-define(JOKERS, ['joker1@zhshzhouf17', 'joker2@zhshzhouf17']).
