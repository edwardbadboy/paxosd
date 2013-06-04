{node, a, 'a@zhshzhouf17'}.
{node, b, 'b@zhshzhouf17'}.
 
{init, [a,b], [{node_start, [{monitor_master, false}]}]}.
 
{alias, basic, "./basic"}.
 
{logdir, "./logs"}.
 
{suites, [a, b], basic, all}.
