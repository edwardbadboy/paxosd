{node, joker1, 'joker1@zhshzhouf17'}.
{node, joker2, 'joker2@zhshzhouf17'}.
{node, bob, 'bob@zhshzhouf17'}.
{node, alice, 'alice@zhshzhouf17'}.

{init, [joker1, joker2], [{node_start, [{monitor_master, true}]}]}.
{init, [bob, alice], [{node_start, [{monitor_master, true}, {erl_flags, "-pa ${TOPSRCDIR}/ebin"}]}]}.

{suites, [bob, alice], "./distributed", all}.

{create_priv_dir, master, auto_per_run}.
{create_priv_dir, all_nodes, auto_per_run}.

{logdir, all_nodes, "./logs"}.
