% -*- mode: Erlang; -*-

{application, paxosd,
 [{description, "paxos lease daemon"},
  {vsn, "0.1.0"},
  {modules, [
      paxosd,
      pdApp,
      pdLeaseServer,
      pdProposer,
      pdServer,
      pdServerSup,
      pdUtils]},
  {registered, [pdServerSup]},
  {mod, {pdApp, []}},
  {applications, [kernel, stdlib]}]}.
