% -*- mode: Erlang; -*-

{application, paxosd,
 [{description, "paxos lease daemon"},
  {vsn, "0.1.0"},
  {modules, [pdApp, pdServer, pdServerSup, paxosd]},
  {registered, [pdServerSup]},
  {mod, {pdApp, []}},
  {applications, [kernel, stdlib]}]}.
