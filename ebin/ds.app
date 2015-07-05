{application, ds,
  [{vsn, "1.0.0"},
    {modules, [ds, ds_server, ds_sup, ds_behaviour,ds_fsm, ds_states_store]},
    {registered, [dssupervisor]},
    {mod, {ds, []}}
  ]}.