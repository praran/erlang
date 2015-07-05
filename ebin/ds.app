{application, ds,
  [{vsn, "1.0.0"},
    {modules, [ds, ds_server, ds_sup, ds_behaviour]},
    {registered, [dssupervisor]},
    {mod, {ds, []}}
  ]}.