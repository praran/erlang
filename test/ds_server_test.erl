%%%-------------------------------------------------------------------
%%% @author pradeep
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jul 2015 17:33
%%%-------------------------------------------------------------------
-module(ds_server_test).
-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, end_per_suite/1,init_per_testcase/2, end_per_testcase/2, all/0]).
-export([start_dock_tests/1]).



all() -> [start_dock].

init_per_suite(Config) ->
  Priv = ds_db:get_db_path(),
  application:set_env(mnesia, dir, Priv),
  ds_db:create([node()]),
  ds_db:start(),
  Config.

end_per_suite(_Config) ->
  ds_db:destroy([node()]),
  ok.

init_per_testcase(start_dock_tests, Config) ->
  DockRef = dock1,
  {ok, _Pid} = ds_server:start_link(DockRef, 5, 4),
  [{dockref, DockRef}| Config].


end_per_testcase(start_dock_tests, Config) ->
  DockRef = ?config(dockref, Config),
   ds_server:stop(DockRef),
  ok.

start_dock_tests(Config) ->
  DockRef = ?config(dockref, Config),
   global:whereis_name(DockRef),
  {ok, Bike} = ds_server:get_cycle(DockRef),
  Bike =/= [],
  ok.