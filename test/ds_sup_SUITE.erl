%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Jul 2015 00:08
%%%-------------------------------------------------------------------
-module(ds_sup_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, end_per_suite/1,init_per_testcase/2, end_per_testcase/2, all/0]).
-export([sup_started_test/1,sup_stop_test/1,child_restart_test/1,child_restart_times_test/1,child_restart_state_maintained_test/1]).

all() -> [sup_started_test,sup_stop_test,child_restart_test,child_restart_times_test,child_restart_state_maintained_test].

%%%%%%%%%%%%%%%%%%%%%%%%
% init per suite
%%%%%%%%%%%%%%%%%%%%%%%%
init_per_suite(Config) ->
  ds_db:create([node()]),
  ds_db:start(),
  Config.

end_per_suite(_Config) ->
  ds_db:destroy([node()]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%
% init per test case
%%%%%%%%%%%%%%%%%%%%%%%%
init_per_testcase(sup_started_test, Config) ->
  {ok, Pid} = ds_sup:start_link(),
  {ok, DockRef} = ds_sup:start_child(3,1),
  [{dockref, DockRef},{pid, Pid}| Config];
init_per_testcase(_, Config) ->
  {ok, Pid} = ds_sup:start_link(),
  {ok, DockRef} = ds_sup:start_child(3,1),
  [{dockref, DockRef},{pid, Pid}| Config].


end_per_testcase(sup_stop_test, _Config) ->
  ok;
end_per_testcase(_ , _Config) ->
  ds_sup:stop(),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%
% Test case
%%%%%%%%%%%%%%%%%%%%%%%%

sup_started_test(Config) ->
   DockRef = ?config(dockref, Config),
   true = erlang:is_process_alive(global:whereis_name(dssup)),
   true = erlang:is_process_alive(global:whereis_name(DockRef)).

sup_stop_test(Config) ->
  DockRef = ?config(dockref, Config),
  ds_sup:stop(),
  true = erlang:is_process_alive(global:whereis_name(DockRef)).

child_restart_test(Config) ->
  DockRef = ?config(dockref, Config),
  true = erlang:is_process_alive(global:whereis_name(DockRef)),
  exit(global:whereis_name(DockRef), kill),
  timer:sleep(1000),
  true = erlang:is_process_alive(global:whereis_name(DockRef)),
  ok.


child_restart_times_test(Config) ->
  DockRef = ?config(dockref, Config),
  true = erlang:is_process_alive(global:whereis_name(DockRef)),
  exit(global:whereis_name(DockRef), kill),
  timer:sleep(1000),
  true = erlang:is_process_alive(global:whereis_name(DockRef)),
  exit(global:whereis_name(DockRef), kill),
  timer:sleep(1000),
  true = erlang:is_process_alive(global:whereis_name(DockRef)),
  exit(global:whereis_name(DockRef), kill),
  timer:sleep(1000),
  true = erlang:is_process_alive(global:whereis_name(DockRef)),
  ok.


child_restart_state_maintained_test(Config) ->
  DockRef = ?config(dockref, Config),
  %% get info
  {info
    , {{total, 3}
    , {occupied, 1}
    , {free, 2}
    , {bikeRef, _}}
  } = ds_server:get_info(DockRef),
  %% get a cycle
  {ok, _BikeRef} = ds_server:get_cycle(DockRef),
  %% check state
  {info
    , {{total, 3}
    , {occupied, 0}
    , {free, 3}
    , {bikeRef, _}}
  } = ds_server:get_info(DockRef),
  %% kill process
  exit(global:whereis_name(DockRef), kill),
  %% should retain state after restart
  timer:sleep(2000),
  {info
    , {{total, 3}
    , {occupied, 0}
    , {free, 3}
    , {bikeRef, _}}
  } = ds_server:get_info(DockRef),
  ok.