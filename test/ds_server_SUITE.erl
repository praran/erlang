%%%-------------------------------------------------------------------
%%% @author pradeep
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jul 2015 17:33
%%%-------------------------------------------------------------------
-module(ds_server_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, end_per_suite/1,init_per_testcase/2, end_per_testcase/2, all/0]).
-export([start_dock_tests/1,get_cycle_tests/1,release_cycle_tests/1,get_info_tests/1]).
-export([get_cycle_when_empty_tests/1,release_cycle_when_full_tests/1]).



all() -> [start_dock_tests, get_cycle_tests,release_cycle_tests,get_info_tests
          ,get_cycle_when_empty_tests,release_cycle_when_full_tests].

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

%%%%%%%%%%%%%%%%%%%%%%%%%
% init per testcase
%%%%%%%%%%%%%%%%%%%%%%%%%%
init_per_testcase(release_cycle_tests, Config) ->
  DockRef = dock1,
  {ok, _Pid} = ds_server:start_link(DockRef, 5, 4),
  [{dockref, DockRef}| Config];
init_per_testcase(release_cycle_when_full_tests, Config) ->
  DockRef = dock1,
  {ok, _Pid} = ds_server:start_link(DockRef, 5, 4),
  [{dockref, DockRef}| Config];
init_per_testcase(_ ,Config) ->
  DockRef = dock1,
  {ok, _Pid} = ds_server:start_link(DockRef, 5, 1),
  [{dockref, DockRef}| Config].


end_per_testcase(release_cycle_tests, Config) ->
  DockRef = ?config(dockref, Config),
   ds_server:stop(DockRef),
  ok;
end_per_testcase(_ , Config) ->
  DockRef = ?config(dockref, Config),
  ds_server:stop(DockRef),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%
% test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_dock_tests(Config) ->
  DockRef = ?config(dockref, Config),
   Pid = global:whereis_name(DockRef),
   true = erlang:is_process_alive(Pid),
  ok.

get_cycle_tests(Config) ->
  DockRef = ?config(dockref, Config),
  {ok,_BikeRef} = ds_server:get_cycle(DockRef),
  {error, empty} = ds_server:get_cycle(DockRef),
  ok.

get_cycle_when_empty_tests(Config) ->
  DockRef = ?config(dockref, Config),
  {ok,_BikeRef} = ds_server:get_cycle(DockRef),
  {error, empty} = ds_server:get_cycle(DockRef),
  ok.


release_cycle_tests(Config) ->
  DockRef = ?config(dockref, Config),
  {ok} = ds_server:release_cycle(DockRef, "asdfasdfs"),
  ok.

release_cycle_when_full_tests(Config) ->
  DockRef = ?config(dockref, Config),
  {ok} = ds_server:release_cycle(DockRef, "asdfasdfs"),
  {error, full} = ds_server:release_cycle(DockRef,"adadfadsf"),
  ok.


get_info_tests(Config) ->
  DockRef = ?config(dockref, Config),
  {info
    , {{total, 5}
    , {occupied, 1}
    , {free, 4}
    , {bikeRef, _}}
  } = ds_server:get_info(DockRef),
  ok.
