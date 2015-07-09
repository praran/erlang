%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Jul 2015 01:31
%%%-------------------------------------------------------------------
-module(ds_app_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, end_per_suite/1,init_per_testcase/2, end_per_testcase/2, all/0]).
-export([get_cycle_test/1,release_cycle_test/1,get_info_test/1]).

all() ->[get_cycle_test,release_cycle_test,get_info_test].

%%%%%%%%%%%%%%%%%%%%%%%%
% init per suite
%%%%%%%%%%%%%%%%%%%%%%%%
init_per_suite(Config) ->
  ds_db:create([node()]),
  ds_db:start(),
   application:start(ds),
   timer:sleep(2000),
Config.

end_per_suite(_Config) ->
  application:stop(ds),
  ds_db:destroy([node()]),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%
% init per test case
%%%%%%%%%%%%%%%%%%%%%%%%
init_per_testcase(_, Config) ->
  ds:start_link(dock1, 3,2),
  [{dock, dock1} | Config].


end_per_testcase(_ , Config) ->
  ds:stop_dock(?config(dock,Config)),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%
% tests
%%%%%%%%%%%%%%%%%%%%%%%%%%

get_cycle_test(Config) ->
  DockRef = ?config(dock,Config),
  {ok, _BikeRef} = ds:get_cycle(DockRef).

release_cycle_test(Config) ->
  DockRef = ?config(dock,Config),
  {ok} = ds:release_cycle(DockRef, "asdfasdfasdf").

get_info_test(Config) ->
  {info
    , {{total, 3}
    , {occupied, 2}
    , {free, 1}
    , {bikeRef, _}}
  } = ds:get_info(?config(dock,Config)).