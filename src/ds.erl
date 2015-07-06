%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jul 2015 03:52
%%%-------------------------------------------------------------------
-module(ds).
-behaviour(application).
-include("dock.hrl").

%% API
-export([start/2, stop/1, start_link/3, get_cycle/1, release_cycle/2, get_info/1]).

%% to start of the application
start(normal, _Args) ->
  Term = ds_sup:start_link(),
  timer:sleep(1000),
  start_docks_from_state(),
  Term;
start({takeover, _OtherNode}, []) ->
  Term = ds_sup:start_link(),
  timer:sleep(1000),
  start_docks_from_state(),
  Term.

%% to stop the application
stop(_State) ->
  ok.

%% @doc start docking station with given reference.
-spec start_link(DockRef :: term(), Total :: non_neg_integer(), Occupied :: non_neg_integer()) -> ok.
start_link(DockRef, Total, Occupied) ->
  ds_sup:start_child(DockRef, Total, Occupied).

%% @doc get cycle from specified docking station
%% returns {ok, BikeReference} or {error, empty}.
-spec get_cycle(DockRef :: term()) -> {ok, BikeRef :: term()} | {error, empty}.
get_cycle(DockRef) ->
  ds_server:get_cycle(DockRef).


%% @doc release specific cycle to the specific docking station
-spec release_cycle(DockRef :: term(), BikeRefs :: list()) -> ok | {error, full}.
release_cycle(DockRef, BikeRef) ->
  ds_server:release_cycle(DockRef, BikeRef).


%% @doc get info of specific docing station
%% get_info(DockRef::term()) -> {ok, [{total, Total::non_neg_integer()},
%% {occupied, Occupied::non_neg_integer()},
%% {free, Free::non_neg_integer()},
%% {bikeRef, BikeRefList::[term()]}
%% ]}.
get_info(DockRef) ->
  ds_server:get_info(DockRef).



%% @doc helper function to start the docks from db
start_docks_from_state() ->
 lists:foreach(fun(Key) ->
                   case ds_db:get_state(Key) of
                     [] -> ok ;
                     [S=#state{}] -> ds_sup:start_child(S)
                   end
               end, ds_db:all_keys()).
