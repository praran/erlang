%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jul 2015 03:52
%%%-------------------------------------------------------------------
-module(ds).
-author("pradeep").
-behaviour(application).

%% API
-export([start/2, stop/1, start_link/3, get_cycle/1, release_cycle/2, get_info/1]).

%% to start of the application
start(normal, _Args) ->
  ds_sup:start_link().

%% to stop the application
stop(_State) ->
  ok.

%% @doc start docking station with given reference.
-spec start_link(DockRef :: term(), Total :: non_neg_integer(), Occupied :: non_neg_integer()) -> ok.
start_link(DockRef, Total, Occupied) ->
  ds_sup:start_child(DockRef, Total, Occupied).

% @doc get cycle from specified docking station
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