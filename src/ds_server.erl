%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jul 2015 21:37
%%%-------------------------------------------------------------------
-module(ds_server).
%% Generic server behaviour
-behaviour(gen_server).
%% API / Client exports
-export([start_link/3, get_cycle/1, release_cycle/2, get_info/1]).
%% internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


%% @doc start docking station.
-spec start_link(DockRef :: term(), Total :: non_neg_integer(), Occupied :: non_neg_integer()) -> ok.
start_link(DockRef, Total, Occupied) ->
  gen_server:start_link({global, DockRef}, ?MODULE, {DockRef,Total, Occupied}, []).

%% @doc get cycle from specified docking station
%% returns {ok, BikeReference} or {error, empty}.
-spec get_cycle(DockRef :: term()) -> {ok, BikeRef :: term()} | {error, empty}.
get_cycle(DockRef) ->
  gen_server:call({global, DockRef}, get_cycle).


%% @doc release specific cycle to the specific docking station
-spec release_cycle(DockRef :: term(), BikeRefs :: list()) -> ok | {error, full}.
release_cycle(DockRef, BikeRef) ->
  gen_server:call({global, DockRef}, {release_cycle, BikeRef}).


%% @doc get info of specific docing station
%% get_info(DockRef::term()) -> {ok, [{total, Total::non_neg_integer()},
%% {occupied, Occupied::non_neg_integer()},
%% {free, Free::non_neg_integer()},
%% {bikeRef, BikeRefList::[term()]}
%% ]}.
get_info(DockRef) ->
  gen_server:call({global, DockRef}, info).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generic server behaviour
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({DockRef, Total, Occupied}) ->
  %% trapping exits to know when the parent shuts down
  process_flag(trap_exit, true),
  case ds_states_store:get_global_dock_state(DockRef) of
    []                 -> {ok, docking_station:start_link(Total, Occupied)};
    [{DockRef, State}] -> {ok, State}
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Synchronous Calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call(Msg, _From, S) ->
  case Msg of
    get_cycle ->
      case docking_station:get_cycle(S) of
        empty -> {reply, {error, empty}, S};
        {H, NewState} ->
          %% State change:: updating the state in global ets table to maintain state on failure
          ds_states_store:store_global_dock_state(docking_station:get_name_of_pid(self()),NewState),
          {reply, {ok, H}, NewState}
      end;
    info ->
      {reply, docking_station:get_info(S), S};
    {release_cycle, BikeRef} ->
      case docking_station:release_cycle(BikeRef, S) of
        full -> {reply, {error, full}, S};
        NewState ->
          %% State change:: updating the state in global ets table to maintain state on failure
          ds_states_store:store_global_dock_state(docking_station:get_name_of_pid(self()),NewState),
          {reply, {ok}, NewState}
      end
  end.


handle_info(Msg, S) ->
  io:format("Unexpected message: ~p~n", [Msg]),
  {noreply, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Asynchronous Calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_cast(_Msg, S) ->
  io:format("No Async calls supported !!!~n"),
  {noreply, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% termination
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terminate(normal, _State) ->
  io:format("Terminating the docking station ~n"),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hot code deploy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
