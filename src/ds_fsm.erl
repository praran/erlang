%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jul 2015 11:42
%%%-------------------------------------------------------------------
-module(ds_fsm).
-behaviour(gen_fsm).

%% API / External exports
-export([start_link/3, stop/1, get_cycle/1 , release_cycle/2, get_info/1]).

%% Internal exports
-export([init/1, handle_sync_event/4, handle_event/3, handle_info/3, terminate/3, code_change/4]).

%% Internal states
-export([full/3, empty/3, available/3]).


%% @doc start docking station.
-spec start_link(DockRef :: term(), Total :: non_neg_integer(), Occupied :: non_neg_integer()) -> ok.
start_link(DockRef, Total, Occupied) ->
  gen_fsm:start_link({global , DockRef}, ?MODULE, {DockRef, Total, Occupied}, []).

%% @doc get cycle from specified docking station
%% returns {ok, BikeReference} or {error, empty}.
-spec get_cycle(DockRef :: term()) -> {ok, BikeRef :: term()} | {error, empty}.
get_cycle(DockRef) ->
  gen_fsm:sync_send_event({global, DockRef}, get_cycle).


%% @doc release specific cycle to the specific docking station
-spec release_cycle(DockRef :: term(), BikeRefs :: list()) -> ok | {error, full}.
release_cycle(DockRef, BikeRef) ->
  gen_fsm:sync_send_event({global, DockRef}, {release_cycle, BikeRef}).

%% @doc get info of specific docing station
%% get_info(DockRef::term()) -> {ok, [{total, Total::non_neg_integer()},
%% {occupied, Occupied::non_neg_integer()},
%% {free, Free::non_neg_integer()},
%% {bikeRef, BikeRefList::[term()]}
%% ]}.
get_info(DockRef) ->
  gen_fsm:sync_send_all_state_event({global, DockRef}, info).


stop(DockRef) ->
   gen_fsm:send_all_state_event({global, DockRef}, terminate).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generic fsm behaviour
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc generic init method
-spec init({non_neg_integer(), non_neg_integer()}) -> {ok, term(), term()}.
init({DockRef,Total, Occupied}) ->
  %% trapping exits
  process_flag(trap_exit, true),
  case ds_db:get_state(DockRef)  of
    []      ->    NewState =  docking_station:create_dock_state(DockRef, Total, Occupied),
                  {ok, docking_station:get_fsm_state(NewState), NewState};
    [State] ->    {ok, docking_station:get_fsm_state(State), State}
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Globlal synchronous Calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc global events get info should be able to return
%%      info irrespective of the state the docking FSM is in.
handle_sync_event(info, _From, StateName, S) ->
  {reply, docking_station:get_info(S), StateName, S}.

%% @doc handle global messages currently unsupported
handle_info(Info, StateName, S) ->
  unexpected(Info, StateName),
  {next_state, StateName, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Globlal Asynchronous Calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc handle async events  supported only for terminate
handle_event(terminate, _StateName, S) ->
  {stop, normal, S};
handle_event(_MSg, StateName, S) ->
  io:format("No Async calls supported !!!~n"),
  {next_state, StateName, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  States of the FSM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc  when in full state release cycle should give error and
%%       get_cycle should be able to return a cycle and switch state to
%%       available
full(get_cycle, _From, S) ->
  {H, NewState}  = docking_station:get_cycle(S),
  %% State change:: updating the state in global ets table to maintain state on failure
  ds_db:add_state(NewState),
  {reply, {ok, H}, available, NewState};
full({release_cycle, _BikeRef}, _From, S) ->
  {reply, {error, full}, full, S}.

%% @doc  when in empty state get cycle should return error
%%       when release cycle should be able to accept release
%%       and switch state to available
empty(get_cycle, _From, S) ->
  {reply, {error, empty}, empty, S};
empty({release_cycle, BikeRef}, _From, S) ->
  NewState = docking_station:release_cycle(BikeRef, S),
  %% State change:: updating the state in global ets table to maintain state on failure
  ds_db:add_state(NewState),
  {reply, {ok}, available, NewState}.

%% @doc  when in available state get cycle should be able to return cycle
%%       and if it becomes empty should switch state to empty else stay in available
%%       when release cycle should be able to release cycle and if full
%%       then should switch to full state else stay in available
available(get_cycle, _From, S) ->
  {H, NewState} = docking_station:get_cycle(S),
  %% State change:: updating the state in global ets table to maintain state on failure
  ds_db:add_state(NewState),
  {reply, {ok, H}, docking_station:get_fsm_state(NewState), NewState};
available({release_cycle, BikeRef}, _from, S) ->
  NewState = docking_station:release_cycle(BikeRef, S),
  %% State change:: updating the state in global ets table to maintain state on failure
  ds_db:add_state(NewState),
  {reply, {ok}, docking_station:get_fsm_state(NewState), NewState}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% termination
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc terminate implemenation
terminate(normal, ready, State) ->
  ds_db:delete_state(docking_station:get_dock_ref(State)),
  io:format("Terminating the docking station ~n");
terminate(_Reason, _StateName, State) ->
  ds_db:delete_state(docking_station:get_dock_ref(State)),
  io:format("Terminating the FSM docking station ~n"),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hot code deploy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc helper function to handle unexpected messates
unexpected(Msg, State) ->
  io:format("~p received unknown event ~p while in state ~p~n",
    [self(), Msg, State]).