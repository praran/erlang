%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jul 2015 23:23
%%%-------------------------------------------------------------------
-module(docking_station).
%% API
-export([start_link/2, get_cycle/1, release_cycle/2, get_info/1]).
%% Helper functions
-export([get_random_string/1, get_fsm_state/1, create_dock_state/3,get_dock_ref/1,get_values_from_state/1]).

-include("dock.hrl").



%% @doc start docking station.
-spec start_link(Total :: non_neg_integer(), Occupied :: non_neg_integer()) -> state.
start_link(Total, Occupied) ->
  #dockstate{total = Total, occupied = Occupied, free = (Total - Occupied), bikeRefs = get_bike_refs(Occupied)}.

%% @doc get cycle from specified docking station
-spec get_cycle(State :: state) -> empty | {string(), state}.
get_cycle(State = #dockstate{occupied = Occupied, free = Free, bikeRefs = BikeRefs}) ->
  if BikeRefs =:= [] ->
    empty;
    BikeRefs =/= [] ->
      [H | T] = BikeRefs,
      NewState = State#dockstate{occupied = Occupied - 1, free = Free + 1, bikeRefs = T},
      {H, NewState}
  end.


%% @doc release specific cycle to the specific docking station
-spec release_cycle(BikeRef :: string(), State :: state) -> full | state.
release_cycle(BikeRef, State = #dockstate{occupied = Occupied, free = Free, bikeRefs = BikeRefs}) ->
  if Free =< 0 ->
    full;
    Free > 0 ->
      NewState = State#dockstate{occupied = Occupied + 1, free = Free - 1, bikeRefs = [BikeRef | BikeRefs]},
      NewState
  end.


%% @doc get info of specific docing station
-spec get_info(State :: state) -> {info, state}.
get_info(State) ->
  {info
    , {{total, State#dockstate.total}
    , {occupied, State#dockstate.occupied}
    , {free, State#dockstate.free}
    , {bikeRef, State#dockstate.bikeRefs}}
  }.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc helper to create state from Total and Occupied.
-spec create_dock_state(DockRef :: term(), Total :: non_neg_integer(), Occupied :: non_neg_integer()) -> state.
create_dock_state(DockRef, Total, Occupied) ->
  #dockstate{dockref =  DockRef, total = Total, occupied = Occupied, free = (Total - Occupied), bikeRefs = get_bike_refs(Occupied)}.

%% @doc gets the fsm state based on  the total and occupied
get_fsm_state(_S =#dockstate{total = Total, occupied = Occupied}) ->
  if Occupied =:= 0 ->
    empty;
    Total =:= Occupied ->
      full;
    Total >= Occupied ->
      available
  end.


%% @doc gets the references for the bikes
get_bike_refs(Num_of_bikes) -> [get_random_string(16) || _A <- lists:seq(1, Num_of_bikes)].

%% @doc generate a random string of given length
get_random_string(Len) ->
  Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
  ChrsSize = size(Chrs),
  F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
  lists:foldl(F, "", lists:seq(1, Len)).

%% @doc extract dockref from State
get_dock_ref(_S = #dockstate{dockref =  DockRef}) ->
  DockRef.

get_values_from_state(_S = #dockstate{dockref =  DockRef, total = Total,  occupied = Occupied}) ->
  {DockRef, Total, Occupied}.


