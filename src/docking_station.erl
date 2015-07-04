%%%-------------------------------------------------------------------
%%% @author pradeep
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jul 2015 15:54
%%%-------------------------------------------------------------------
-module(docking_station).
%% API
-export([start_link/3, get_cycle/1, release_cycle/2, get_info/1]).
%% internal
-export([init/3,loop/1]).
-record(state, {total = 0,
  occupied = 0,
  free = 0,
  bikeRefs = []}).


%% @doc start docking station.
-spec start_link(DockRef :: term(), Total :: non_neg_integer(), Occupied :: non_neg_integer()) -> ok.
start_link(DockRef, Total, Occupied) ->
  spawn(?MODULE, init, [DockRef, Total, Occupied]).

%% @doc get cycle from specified docking station
%% returns {ok, BikeReference} or {error, empty}.
-spec get_cycle(DockRef :: term()) -> {ok, BikeRef :: term()} | {error, empty}.
get_cycle(DockRef) ->
  Ref = erlang:monitor(process, DockRef),
  DockRef ! {self(), Ref, get_cycle},
  receive
    {Ref, ok, BikeRef} ->
      erlang:demonitor(Ref, [flush]),
      {ok, BikeRef};
    {Ref, error, empty} ->
      erlang:demonitor(Ref, [flush]),
      {error, empty};
    {'DOWN', Ref, process, _Pid, Reason} ->
      {ok, Reason}
  after 5000 ->
    {error, timeout}
  end.


%% @doc release specific cycle to the specific docking station
-spec release_cycle(DockRef :: term(), BikeRefs :: list()) -> ok | {error, full}.
release_cycle(DockRef, BikeRef) ->
  Ref = erlang:monitor(process, DockRef),
  DockRef ! {self(), Ref, {release_cycle, BikeRef}},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {Ref, error, full} ->
      erlang:demonitor(Ref, [flush]),
      {error, full};
    {'DOWN', Ref, process, _Pid, Reason} ->
      {ok, Reason}
  after 5000 ->
    {error, timeout}
  end.


%% @doc get info of specific docing station
%% -spec get_info(DockRef::term()) -> {ok, [{total, Total::non_neg_integer()},
%% {occupied, Occupied::non_neg_integer()},
%% {free, Free::non_neg_integer()},
%% {bikeRef, BikeRefList::[term()]}
%% ]}.
get_info(DockRef) ->
  Ref = erlang:monitor(process, DockRef),
  DockRef ! {self(), Ref, info},
  receive
    {Ref, info, Info} -> Info;
    {'DOWN', Ref, process, _Pid, Reason} ->
      {ok, Reason}
  after 5000 ->
    {error, timeout}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Initializing the state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(DockRef, Total, Occupied) -> {
  loop(#state{total = Total, occupied = Occupied, free = Total-Occupied, bikeRefs = get_bike_refs(Occupied)})
}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Looping and core business logic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Core business logic to maintain the state of the docking station
loop(S = #state{occupied = Occupied, free = Free, bikeRefs = BikeRefs}) ->
  receive
    {From, Ref, get_cycle} ->
      if BikeRefs =:= [] ->
        From ! {Ref, error, empty},
        loop(S);
        BikeRefs =/= [] ->
          [H | T] = BikeRefs,
          From ! {Ref, ok, H},
          NewState = S#state{occupied = Occupied - 1, free = Free + 1, bikeRefs = T},
          loop(NewState)
      end;
    {From, Ref, {release_cycle, BikeRef}} ->
      case Free =< 0 of
        true ->
          From ! {Ref, error, full},
          loop(S);
        false ->
          From ! {Ref,ok},
          loop(S#state{occupied = Occupied+1, free= Free -1, bikeRefs = [BikeRef| BikeRefs]})
      end;
    {From, Ref, info} ->
      From ! {Ref, info
        , {
          {total, S#state.total}
          , {occupied, S#state.occupied}
          , {free, S#state.free}
          , {bikeRef, S#state.bikeRefs}
        }},
      loop(S)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc gets the references for the bikes
get_bike_refs(Num_of_bikes) -> [get_random_string(16) || _A <- lists:seq(1, Num_of_bikes)].

%% @doc generate a random string of given length
get_random_string(Length) ->
  AllowedChars = "0123456789asdfghjklqwertyuiopzxcvbnm",
  get_random_string(Length, AllowedChars).

%% @doc get random string of given length and allowd characters
get_random_string(Length, AllowedChars) ->
  lists:foldl(fun(_, Acc) ->
    [lists:nth(random:uniform(length(AllowedChars)),
      AllowedChars)]
    ++ Acc
  end, [], lists:seq(1, Length)).