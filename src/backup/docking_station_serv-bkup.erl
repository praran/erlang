%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jul 2015 21:37
%%%-------------------------------------------------------------------
-module('docking_station_serv-bkup').
-define(NODE, 'a@localhost').
-define(NAME, ?MODULE).
-define(SERVER, {?NAME, ?NODE}).
%% generic server behaviour
-behaviour(gen_server).

%% API/ Client exports
-export([start_link/3, get_cycle/1, release_cycle/2, get_info/1]).
%% internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {total = 0,
  occupied = 0,
  free = 0,
  bikeRefs = []}).


%% @doc start docking station.
-spec start_link(DockRef :: term(), Total :: non_neg_integer(), Occupied :: non_neg_integer()) -> ok.
start_link(DockRef, Total, Occupied) ->
  gen_server:start_link(?MODULE,{DockRef, Total, Occupied},[]).

%% @doc get cycle from specified docking station
%% returns {ok, BikeReference} or {error, empty}.
-spec get_cycle(DockRef :: term()) -> {ok, BikeRef :: term()} | {error, empty}.
get_cycle(DockRef) ->
  gen_server:call(DockRef, get_cycle).


%% @doc release specific cycle to the specific docking station
-spec release_cycle(DockRef :: term(), BikeRefs :: list()) -> ok | {error, full}.
release_cycle(DockRef, BikeRef) ->
  gen_server:call(DockRef, {release_cycle, BikeRef}).


%% @doc get info of specific docing station
%% get_info(DockRef::term()) -> {ok, [{total, Total::non_neg_integer()},
%% {occupied, Occupied::non_neg_integer()},
%% {free, Free::non_neg_integer()},
%% {bikeRef, BikeRefList::[term()]}
%% ]}.
get_info(DockRef) ->
  gen_server:call(DockRef, info).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generic server behaviour
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({DockRef, Total, Occupied}) ->
  {ok, #state{total = Total, occupied = Occupied, free =(Total - Occupied), bikeRefs = get_bike_refs(Occupied)}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Synchronous Calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call(Msg, _From, S = #state{occupied = Occupied, free = Free, bikeRefs = BikeRefs}) ->
  case Msg of
    get_cycle ->
      if BikeRefs =:= [] ->
        {reply, {error, empty}, S};
        BikeRefs =/= [] ->
          [H | T] = BikeRefs,
          NewState = S#state{occupied = Occupied - 1, free = Free + 1, bikeRefs = T},
          {reply, {ok, H}, NewState}
      end;
    info ->
      {reply, {info
        , {{total, S#state.total}
          , {occupied, S#state.occupied}
          , {free, S#state.free}
          , {bikeRef, S#state.bikeRefs}}
      }, S};
    {release_cycle, BikeRef} ->
      if Free=< 0 ->
        {reply, {error, full}, S};
        Free > 0 ->
          NewState = S#state{occupied = Occupied + 1, free = Free - 1, bikeRefs = [BikeRef | BikeRefs]},
          {reply, {ok}, NewState}
      end
  end.

handle_info(Msg, S) ->
  io:format("Unexpected message: ~p~n",[Msg]),
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc gets the references for the bikes
-spec get_bike_refs(non_neg_integer()) -> [string()].
get_bike_refs(Num_of_bikes) -> [get_random_string(16) || _A <- lists:seq(1, Num_of_bikes)].

%% @doc generate a random string of given length
-spec get_random_string(non_neg_integer()) -> string().
get_random_string(Length) ->
  AllowedChars = "0123456789asdfghjklqwertyuiopzxcvbnm",
  get_random_string(Length, AllowedChars).

%% @doc get random string of given length and allowd characters
-spec get_random_string(non_neg_integer(), string()) -> string().
get_random_string(Length, AllowedChars) ->
  lists:foldl(fun(_, Acc) ->
    [lists:nth(random:uniform(length(AllowedChars)),
      AllowedChars)]
    ++ Acc
  end, [], lists:seq(1, Length)).