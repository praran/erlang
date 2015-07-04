%%%-------------------------------------------------------------------
%%% @author pradeep
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jul 2015 15:54
%%%-------------------------------------------------------------------
-module(docking_station).
-author("pradeep").
-compile(export_all).
%% API
-export([]).
-record(state, {total =0,
                occupied=0,
                free=0,
                bikeRefs=[]}).

%% -record(state, {server,
%%   name="",
%%   to_go=0}).

%% @doc start docking station.
-spec start_link(DockRef::term(), Total::non_neg_integer(), Occupied::non_neg_integer()) -> ok.
start_link(DockRef, Total, Occupied) ->
   spawn(?MODULE, init, [DockRef, Total, Occupied]).

%% @doc get cycle from specified docking station
%% returns {ok, BikeReference} or {error, empty}.
-spec get_cycle(DockRef::term()) -> {ok, BikeRef::term()} | {error, empty}.
get_cycle(DockRef) ->  undefined.


%% @doc release specific cycle to the specific docking station
-spec release_cycle(DockRef::term(), BikeRefs::list()) -> ok | {error, full}.
release_cycle(DockRef, BikeRefs) ->  undefined.


%% @doc get info of specific docing station
%% -spec get_info(DockRef::term()) -> {ok, [{total, Total::non_neg_integer()},
%% {occupied, Occupied::non_neg_integer()},
%% {free, Free::term()},
%% {bikeRef, BikeRefList::list()}
%% ]}.
get_info(DockRef) -> undefined.


init(DockRef, Total, Occupied) -> {
   loop(#state{total = Total, occupied = Occupied, free =  Total- Occupied, bikeRefs = []})
}.

loop(S=#state{}) -> {
  receive
    {From, Ref, get_cycle} ->
        ok;
    {From, Ref, {release_cycle, BikeRefs}} ->
      ok

  end

}.


get_bikeRefs(Num) -> undefined
  .

get_random_string(Length) ->
  AllowedChars = [0,1,2,3,4,5,6,7,8,9,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s],
  lists:foldl(fun(_, Acc) ->
    [lists:nth(random:uniform(length(AllowedChars)),
      AllowedChars)]
    ++ Acc
  end, [], lists:seq(1, Length)).