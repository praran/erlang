%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jul 2015 00:20
%%%-------------------------------------------------------------------
-module(ds_sup).
-behaviour(supervisor).
%% API
-export([start_link/0, stop/0, init/1, stop_child/1, start_child/1, start_child/2, start_child/3]).
-define(MAX_RESTART, 3).
-define(MAX_TIME, 3600).
-define(SUPERVISOR, dssup).
-define(STATES_TABLE, dsstatestable).


%% @doc start the docking station supervisor
%% starts the supervisor with name defined in ?SUPERVISOR
-spec start_link() -> pid().
start_link() ->
  supervisor:start_link({global, dssup}, ?MODULE, []).

%% @doc stop the docking station supervisor
stop() ->
  case whereis(?SUPERVISOR) of
    P when is_pid(P) ->
      exit(P, kill);
    _ -> ok
  end.

%% @doc start child with give refrence with total and occupied
-spec start_child(term(), non_neg_integer(), non_neg_integer()) -> {ok, pid()}.
start_child(DockRef, Total, Occupied) ->
  %% intially store the state in the global states store with given DockRef
  ds_db:add_state(docking_station:create_dock_state(DockRef, Total, Occupied)),
  ChildSpec = {DockRef,
    {ds_server, start_link, [DockRef, Total, Occupied]},
    permanent, infinity, worker, [ds_server]},
  supervisor:start_child({global, ?SUPERVISOR}, ChildSpec).


%% @doc start child with given total and occupied and return a reference
-spec start_child(non_neg_integer(), non_neg_integer()) -> {ok, term()}.
start_child(Total, Occupied) ->
  DockRef = list_to_atom(docking_station:get_random_string(20)),
  %% intially store the state in the global states store with given DockRef
  ds_db:add_state(docking_station:create_dock_state(DockRef, Total, Occupied)),
  ChildSpec = {DockRef,
    {ds_server, start_link, [DockRef, Total, Occupied]},
    permanent, infinity, worker, [ds_server]},
  supervisor:start_child({global, ?SUPERVISOR}, ChildSpec),
  {ok, DockRef}.

%% @doc start child with give refrence with total and occupied
-spec start_child(term()) -> {ok, pid()}.
start_child(S) ->
  %% intially store the state in the global states store with given DockRef
  ds_db:add_state(S),
  {DockRef, Total, Occupied} = docking_station:get_values_from_state(S),
  ChildSpec = {DockRef,
                {ds_server, start_link, [DockRef, Total, Occupied]},
                  permanent, infinity, worker, [ds_server]},
  supervisor:start_child({global, ?SUPERVISOR}, ChildSpec).

%% @doc stop child with given reference
-spec stop_child(term()) -> ok.
stop_child(DocRef) ->
  supervisor:terminate_child(?SUPERVISOR, DocRef),
  supervisor:delete_child(?SUPERVISOR, DocRef).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% generic supervisor init function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc init function for supervisor behaviour
init([]) ->
  {ok, {{one_for_one, ?MAX_RESTART, ?MAX_TIME}, []}}.

