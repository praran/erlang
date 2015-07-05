%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jul 2015 18:50
%%%-------------------------------------------------------------------
-module(ds_states_store).
-define(STATES_TABLE,dsstatestable).
%% process states functions
-export([create_global_state_table/0, get_global_dock_state/1, store_global_dock_state/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Store / Retreive Process States  Functions
%% for the sake of simplicity the storing is done
%% as serial operations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc creates a new ets table
-spec create_global_state_table() -> term().
create_global_state_table() ->
  ets:new(?STATES_TABLE, [named_table, public, set]).

%% @doc lookup a key from the global store
-spec get_global_dock_state(term()) -> [] | [{term(), term()}].
get_global_dock_state(Key) ->
  ets:lookup(?STATES_TABLE, Key).

%% @doc insert key value into the store
-spec store_global_dock_state(term(), term()) -> true.
store_global_dock_state(Key, Value) ->
  ets:insert(?STATES_TABLE, {Key, Value}).
