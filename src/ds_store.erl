%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2015 14:21
%%%-------------------------------------------------------------------
-module(ds_store).
-include("dock.hrl").
-define(STATES_TABLE, state).

%% API
-export([setup/1, add_state/1, get_state/1, start/0, all_keys/0, is_ready/0]).

%% @doc setup the database
setup(Nodes) ->
  ok = mnesia:create_schema(Nodes),
  rpc:multicall(Nodes, application, start, [mnesia]),
  mnesia:create_table(?STATES_TABLE,
                      [{attributes, record_info(fields, state)},
                       {disc_copies, Nodes}]),
  rpc:multicall(Nodes, application, stop, [mnesia]).

%% start the database
start() ->
  application:start(mnesia).

%% check if db is ready to use
is_ready() ->
  mnesia:wait_for_tables([?STATES_TABLE], 5000).

%% add state to db
add_state(S = #state{}) ->
  F = fun() -> mnesia:write(S) end,
  mnesia:activity(transaction, F).

%% get state from db
get_state(DockRef) ->
  F = fun() ->  mnesia:read({?STATES_TABLE, DockRef}) end,
   case mnesia:activity(transaction, F) of
       [] -> [] ;
       [S=#state{}] -> [S]
   end.

%% get all keys from the db
all_keys() ->
  F = fun() -> mnesia:all_keys(?STATES_TABLE) end,
  mnesia:activity(transaction, F).

