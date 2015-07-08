%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2015 14:21
%%%-------------------------------------------------------------------
-module(ds_db).
-include("dock.hrl").
-define(STATES_TABLE, dock_state).

%% API
-export([create/1, destroy/1, start/0, stop/0, add_state/1, get_state/1,delete_state/1, all_keys/0, is_ready/0, get_db_path/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  DB to store data of dock stations
%%  Using Mnesia database as it supports replication across
%%  multiple nodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc create the database for multiple nodes
%% create([nonode@nohost]) ->
%%   application:set_env(mnesia, dir, get_db_path()),
%%   case mnesia:create_schema([nonode@nohost]) of
%%           ok ->       application:start(mnesia),
%%                       case mnesia:create_table(?STATES_TABLE,
%%                                   [{attributes, record_info(fields, state)},
%%                                     {disc_copies, [nonode@nohost]}]) of
%%                         {_, ok} -> application:stop(mnesia);
%%                         {aborted, Reason} -> {Reason}
%%                       end;
%%     {error, Reason} -> Reason
%%   end;
%% create(Nodes) ->
%%   application:set_env(mnesia, dir, get_db_path()),
%%   case mnesia:create_schema(Nodes) of
%%       ok  -> rpc:multicall(Nodes, application, start, [mnesia]),
%%              case mnesia:create_table(?STATES_TABLE,
%%                                   [{attributes, record_info(fields, state)},
%%                                    {disc_copies, Nodes}]) of
%%                    {_ ,ok } -> rpc:multicall(Nodes, application, stop, [mnesia]);
%%                    {aborted, Reason} -> {Reason}
%%              end;
%%     {error, Reason} -> Reason
%%   end.

create(Nodes) ->
  application:set_env(mnesia, dir, get_db_path()),
  mnesia:stop(),
  mnesia:create_schema(Nodes),
%%   rpc:multicall(Nodes, application, start, [mnesia]),
     application:start(mnesia),
     mnesia:create_table(?STATES_TABLE,
                                  [{attributes, record_info(fields, state)},
                                   {disc_copies, Nodes}]),
     %%rpc:multicall(Nodes, application, stop, [mnesia]),
    is_ready(),
    application:stop(mnesia).

%% mnesia:create_table(state, [{attributes, record_info(fields, state)}, {disc_copies, Nodes}])


%% @doc destroy the schema
destroy(DiscNodesList) ->
  %%rpc:multicall(DiscNodesList, application, stop, [mnesia]),
  mnesia:stop(),
  mnesia:delete_schema(DiscNodesList),
  ok.


%% @doc start the database
start() ->
  application:start(mnesia),
  is_ready().


%% @doc stop the database
stop() ->
  application:stop(mnesia).


%% @doc check if db is ready to use
is_ready() ->
  mnesia:wait_for_tables([?STATES_TABLE], 10000).

%% @doc add state to db
add_state(S = #state{}) ->
  io:format("TRYing to add state : ~p~n",[S]),
  is_ready(),
  F = fun() ->  mnesia:write(S) end,
  mnesia:activity(transaction, F).

%% @doc get state from db
get_state(DockRef) ->
  is_ready(),
  F = fun() -> is_ready(), mnesia:read({?STATES_TABLE, DockRef}) end,
   case mnesia:activity(transaction, F) of
       [] -> [] ;
       [S=#state{}] -> [S]
   end.


%% @doc get state from db
delete_state(DockRef) ->
  F = fun() ->  mnesia:delete({?STATES_TABLE, DockRef}) end,
    mnesia:activity(transaction, F).

%% @doc get all keys from the db
all_keys() ->
  F = fun() -> mnesia:all_keys(?STATES_TABLE) end,
  mnesia:activity(transaction, F).

%% store data files in priv folder
get_db_path() ->
  {ModPath, _} = filename:find_src(?MODULE),
  AppPath = filename:dirname(filename:dirname(ModPath)),
  filename:join(AppPath, "priv").