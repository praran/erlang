%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jul 2015 10:46
%%%-------------------------------------------------------------------
-module(ds_server_tests).
-include_lib("eunit/include/eunit.hrl").

%% API
-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start_stop_test_() ->
%%     {"The docking station  can be started, stopped with given dock reference",
%%     ?setup(fun is_registered/1)}.


%% create_db_test() ->
%%   io:format("inside create db~n"),
%%   io:format("create db on node: ~p~n",[node()]),
%%   case ds_db:create([node()]) of
%%     {_, {already_exists, _}} -> ds_db:destroy([node()]);
%%     {ok} -> ok
%%   end,
%%   ds_db:start().

some_test_() ->
   [
      ?setup(fun is_registered/1),
       ?setup(fun test_release_cycle/1)].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

start() ->
  io:format("inside start: ~n"),
  create_db(),
  io:format("created db: ~n"),
  DockRef = dock1,
  {ok, Pid} = ds_server:start_link(DockRef, 5, 4),
  {Pid, DockRef}.

stop({_, DockRef}) ->
  ds_server:stop(DockRef),
  destroy_db().

dummy() -> ok.

dummy(_) -> ok.


%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
create_db() ->
  timer:sleep(5000),
  io:format("is registered test: ~n"),
  ds_db:create([node()]),
  io:format("created db ~n"),
  ds_db:start(),
  io:format("started db ~n").


is_registered({Pid, DockRef}) ->
  io:format("is registered test: ~n"),
  [?_assert(erlang:is_process_alive(Pid)),
    ?_assertEqual(Pid, global:whereis_name(DockRef))].

test_release_cycle({_Pid, DockRef}) ->
  {ok, BikRef} = ds_server:get_cycle(DockRef),
  io:format("Got bike ref : ~p~n",[BikRef]),
  {error, Empty} = ds_server:get_cycle(DockRef),
  io:format("got empy : ~p~n",[Empty]),
  [?_assert(BikRef =/= []),
         ?_assert(Empty = empty)].

destroy_db() ->
   ds_db:destroy([node()]).


%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%