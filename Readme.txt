Running Tests
================

ct_run -pa ebin/ -spec ds.spec


Running application on multiple nodes
===============================================

Step 1 :
========

erl -sname a -config config/a -pa ebin

erl -sname b -config config/b -pa ebin

erl -sname c -config config/c -pa ebin


Step 2:
==========

on one of the node  create the database
----------------------------------------
ds_db:create([a@GBPTL7FBKPHV1,b@GBPTL7FBKPHV1,c@GBPTL7FBKPHV1]).
ds_db:create([a@praran,b@praran,c@praran]).

NOTE !!!!!!!!!!!
  change the node names accordingly in the each of the .config files in config folder

step 3:
=========

start db on all nodes
----------------------
ds_db:start()

step 4:
==========

start application on all nodes
-------------------------------

application:start(ds)

step 5:
===========

start dock and use functions
-------------------------------
ds:start_link(dock1, 10,5).
ds:release_cycle(dock1).
ds:get_info(dock1).


