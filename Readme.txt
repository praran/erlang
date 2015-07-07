erl -sname a -config config/a -pa ebin

erl -sname b -config config/b -pa ebin

erl -sname c -config config/c -pa ebin


%% only first time
ds_db:create([a@GBPTL7FBKPHV1,b@GBPTL7FBKPHV1,c@GBPTL7FBKPHV1]).


ds_db:create([a@praran,b@praran,c@praran]).

ds_db:start()

application:start(ds)

ds:start_link(dock1, 10,5).


erl -mnesia dir '"/ldisc/scratch/Mnesia.Company"'