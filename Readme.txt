erl -sname a -config config/a -pa ebin

erl -sname b -config config/b -pa ebin

erl -sname c -config config/c -pa ebin

%% only first time
ds_store:setup([a@GBPTL7FBKPHV1,b@GBPTL7FBKPHV1,c@GBPTL7FBKPHV1])

 ds_store:start()

 application:start(ds)

 ds:start_link(dock1, 10,5).