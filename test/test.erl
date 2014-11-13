-module(test).

-export([test/3,
         testr/3,
         collect/3,
         worker/5]).



testr(CntCircles, Sleep, N0) ->
      testr(CntCircles, Sleep, N0, 0).

testr(_CntCircles, _Sleep, N0, N0) -> ok;
testr(CntCircles, Sleep, N0, N) ->
    test(round(math:pow(2,N)), CntCircles, Sleep ),
    testr(CntCircles, Sleep, N0, N + 1).


test(CntProc, CntCircles, Sleep) ->
    ets:new(ecollect,[named_table, public]),
    dets:open_file(dcollect, [{file, "collect.dets"}]),
    register(owner, self()),
    
    register(collector, spawn(?MODULE, collect,  [CntProc, CntCircles, Sleep])),

    test_run(CntProc, CntCircles, Sleep, now()),
    receive 
    	stop -> stop
    end,
    ets:to_dets(ecollect, dcollect),
    ets:delete(ecollect),
    dets:close(dcollect),
    unregister(owner).
    


test_run(0,_CntCircles,_Sleep, _TimeStart ) -> ok;
test_run(CntProc, CntCircles, Sleep, TimeStart ) ->
    spawn(?MODULE, worker, [Sleep, p, CntProc, CntCircles, TimeStart]),
    test_run(CntProc-1, CntCircles, Sleep, TimeStart).


collect(0, 0, _Sleep) ->
    owner ! stop;
collect(CntProc, CntCircles, Sleep) ->
    receive
        {res, {Res,Id}, C, Count, Seconds} ->
            ets:insert_new(ecollect, {Id, Res, CntCircles, C, Count, Seconds}),
            collect(CntProc, Count-1, Sleep);
    	stop -> 
            collect(CntProc-1, CntCircles, Sleep)

    end.             
    
worker(_Sleep, _Seq, _CntProc, 0, _TimeStart) -> 
        collector ! stop,
        ok;
worker(Sleep, Seq, CntProc, Count, TimeStart) ->
    receive 
    	{sleep, Val} when Sleep + Val > 5  -> worker(Sleep + Val, Seq, CntProc, Count, TimeStart);
    	{sleep, _Val}   -> worker(Sleep, Seq, CntProc, Count, TimeStart)
    after random:uniform(Sleep) ->
    	Id = sequence:get_next_id(Seq),
    	collector ! {res, Id, CntProc, Count, timer:now_diff(now(), TimeStart) div 1000000},
    	worker(Sleep, Seq, CntProc, Count-1, TimeStart)
      end.		 