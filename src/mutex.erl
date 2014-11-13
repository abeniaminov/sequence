%%   Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson


-module(mutex).
-export([start/0, stop/1]).
-export([wait/1, signal/1]).
-export([init/0]).

start() ->
     spawn_link(?MODULE, init, []).
    
stop(MPid) ->
    unlink(MPid),
    MPid ! stop.

wait(MPid) ->
    MPid ! {wait, self()},
    receive 
    	ok -> ok;
    	terminate -> terminate 
    after 1000 -> timeout 
    end.

signal(MPid) ->
    MPid ! {signal, self()}, ok.

init() ->
    process_flag(trap_exit, true),
    free().

free() ->
    receive
        {wait, Pid} ->
            case catch(link(Pid)) of
                {'EXIT', _Why} ->
                    free();
                _ ->   
                    Pid ! ok,
                    busy(Pid)
            end;        
        {'EXIT', _Pid, _Info} ->
            free();    
        stop -> 
            terminate()
    end.

busy(Pid) ->
    receive
        {signal, Pid} -> 
            free();
        {'EXIT', _Pid, _Info} ->
            free()    
    end.

terminate() ->
    receive
        {wait, Pid} ->
            Pid ! terminate,
            terminate();
        {'EXIT', _Pid, _Info} ->
            terminate()    
    after
        0 -> ok
    end.