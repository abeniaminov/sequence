%% -*- coding: utf-8 -*-

%% Copyright (c) 2014
%%
%%
%% Alexander Beniaminov (abeniaminov@gmail.com)
%%
%% Permission is  hereby  granted,  free of charge,  to any person
%% obtaining  a copy of this software and associated documentation
%% files (the "Software"),to deal in the Software without restric-
%% tion,  including  without  limitation the rights to use,  copy,
%% modify, merge,  publish,  distribute,  sublicense,  and/or sell
%% copies  of the  Software,  and to  permit  persons to  whom the
%% Software  is  furnished  to do  so,  subject  to the  following
%% conditions:
%%
%% The above  copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF  MERCHANTABILITY,  FITNESS  FOR  A  PARTICULAR  PURPOSE  AND
%% NONINFRINGEMENT. IN  NO  EVENT  SHALL  THE AUTHORS OR COPYRIGHT
%% HOLDERS  BE  LIABLE FOR  ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT,  TORT  OR OTHERWISE,  ARISING
%% FROM,  OUT OF OR IN CONNECTION WITH THE SOFTWARE  OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%% 

-module(db_seq).

-export([
	     open_db/0,
	     close_db/0,
	     create_seq/2,
	     delete_seq/1,
	     get_next_id/1,
         set_id/2,
         restart_mutex/1,
         set_default_path/0
	     ]).

-define(APP, sequence).
-define(ENV(Var, Def), (get_env(Var, Def))).
-define(TYPE,   ?ENV(type, dets)).
-define(DB,   (?ENV(path, "priv/seq.dets"))).

set_default_path() ->
    filename:join([code:priv_dir(sequence) , "seq.dets"]).      


-spec get_env(Var :: atom(), Def :: term()) -> term().
get_env(Var, Def) ->
    case application:get_env(?APP, Var) of
        undefined -> Def;
        {ok, Val} -> Val
    end.


open_db() -> 
    case catch(ets:new(mutex_pids, [named_table])) of
    	{'EXIT', _Why} ->
            ok;
        _ ->
            dets:open_file(seq_db, [{file, ?DB}]),
            dets:traverse(seq_db, fun({SeqName, _Val}) -> 
    		    ets:insert(mutex_pids, {SeqName, mutex:start()}), continue end)     
    end.

close_db() ->
    ets:foldl(fun({_SeqName, MPid}, I) -> mutex:stop(MPid), I end , 0, seq_db ),
    dets:close(seq_db),
    ets:close(mutex_pids). 

restart_mutex(M) when is_pid(M)->
    Key =  hd(hd(ets:match(mutex_pids, {'$1', M }))),
    ets:insert(mutex_pids, {Key, mutex:start()}),
    true.


create_seq(SeqName, Val) when is_integer(Val) -> 
    case dets:lookup(seq_db, SeqName) of
    	[] -> dets:insert(seq_db, {SeqName, Val}),
    	      ets:insert(mutex_pids, {SeqName, mutex:start()}),
    	      true;
    	_  -> false
    end.	         

delete_seq(SeqName) ->
    V = ets:lookup(mutex_pids, SeqName),
    case V of
    	[] -> 
    	    false;
    	[{SeqName, MPid}] -> 
    	    mutex:stop(MPid), 
    	    ets:delete(mutex_pids, SeqName), 
    	    dets:delete(seq_db, SeqName), 
    	    true
    end.

get_next_id(SeqName) ->
    V = ets:lookup(mutex_pids, SeqName),
    case V of
        [] -> 
            {false , -1};
        [{SeqName, MPid}] ->
            case mutex:wait(MPid) of 
            	ok -> Val = dets:update_counter(seq_db, SeqName, 1),
                      mutex:signal(MPid),
                      {true, Val};
                timeout -> {false , timeout};
                terminate -> {false , terminate}
            end          
    end.

set_id(SeqName, Val) when is_integer(Val) ->
    V = ets:lookup(mutex_pids, SeqName),
    case V of
        [] -> 
            {false, Val};
        [{SeqName, MPid}] ->
            case mutex:wait(MPid) of  
            	ok -> dets:insert(seq_db, {SeqName, Val}),
                      mutex:signal(MPid),
                      {true, Val};
                timeout -> {false , timeout};
                terminate -> {false , terminate}                
            end      
    end.
