%% -*- coding: utf-8 -*-

%% Copyright (c) 2014
%%
%% Russian language Implementation of Porter Stemming Algorithm in Erlang  
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



-module(sequence).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([stop/0, create/1, create/2, delete/1, get_next_id/1, set_id/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(Args) ->
    process_flag(trap_exit, true),
    db_seq:open_db(),
    {ok, Args}.    

stop() ->
    gen_server:cast(?MODULE, stop).    

create(SeqName) ->
    gen_server:call(?MODULE, {create, SeqName, 0}).

create(SeqName, Val) ->
    gen_server:call(?MODULE, {create, SeqName, Val}).    

delete(SeqName) ->
    gen_server:call(?MODULE, {delete, SeqName}).       

get_next_id(SeqName) -> 
    db_seq:get_next_id(SeqName).

set_id(SeqName, Val) ->
    gen_server:call(?MODULE, {setid, SeqName, Val}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

handle_call({create, SeqName, Val}, _From, _State) ->
    Res = db_seq:create_seq(SeqName, Val),
    {reply, Res, null};

handle_call({delete, SeqName}, _From, _State) ->
    Res = db_seq:delete_seq(SeqName),
    {reply, Res, null};

handle_call({setid, SeqName, Val}, _From, _State) ->
    Res = db_seq:set_id(SeqName, Val),
    {reply, Res, null}.        

handle_cast(stop, State) ->
    db_seq:close_db(),    
    {stop, normal, State}.

handle_info({'EXIT', Pid, _Info}, State) ->
    db_seq:restart_mutex(Pid),
    {noreply, State};

handle_info( _Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->    
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

