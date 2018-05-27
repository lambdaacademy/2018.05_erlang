-module(rolnik_temp_backup_handler).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
         terminate/2]).

-record(state, {temps = []}).

init([]) ->
    {ok, #state{}}.

handle_event(TimeTempData, #state{temps = TempsBackup}) ->
    UpdatedTempsBackup = [TimeTempData | TempsBackup],
    NewState = #state{temps = UpdatedTempsBackup},
    {ok, NewState}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% Internal functions
