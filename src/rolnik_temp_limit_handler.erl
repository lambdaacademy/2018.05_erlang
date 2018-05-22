-module(rolnik_temp_limit_handler).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
         terminate/2]).

-record(state, {current_sample_temps = [], current_temp_status = normal,
                upper_limit, lower_limit, checks_per_sample,
                status_return_limit}).

init([LowerLimit, UpperLimit, ChecksPerSample, StatusReturnLimit]) ->
    {ok, #state{lower_limit = LowerLimit, upper_limit = UpperLimit,
                checks_per_sample = ChecksPerSample,
                status_return_limit = StatusReturnLimit}}.

handle_event({_Time, Temp}, State = #state{current_temp_status = CurrStatus,
                                           current_sample_temps = Temps,
                                           checks_per_sample = ChecksPerSample})
  when length(Temps) == (ChecksPerSample - 1) ->
    UpdatedTemps = [Temp | Temps],
    NewTempStatus = new_temp_status(State, UpdatedTemps), 
    case make_decision(NewTempStatus, CurrStatus) of
        do_nothing -> ok;
        alarm -> send(NewTempStatus)
    end,
    NewState = State#state{current_sample_temps = [],
                           current_temp_status = NewTempStatus},
    {ok, NewState};
handle_event({_Time, Temp}, State = #state{current_sample_temps = Temps}) ->
    UpdatedTemps = [Temp | Temps],
    NewState = State#state{current_sample_temps = UpdatedTemps},
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

new_temp_status(#state{current_temp_status = CurrStatus,
                       lower_limit = LowerLimit, upper_limit = UpperLimit,
                       status_return_limit = StatusReturnLimit}, Temps) ->
    AvrgTemp = average(Temps),
    verify_average_temp(CurrStatus, AvrgTemp, LowerLimit, UpperLimit,
                        StatusReturnLimit).

average(List) ->
    lists:sum(List) / length(List).

verify_average_temp(normal, Temp, LowerLimit, _UpperLimit, _StatusReturnLimit)
  when Temp < LowerLimit ->
    below;
verify_average_temp(normal, Temp, _LowerLimit, UpperLimit, _StatusReturnLimit)
  when Temp > UpperLimit ->
    above;
verify_average_temp(below, Temp, LowerLimit, _UpperLimit, StatusReturnLimit)
  when Temp > (LowerLimit + StatusReturnLimit) ->
    normal;
verify_average_temp(above, Temp, _LowerLimit, UpperLimit, StatusReturnLimit) 
  when Temp < (UpperLimit - StatusReturnLimit) ->
    normal;
verify_average_temp(below, _Temp, _LowerLimit, _UpperLimit, _StatusReturnLimit) ->
    below;
verify_average_temp(above, _Temp, _LowerLimit, _UpperLimit, _StatusReturnLimit) ->
    above;
verify_average_temp(normal, _Temp, _LowerLimit, _UpperLimit, _StatusReturnLimit) ->
    normal.


make_decision(below, normal) ->
    alarm;
make_decision(above, normal) ->
    alarm;
make_decision(_NewStatus, _CurrStatus) ->
    do_nothing.

send(Status) ->
    ok = rolnik_notifier:alarm(Status).
