%% @doc 
%% @end
-module(rolnik_temp_checker).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {temp_check_freq, sensor_id}).

%% API 

%% @doc Starts rolnik_temp_checker which notifies rolnik_temp_notify_manager
%% about temperature with given frequency 
-spec start_link(Args :: list()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% gen_server callbacks

init([TempCheckFreq, SensorId]) ->
    process_flag(trap_exit, true),
    %% Request first update after giving time for whole app to start
    ?MODULE ! temp_update,
    ok = led_start_notify(),
    {ok, #state{temp_check_freq = TempCheckFreq, sensor_id = SensorId}}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(temp_update, State = #state{temp_check_freq = Lag, sensor_id = Id}) ->
    ok = request_temp_update_after_lag(Lag),
    {ok, NewTemp} = get_temp(Id),
    ok = temp_notify(NewTemp),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
led_start_notify() ->
    grisp_led:color(1, aqua),
    ok.

request_temp_update_after_lag(Lag) ->
    erlang:send_after(Lag, ?MODULE, temp_update),
    ok.

get_temp(Id) ->
    ok = onewire_ds18b20:convert(Id, 500),
    Temp = onewire_ds18b20:temp(Id),
    {ok, Temp}.

temp_notify(Temp) ->
    Time = erlang:localtime(),
    TimeTempData = {Time, Temp},
    gen_event:notify(rolnik_temp_notify_manager, TimeTempData),
    ok.
