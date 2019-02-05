% @doc
% @end
-module(rolnik_example).

-import(rolnik_device, [device_id_onewire/0,
                        read_temperature_onewire/1]).

% API
-export([init/0]).


%--- API -----------------------------------------------------------------------

init() ->
    %% grisp_onewire:search/0 returns a list of IDs of devices connected via one wire,
    %% provided example assumes there is only one slave connected
    case device_id_onewire() of
        [ID] ->
            InitTemp = read_temperature_onewire(ID),
            loop(ID, InitTemp);
        _ ->
            grisp_led:color(1, blue),
            grisp_led:color(2, blue)
    end.


%--- Internal ------------------------------------------------------------------

loop(ID, Temp) ->
    timer:sleep(1000),
    try read_temperature_onewire(ID) of
        NewTemp  ->
            case abs(NewTemp - Temp) of
                Dif when Dif < 0.5 ->
                    grisp_led:color(1, green),
                    grisp_led:color(2, green);
                _ ->
                    grisp_led:color(1, red),
                    grisp_led:color(2, red)
            end,
            loop(ID, NewTemp)
    catch
        _:_ ->
            grisp_led:flash(1, red, 500),
            grisp_led:flash(2, yellow, 500)
    end.
