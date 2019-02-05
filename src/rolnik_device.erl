% @doc
% @end
-module(rolnik_device).

% API
% ds18b20 termometer - connected via one wire
-export([read_temperature_onewire/1, device_id_onewire/0]).
% pmod hygro - connected via i2c
-export([read_temperature_i2c/0, read_humidity_i2c/0]).


%--- API -----------------------------------------------------------------------

-spec device_id_onewire() -> [binary()].
device_id_onewire() ->
    grisp_onewire:transaction(fun() -> grisp_onewire:search() end).

-spec read_temperature_onewire(ID :: binary()) -> float().
read_temperature_onewire(ID) ->
    %% ds18b20 is a digital thermomether
    onewire_ds18b20:convert(ID, 500),
    onewire_ds18b20:temp(ID).

read_temperature_i2c() ->
    grisp_i2c:msgs([16#40, {write, <<16#00>>}]),
    timer:sleep(20),
    T = grisp_i2c:msgs([16#40, {read, 2, 16#0800}]),
    <<Temp:14/unsigned-big,_:2>> = T,
    (Temp / 16384) * 165 - 40.

read_humidity_i2c() ->
    grisp_i2c:msgs([16#40, {write, <<16#01>>}]),
    timer:sleep(20),
    H = grisp_i2c:msgs([16#40, {read, 2, 16#0800}]),
    <<Hum:14/unsigned-big,_:2>> = H,
    (Hum / 16384) * 100.
