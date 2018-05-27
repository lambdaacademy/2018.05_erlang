% @doc rolnik top level supervisor.
% @end
-module(rolnik_sup).

-behavior(supervisor).

% API
-export([start_link/0]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------
%% @doc Starts rolnik top level supervisor.
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    %% start event handlers
    {ok, LowerLimit} = application:get_env(rolnik, lower_limit),
    {ok, UpperLimit} = application:get_env(rolnik, upper_limit),
    {ok, ChecksPerSample} = application:get_env(rolnik, checks_per_sample),
    {ok, StatusReturnLimit} = application:get_env(rolnik, normal_status_return_limit),
    ok = gen_event:add_handler(rolnik_temp_notify_manager,
                               rolnik_temp_limit_handler,
                               [LowerLimit, UpperLimit, ChecksPerSample,
                                StatusReturnLimit]),
    ok = gen_event:add_handler(rolnik_temp_notify_manager,
                               rolnik_temp_backup_handler, []),
    {ok, Pid}.

%--- Callbacks -----------------------------------------------------------------

init([]) ->
    {ok, SmtpConfig} = application:get_env(rolnik, smtp_config),
    {ok, Sender} = application:get_env(rolnik, alarm_sender_email),
    {ok, Receiver} = application:get_env(rolnik, alarm_receiver_email),
    {ok, TempCheckFreq} = application:get_env(rolnik, temp_check_freq),
    SensorId = get_sensor_id(),
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [
                  %% start rolnik_notifier
                  #{id => rolnik_notifier_id,
                    start => {rolnik_notifier, start_link, [[SmtpConfig,
                                                             Sender, Receiver]]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [rolnik_notifier]},
                  %% start event manager
                  {rolnik_temp_notify_manager, {gen_event, start_link,
                                        [{local, rolnik_temp_notify_manager}]},
                   permanent, 5000, worker, [dynamic]},
                  %% start rolnik_temp_checker
                  #{id => rolnik_temp_checker_id,
                    start => {rolnik_temp_checker, start_link, [[TempCheckFreq,
                                                                 SensorId]]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [rolnik_temp_checker]}
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%% Internal functions

get_sensor_id() ->
    [ID] = grisp_onewire:transaction(fun() -> grisp_onewire:search() end),
    ID.

