%% @doc 
%% @end
-module(rolnik_notifier).

-behaviour(gen_server).

%% API
-export([start_link/1, alarm/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

-record(state, {socket_info, sender_email, receiver_email}).

%% API 

%% @doc Starts rolnik_notifier which handles sending alarm messages to client
%% when temperature status changes from normal  
-spec start_link(Args :: list()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Args)->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% @doc Starts rolnik_notifier which handles sending alarm messages to client
-spec alarm(Status :: atom()) -> ok.
alarm(Status) ->
    gen_server:call(?MODULE, {alarm, Status}).

%% gen_server callbacks

init([SmtpConfig, Sender, Receiver]) ->
    process_flag(trap_exit, true),
    SocketInfo = open_socket_for_notify(SmtpConfig),
    {ok, #state{socket_info = SocketInfo, sender_email = Sender,
                receiver_email = Receiver}}.

handle_call({alarm, below}, _From, State) ->
    Message = "Subject: Temperature Warning\r\nFrom: GRISP board \r\n
              Temperature dropped below the lower limit.",
    send(Message, State),
    {reply, ok, State};
handle_call({alarm, above}, _From, State) ->
    Message = "Subject: Temperature Warning\r\nFrom: GRISP board \r\n
              Temperature rised above the upper limit.",
    send(Message, State),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

open_socket_for_notify(SmtpConfig) ->
    {ok, SocketInfo} = gen_smtp_client:open(SmtpConfig),
    SocketInfo.

send(Message, #state{socket_info = SocketInfo, sender_email = Sender,
                     receiver_email = Receiver}) ->
    % Info backup to implement
    {ok, _Info} = gen_smtp_client:deliver(SocketInfo, {Sender, [Receiver],
                                                       Message}).
