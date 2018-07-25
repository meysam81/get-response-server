-module(my_app_v4_worker).

-behaviour(gen_server).


-include("my_app_v4.hrl").


%% API
-export([start/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2,
         code_change/3,
         format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {socket, transport, buffer}).
%%%===================================================================
%%% API
%%%===================================================================
start(Transport, Socket, Opts) ->
    init([Transport, Socket, Opts]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Transport, Socket, _Opts]) ->
    process_flag(trap_exit, true),

    proc_lib:init_ack({ok, self()}),


    State = #state{socket = Socket,
                   transport = Transport,
                   buffer = <<>>},


    gen_server:enter_loop(?MODULE, [], State),

    {ok, State}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(#message{} = Message, #state{transport = Transport,
                                         socket = Socket} = State) ->
    {ok, FramedMsg} = my_app_v4_codec:encode_frame(Message),
    Transport:send(Socket, FramedMsg),
    {noreply, State};
handle_cast(Request, State) ->
    ?LOG_DEBUG("Request: ~p", [Request]),
    {noreply, State}.


handle_info({tcp, Socket, Data}, #state{socket = Socket,
                                        buffer = Buffer} = State) ->
    case my_app_v4_codec:parse_buffer(Buffer, Data) of
        #parsed_buffer{framed = Framed, buffered = Buffered} ->
            Msgs = [my_app_v4_codec:deframe_decode(Frame) ||
                       Frame <- Framed],
            [dispatch_messages(Msg#message{socket_pid = self()})
                               || Msg <- Msgs],
            {noreply, State#state{buffer = Buffered}};
        _ ->
            {noreply, State}
    end;
handle_info(Info, State) ->
    ?LOG_INFO("Info: ~p", [Info]),
    {noreply, State}.




terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
dispatch_messages(#message{actor = Actor} = Msg) ->
    Actor:start(Msg);
dispatch_messages(_) ->
    false.
