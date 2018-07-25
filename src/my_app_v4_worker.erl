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
    gen_server:start_link(?MODULE, [[Transport, Socket, Opts]], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Transport, Socket, _Opts]) ->
    State = #state{socket = Socket,
                   transport = Transport,
                   buffer = <<>>},
    {ok, State}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
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
