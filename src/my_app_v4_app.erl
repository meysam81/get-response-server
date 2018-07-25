-module(my_app_v4_app).

-behaviour(application).

-include("my_app_v4.hrl").
-include("my_app_v4_sample_func.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    my_app_v4_dict:init(), %% dictionary initialization

    Port = my_app_v4_utils:get(port),

    ok = application:ensure_started(tecipe),

    {ok, _} = tecipe:start_listener(tecipe_ref,
                                    Port,
                                    {my_app_v4_worker, start, []},
                                    [{monitor, true},
                                     {pool, 8}],
                                    [binary,
                                     {keepalive, false},
                                     {backlog, 256},
                                     {reuseaddr, true}]),


    my_app_v4_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
