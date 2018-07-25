-module(my_app_v4_actor).

-export([start/1, init/0]).
-include("my_app_v4.hrl").
-include("my_app_v4_sample_func.hrl").
-include("my_app_v4_sample_type.hrl").

start(Msg) ->
    Pid = spawn(my_app_v4_actor, init, []),
    Pid ! Msg.

init() ->
    receive
        #message{object = Object,
                 socket_pid = Socket_pid} = Msg->
            case Object of
                #'my_app_v4.sample.func.Fact'{x = X} ->
                    Response = #'my_app_v4.sample.type.FactResp'
                        {y = calc_fact(X)},
                    NewMsg = Msg#message{object = Response,
                                         type = response,
                                         flags = 2#00010000},
                    gen_server:cast(Socket_pid, NewMsg),
                    ok;
                _ ->
                    false
            end
    end.
calc_fact(1) ->
    1;
calc_fact(N) when N > 1->
    calc_fact(N - 1) * N.
