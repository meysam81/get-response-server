-module(my_app_v4_utils).

-export([get/1]).

get(Key) ->
    case application:get_env(my_app_v4, Key) of
        {ok, Val} ->
            Val;
        undefined ->
            {error, no_such_value}
    end.
