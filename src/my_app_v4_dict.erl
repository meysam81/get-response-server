-module(my_app_v4_dict).

-export([start/0]).

-export([init/0,
         get_by_code/1,
         get_by_name/1]).

-include("my_app_v4.hrl").

start() ->
    spawn(my_app_v4_dict, init, []).

init() ->
    case ets:info(?NAME_TABLE) of
        undefined ->
            ?NAME_TABLE = ets:new(?NAME_TABLE, [named_table, public]);
        _ ->
            ok
    end,
    case ets:info(?CODE_TABLE) of
        undefined ->
            ?CODE_TABLE = ets:new(?CODE_TABLE, [named_table, public]);
        _ ->
            ok
    end,

    [begin
         ets:insert(?NAME_TABLE, {Name, Code, Actor, Codec}),
         ets:insert(?CODE_TABLE, {Code, Name, Actor, Codec})
     end ||
        #{code := Code,
          name := Name,
          actor := Actor,
          codec := Codec} <- get_list()],
    ok.

get_list() ->
    [
      %% =============== funcs ==================
      #{name => 'my_app_v4.sample.func.Fact',
        code => 51,
        actor => my_app_v4_actor,
        codec => my_app_v4_sample_func},

      %% =============== types ==================
      #{name => 'my_app_v4.sample.type.FactResp',
        code => 1,
        actor => undefined,
        codec => my_app_v4_sample_type}
     ].

get_by_code(Code) ->
    case ets:lookup(?CODE_TABLE, Code) of
        [{_Code, _Name, _Actor, _Codec}] = Response ->
            Response;
        _ ->
            {error, undefined}
    end.
get_by_name(Name) ->
    case ets:lookup(?NAME_TABLE, Name) of
        [{_Code, _Name, _Actor, _Codec}] = Response ->
            Response;
        _ ->
            {error, undefined}
    end.
