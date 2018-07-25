%% -*- mode:erlang -*-

-ifndef(HEADER_MY_APP_V4).
-define(HEADER_MY_APP_V4, true).


-record(message, {object :: term(),
                  type :: request | response,
                  flag :: 2#00100000 | 2#00010000,
                  tracking_id :: integer(),
                  actor :: module()}).

-define(NAME_TABLE, name_table).
-define(CODE_TABLE, code_table).

-ifdef(TEST).
-define(LOG_ERROR(Format, Args), ct:print(default, 50, Format, Args)).
-define(LOG_INFO(Format, Args), ?LOG_ERROR(Format, Args)).
-define(LOG_DEBUG(Format, Args), ?LOG_ERROR(Format, Args)).
-else.
-define(LOG_ERROR(Format, Args), lager:error(Format, Args)).
-define(LOG_INFO(Format, Args), lager:info(Format, Args)).
-define(LOG_DEBUG(Format, Args), lager:debug(Format, Args)).
-endif.

-endif.
