%% -*- mode:erlang -*-

-ifndef(HEADER_MY_APP_V4).
-define(HEADER_MY_APP_V4, true).


-record(message, {object :: term(),
                  type :: request | response,
                  flags :: 2#00100000 | 2#00010000,
                  tracking_id :: integer(),
                  socket_pid :: pid(),
                  actor :: module()}).
-record(parsed_buffer, {framed, buffered}).



-define(FRAME_HEADER_BYTE_SIZE, 11).
-define(MAX_FRAME_BYTE_SIZE, 65535).

-define(FRAME_LEN_BYTE_SIZE, 4).
-define(FRAME_LEN_BIT_SIZE, 32).

-define(FRAME_CODE_BYTE_SIZE, 2).
-define(FRAME_CODE_BIT_SIZE, 16).

-define(FRAME_FLAGS_BYTE_SIZE, 1).
-define(FRAME_FLAGS_BIT_SIZE, 8).

-define(FRAME_TRACKING_ID_BYTE_SIZE, 4).
-define(FRAME_TRACKING_ID_BIT_SIZE, 32).

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
