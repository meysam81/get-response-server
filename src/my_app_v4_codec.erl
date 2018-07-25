-module(my_app_v4_codec).

-export([encode_frame/1]).

-include("my_app_v4.hrl").
-include("my_app_v4_sample_func.hrl").
-include("my_app_v4_sample_type.hrl").

encode_frame(#message{object = Object,
                      tracking_id = Tracking_id,
                      flags = Flags}) ->
    case encode_object(Object) of

        {error, Reason} = Error ->
            ?LOG_ERROR("Object: ~p, Reason: ~p",
                       [Object, Reason]),
            Error;


        {Payload, Code} ->
            ObjectLen = byte_size(Payload),
            FrameLen = ?FRAME_HEADER_BYTE_SIZE + ObjectLen,


            FrameHeader = <<FrameLen:?FRAME_LEN_BIT_SIZE,
                            Code:?FRAME_CODE_BIT_SIZE,
                            Flags:?FRAME_FLAGS_BIT_SIZE,
                            Tracking_id:?FRAME_TRACKING_ID_BIT_SIZE>>,


            Frame = <<FrameHeader/binary, Payload/binary>>,

            {ok, Frame}

    end.

encode_object(Object) ->
    ObjectName = element(1, Object),
    case my_app_v4_dict:get_by_name(ObjectName) of
        {error, _Reason} = Error ->
            Error;
        [{_Name, Code, _Actor, Codec}] ->
            {Codec:encode_msg(Object), Code}
    end.
