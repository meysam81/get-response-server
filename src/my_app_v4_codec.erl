-module(my_app_v4_codec).

-export([encode_frame/1,
         deframe_decode/1,
         parse_buffer/2]).


-include("my_app_v4.hrl").
-include("my_app_v4_sample_func.hrl").
-include("my_app_v4_sample_type.hrl").


%% ---------------------------- encode frame --------------------------------
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




%% ---------------------------- decode frame --------------------------------
deframe_decode(<<_FrameLen:?FRAME_LEN_BYTE_SIZE/binary,
                 Code:?FRAME_CODE_BIT_SIZE/integer,
                 Flags:?FRAME_FLAGS_BIT_SIZE/integer,
                 Tracking_id:?FRAME_TRACKING_ID_BIT_SIZE/integer,
                 Payload/binary>>) ->

    case decode_object(Payload, Code) of
        {error, Reason} = Error ->
            ?LOG_ERROR("Payload: ~p, Reason: ~p",
                       [Payload, Reason]),
            Error;
        Object ->
            Type = case Flags of
                       2#00100000 -> request;
                       2#00010000 -> response;
                       _ -> unknown
                   end,
            #message{object = Object,
                     flags = Flags,
                     type = Type,
                     tracking_id = Tracking_id}
    end.



decode_object(Payload, Code) ->
    case my_app_v4_dict:get_by_code(Code) of
        {error, _Reason} = Error ->
            Error;
        [{_Code, Name, _Actor, Codec}] ->
            Codec:decode_msg(Payload, Name)
    end.





parse_buffer(OldBuffer, NewData) ->
    NewBuffer = <<OldBuffer/binary, NewData/binary>>,
    parse_buffer(#parsed_buffer{framed = <<>>, buffered = NewBuffer}).
parse_buffer(#parsed_buffer{buffered = <<>>} = Buffer) ->
    Buffer;
parse_buffer(#parsed_buffer{framed = <<>>, buffered = <<>>}) ->
    {malformed_frame, empty};
parse_buffer(#parsed_buffer{framed = Framed, buffered = Buffered} = Buffer) ->
    case get_frame(Buffered) of
        {complete_frame, NewFrame, <<>>} ->
            #parsed_buffer{framed = <<Framed/binary, NewFrame/binary>>,
                           buffered = <<>>};
        {complete_frame, NewFrame, NewBuffer} ->
            parse_buffer(#parsed_buffer
                         {framed = <<Framed/binary, NewFrame/binary>>,
                          buffered = NewBuffer});
        {incomplete_frame, Buffered} ->
            Buffer;
        {malformed_frame, too_large} = Malformed ->
            ?LOG_ERROR("frame is larger than ~p byte",
                       [?MAX_FRAME_BYTE_SIZE]),
            Malformed
    end.
get_frame(<<FrameLen:?FRAME_LEN_BIT_SIZE/integer, _/binary>> = Buffer) ->
    case byte_size(Buffer) of
        BufferLen when BufferLen >= ?MAX_FRAME_BYTE_SIZE ->
            {malformed_frame, too_large};
        BufferLen when BufferLen >= FrameLen ->
            <<NewFrame:FrameLen/binary, NewBuffer/binary>> = Buffer,
            {complete_frame, NewFrame, NewBuffer};
        BufferLen when BufferLen < FrameLen ->
            {incomplete_frame, Buffer}
    end.
