-module(my_app_v4_codec).

-include("my_app_v4.hrl").

encode_frame(#message{object = Object,
                      tracking_id = Tracking_id} = Message) ->
    ok.
