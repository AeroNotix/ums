-module(ums_server).

-export([identifier/0]).
-export([new_session_id/0]).


identifier() ->
    atom_to_binary(node(), latin1).

new_session_id() ->
    RawUUID = uuid:get_v4(),
    StringUUID = uuid:uuid_to_string(RawUUID),
    list_to_binary(StringUUID).
