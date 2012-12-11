-module(test_riak).

%% Riak exports
-export([connect/1, create/3, fetch/3, update/2, get_value/1, save/2]).

connect({IP, Port}) ->
  {ok, RiakPid} = riakc_pb_socket:start_link(IP, Port),
  RiakPid.

create(Bucket, Key, Item) ->
  RiakObj = riakc_obj:new(Bucket, Key, Item),
  RiakObj.

fetch(RiakPid, Bucket, Key) ->
  {ok,RiakObj} = riakc_pb_socket:get(RiakPid, Bucket, Key),
  RiakObj.

update(RiakObj, NewValue) ->
  NewRiakObj = riakc_obj:update_value(RiakObj, NewValue),
  NewRiakObj.

get_value(RiakObj) ->
  Value = riakc_obj:get_value(RiakObj),
  Value.

save(RiakPid, RiakObj) ->
  Result = riakc_pb_socket:put(RiakPid, RiakObj),
  Result.