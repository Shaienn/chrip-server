%%%-------------------------------------------------------------------
%%% @author shaienn
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Jan 2016 13:56
%%%-------------------------------------------------------------------
-module(songs_handler).
-author("shaienn").

%% API
-export([init/2, content_types_provided/2, songs_json/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, songs_json}
  ], Req, State}.

songs_json(Req, State)->
  SingerId = cowboy_req:binding(aid, Req),
  SongId = cowboy_req:binding(sid, Req, list),
  Aid=binary_to_integer(SingerId),
  Convert=fun(List) -> jiffy:encode({List}) end,
  {ok, Answer} = case SongId of
                   list ->
                     db:get_songs_list_by_singer_id(Aid);
                   Binary ->
                     Sid = binary_to_integer(Binary),
                     db:get_song_by_singer_id_and_song_id(Aid, Sid)
                 end,
  JsonList = lists:map(Convert, Answer),
  {JsonList, Req, State}.

