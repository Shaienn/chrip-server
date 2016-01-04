%%%-------------------------------------------------------------------
%%% @author shaienn
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Dec 2015 18:13
%%%-------------------------------------------------------------------
-module(singers_handler).
-author("shaienn").

%% API
-export([init/2, content_types_provided/2, songbase_json/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, songbase_json}
  ], Req, State}.

songbase_json(Req, State) ->
  Type = cowboy_req:binding(aid, Req, list),
  Convert = fun(List) -> jiffy:encode({List}) end,
  {ok, Answer} = case Type of
                   list ->
                     db:get_singers_list();
                   Binary ->
                     Id = binary_to_integer(Binary),
                     db:get_singer_by_id(Id)
                 end,
  JsonList = lists:map(Convert, Answer),
  {JsonList, Req, State}.