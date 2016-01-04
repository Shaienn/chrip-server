%%%-------------------------------------------------------------------
%%% @author shaienn
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Dec 2015 18:13
%%%-------------------------------------------------------------------
-module(songbase_handler).
-author("shaienn").

%% API
-export([init/2, content_types_provided/2, songbase_json/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, songbase_json}
  ], Req, State}.


songbase_json(Req, State)->
  Type = cowboy_req:binding(type, Req, <<"version">>),
  {ok, Answer} = case Type of
      <<"version">> ->
            {ok, _Version} = db:get_songbase_version();
      <<"link">> ->
            {ok, _Link} = db:get_songbase_link()
  end,
  Body=jiffy:encode({[{Type, Answer}]}),
  {Body, Req, State}.

