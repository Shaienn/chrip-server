%%%-------------------------------------------------------------------
%%% @author shaienn
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2016 13:35
%%%-------------------------------------------------------------------
-module(approve_handler).
-author("shaienn").

%% API
-export([init/2,
  content_types_accepted/2,
  add_song_to_approve/2,
  allowed_methods/2,
  content_types_provided/2,
  add_song_confirm/2]).

allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

content_types_accepted(Req, State) ->
  {[
%%    {<<"application/json">>, add_song_to_approve}
    {{<<"application">>, <<"json">>, []}, add_song_to_approve}

  ], Req, State}.

add_song_to_approve(Req, State) ->
  {ok, Body, Req1} = cowboy_req:body(Req),
  {JsonContent} = jiffy:decode(Body),
  io:format("here: ~p~n", [JsonContent]),

  {ok, Id} = approve_db:add_song_for_approve(
    proplists:get_value(<<"id">>, JsonContent),
    proplists:get_value(<<"gaid">>, JsonContent),
    proplists:get_value(<<"uaid">>, JsonContent),
    proplists:get_value(<<"gsid">>, JsonContent),
    proplists:get_value(<<"usid">>, JsonContent),

    proplists:get_value(<<"singer_name">>, JsonContent),
    proplists:get_value(<<"song_name">>, JsonContent),
    proplists:get_value(<<"song_text">>, JsonContent),
    proplists:get_value(<<"mac">>, JsonContent),

    cowboy_req:host(Req1)
  ),

  Answer = jiffy:encode({[{<<"approve">>, Id}]}),
  io:format("A: ~p~n",[Answer]),
  Req2 = cowboy_req:set_resp_body(Answer, Req1),
  {true, Req2, State}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, add_song_confirm}
  ], Req, State}.

add_song_confirm(Req, State) ->
  {true, Req, State}.
