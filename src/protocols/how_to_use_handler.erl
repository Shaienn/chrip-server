%%%-------------------------------------------------------------------
%%% @author shaienn
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Dec 2015 17:48
%%%-------------------------------------------------------------------
-module(how_to_use_handler).
-author("shaienn").

%% API
-export([init/2, content_types_provided/2, how_to_use_html/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
  {[
    {<<"text/html">>, how_to_use_html}
  ], Req, State}.

how_to_use_html(Req, State)->
  Body = <<"<html>
<head>
	<meta charset=\"utf-8\">
	<title>REST Hello World!</title>
</head>
<body>
	<p>REST Hello World as HTML!</p>
</body>
</html>">>,
  {Body, Req, State}.