-module(chrip_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

    ok = application:start(ranch),
    {ok, _} = ranch:start_listener(chrip_server, 1,
        ranch_tcp, [{port, 8888}],
        client_protocol, []),

    io:format("Start supervisor~n"),
    chrip_server_sup:start_link().

stop(_State) ->
    ok.
