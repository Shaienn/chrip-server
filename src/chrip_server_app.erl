-module(chrip_server_app).

-behaviour(application).

-include("include/chrip_header.hrl").

%% Application callbacks

-export([start/2, stop/1]).
-export([start/0, stop/0]).

-define(APPS, [esqlite, crypto, ranch, cowlib, cowboy, chrip_server]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start()->
    ok = ensure_started(?APPS),
    ok = sync:go().

stop()->
    sync:stop(),
    ok = stop_apps(lists:reverse(?APPS)).


start(_StartType, _StartArgs) ->

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", how_to_use_handler, []},
            {"/songbase/[:type]", songbase_handler, []},
            {"/singers/[:aid]", singers_handler, []},
            {"/singers/:aid/songs/[:sid]", songs_handler, []},
            {"/global.db", cowboy_static, {file, ?SONGDBPATH}},
            {"/approve", approve_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
        {env, [{dispatch, Dispatch}]}
    ]),

    chrip_server_sup:start_link().

stop(_State) ->
    ok.

ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
    case application:start(App) of
        ok -> ensure_started(Apps);
        {error, {already_started, App}} -> ensure_started(Apps)
    end.

stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
    application:stop(App),
    stop_apps(Apps).
