%%%-------------------------------------------------------------------
%%% @author shaienn
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Jan 2016 11:24
%%%-------------------------------------------------------------------
-module(db).
-author("shaienn").

-behaviour(gen_server).

-include("include/chrip_header.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([get_songbase_version/0,
  get_songbase_link/0,
  get_singers_list/0,
  get_singer_by_id/1,
  get_songs_list_by_singer_id/1,
  get_song_by_singer_id_and_song_id/2]).

-define(SERVER, ?MODULE).

-record(state, {connection=null, db_version=0, db_version_timer=null}).

%%%===================================================================
%%% API
%%%===================================================================

get_songbase_version() ->
    gen_server:call(?MODULE, get_songbase_version, infinity).

get_songbase_link() ->
  gen_server:call(?MODULE, get_songbase_link, infinity).

get_singers_list() ->
  gen_server:call(?MODULE, get_singers_list, infinity).

get_singer_by_id(Aid) ->
  gen_server:call(?MODULE, {get_singer_by_id, Aid}, infinity).

get_songs_list_by_singer_id(Aid) ->
  gen_server:call(?MODULE, {get_songs_list_by_singer_id, Aid}, infinity).

get_song_by_singer_id_and_song_id(Aid, Sid) ->
  gen_server:call(?MODULE, {get_song_by_singer_id_and_song_id, Aid, Sid}, infinity).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, Connection} = esqlite3:open(?SONGDBPATH, {readonly}, 5000),
  [{Version}] = esqlite3:q("select version from Version where version_id=1", Connection),
  error_logger:info_msg("Song base opened: ~p, Version:~p~n", [Connection, Version]),
  Timer = erlang:send_after(?UPDATE_VERSION_INTERVAL, self(), update_version),
  {ok, #state{connection = Connection, db_version = Version, db_version_timer = Timer}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).


handle_call(get_songbase_link, _From, State) ->
  {reply, {ok, <<"/global.db">>}, State};

handle_call(get_songbase_version, _From, State) ->
  {reply, {ok, State#state.db_version}, State};

handle_call(get_singers_list, _From, State) ->
  {ok, Statement} = esqlite3:prepare("select name, author_id from Authors", State#state.connection),
  Assoc = fun(Names, Row) -> lists:zip(tuple_to_list(Names), tuple_to_list(Row)) end,
  SingerList = esqlite3:map_s(Assoc, Statement),
  io:format("List: ~p~n", [SingerList]),
  {reply, {ok, SingerList}, State};

handle_call({get_singer_by_id, Aid}, _From, State) ->
  {ok, Statement} = esqlite3:prepare("select name, author_id from Authors where author_id=?", State#state.connection),
  esqlite3:bind(Statement,[Aid]),
  Assoc = fun(Names, Row) -> lists:zip(tuple_to_list(Names), tuple_to_list(Row)) end,
  Singer = esqlite3:map_s(Assoc, Statement),
  io:format("Singer: ~p~n", [Singer]),
  {reply, {ok, Singer}, State};

handle_call({get_songs_list_by_singer_id, Aid}, _From, State) ->
  {ok, Statement} = esqlite3:prepare("select name, song_id, author_id from Songs where author_id=?", State#state.connection),
  esqlite3:bind(Statement,[Aid]),
  Assoc = fun(Names, Row) -> lists:zip(tuple_to_list(Names), tuple_to_list(Row)) end,
  Songs = esqlite3:map_s(Assoc, Statement),
  io:format("Songs: ~p~n", [Songs]),
  {reply, {ok, Songs}, State};

handle_call({get_song_by_singer_id_and_song_id, Aid, Sid}, _From, State) ->
  {ok, Statement} = esqlite3:prepare("select * from Songs where author_id=? and song_id=?", State#state.connection),
  esqlite3:bind(Statement,[Aid, Sid]),
  Assoc = fun(Names, Row) -> lists:zip(tuple_to_list(Names), tuple_to_list(Row)) end,
  Song = esqlite3:map_s(Assoc, Statement),
  io:format("Song: ~p~n", [Song]),
  {reply, {ok, Song}, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).


handle_info(update_version, State) ->
  io:format("update version~n"),
  erlang:cancel_timer(State#state.db_version_timer),
  [{Version}] = esqlite3:q("select version from Version where version_id=1", State#state.connection),
  Timer = erlang:send_after(?UPDATE_VERSION_INTERVAL, self(), update_version),
  {noreply, State#state{db_version = Version, db_version_timer = Timer}};

handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
