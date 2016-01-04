%%%-------------------------------------------------------------------
%%% @author shaienn
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2016 12:04
%%%-------------------------------------------------------------------
-module(approve_db).
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

-export([add_song_for_approve/10]).

-define(SERVER, ?MODULE).

-record(state, {connection = null}).

%%%===================================================================
%%% API
%%%===================================================================

add_song_for_approve(Id, Gaid, Uaid, Gsid, Usid, SingerName, SongName, SongText, Mac, Ip) ->
  gen_server:call(?MODULE, {add_song_for_approve, Id, Gaid, Uaid, Gsid, Usid, SingerName, SongName, SongText, Mac, Ip}, infinity).


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
  io:format("approve init started~n"),
  {ok, Connection} = case esqlite3:open(?APPROVEDBPATH, {readwrite}, 5000) of
                       {ok, Conn} -> {ok, Conn};
                       {error, {_, "unable to open database file"}} ->
                         {ok, Conn} = esqlite3:open(?APPROVEDBPATH, 5000),
                         ok = esqlite3:exec("begin;", Conn),
                         ok = esqlite3:exec("create table need_approve(id INTEGER PRIMARY KEY AUTOINCREMENT, gaid INT, uaid INT, gsid INT, usid INT, singer_name VARCHAR(255), song_name VARCHAR(255), song_text TEXT, mac VARCHAR(18), ip VARCHAR(16));",
                           Conn),
                         ok = esqlite3:exec("commit;", Conn),
                         {ok, Conn}
                     end,

  error_logger:info_msg("Approve base opened: ~p~n", [Connection]),
  {ok, #state{connection = Connection}}.

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

handle_call({add_song_for_approve, Id, Gaid, Uaid, Gsid, Usid, SingerName, SongName, SongText, Mac, Ip}, _From, State) ->
  case esqlite3:q("SELECT id FROM need_approve WHERE gaid=? AND uaid=? AND gsid=? AND usid=? AND mac=?",
    [Gaid, Uaid, Gsid, Usid, Mac],
    State#state.connection) of
    [] ->
      io:format("non existed~n"),
      {ok, Statement} = esqlite3:prepare(
        "INSERT INTO need_approve (gaid, uaid, gsid, usid, singer_name, song_name, song_text, mac, ip) VALUES(?,?,?,?,?,?,?,?,?)",
        State#state.connection),
      esqlite3:bind(Statement, [Gaid, Uaid, Gsid, Usid, SingerName, SongName, SongText, Mac, Ip]),
      '$done' = esqlite3:step(Statement);
    [{RowID}] ->
      io:format("existed~n"),
      {ok, Statement} = esqlite3:prepare(
        "UPDATE need_approve SET singer_name=?, song_name=?, song_text=? WHERE id=?",
        State#state.connection),
      esqlite3:bind(Statement, [SingerName, SongName, SongText, RowID]),
      '$done' = esqlite3:step(Statement)
  end,
{reply, {ok, Id}, State};

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
