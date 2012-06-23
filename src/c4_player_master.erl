% @doc Handles the table of players registered to receive notifications
-module(c4_player_master).
-behaviour(gen_fsm).
% FSM callbacks
-export([init/1, handle_sync_event/4, handle_event/3, handle_info/3, loop/2, loop/3, terminate/3, code_change/4]).
% Public API
-export([start/0, start_link/0, stop/0, register_player/1, unregister_player/1, notify_seek_removed/1, notify_seek_issued/1]).
-include("c4_common.hrl").
-record(state, {parent}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
    
% @doc Starts a standalone c4_game_master process. Mostly for testing.
-spec(start() ->  {ok, pid()} | ignore | {error, binary()}).
start() ->
	gen_fsm:start({local, c4_player_master}, ?MODULE, self(), []).

% @doc Starts a c4_game_master process that is linked to the current
% process.
-spec(start_link() -> {ok, pid()} | ignore | {error, binary()}).
start_link() ->
	gen_fsm:start_link({local, c4_player_master}, ?MODULE, self(), []).

% @doc Stops the player master process
-spec(stop() -> ok).
stop() ->
	gen_fsm:sync_send_all_state_event(?MODULE, stop).

% @doc Registers a player so they can receive seek notifications
register_player(Pid) ->
	gen_fsm:sync_send_event(?MODULE, {register_player, Pid}).
	  
% @doc Registers a player so they can receive seek notifications
unregister_player(Pid) ->
	gen_fsm:sync_send_event(?MODULE, {unregister_player, Pid}).
	  
% @doc 
notify_seek_removed(GameId) ->
	gen_fsm:sync_send_event(?MODULE, {notify_seek_removed, GameId}).

notify_seek_issued(#seek{} = Seek) ->
	gen_fsm:sync_send_event(?MODULE, {notify_seek_issued, Seek}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FSM callbacks


init(ParentId) ->
	ets:new(c4_player_tbl, [named_table, private, set]),
	{ok, loop, #state{parent=ParentId}}.

terminate(_Reason, _Name, _State) ->
	ets:delete(c4_player_tbl),
	ok.

loop({register_player, Pid}, _From, Data) ->
	ets:insert(c4_player_tbl, {Pid}),
	{reply, ok, loop, Data};
loop({unregister_layer, Pid}, _From, Data) ->
	ets:delete(c4_player_tbl, Pid),
	{reply, ok, loop, Data};
loop({notify_seek_issued, Seek}, _From, Data) ->
	send_seek_notification({seek_issued, Seek}),
	{reply, ok, loop, Data};
loop({notify_seek_removed, SeekId}, _From, Data) ->
	send_all_players({seek_removed, SeekId}),
	{reply, ok, loop, Data}.

loop({notify_seek_removed, SeekId}, Data) ->
	send_all_players({seek_removed, SeekId}),
	{next_state, loop, Data}; 
loop({notify_seek_issued, Seek}, Data) ->
	send_seek_notification({seek_issued, Seek}),
	{next_state, loop, Data}. 

send_all_players(Msg) ->
	ets:foldl(fun({Pid}, []) -> Pid!Msg, [] end, [], c4_player_tbl),
	ok.

send_seek_notification({seek_issued, #seek{pid=SPid}} = Msg) ->
	ets:foldl(
		fun({Pid}, []) -> 
				case Pid of 
					SPid -> ok; 
					_ -> Pid!Msg 
				end, 
				[] 
		end, 
		[], 
		c4_player_tbl
		),
	ok.

handle_event(stop, Data, Data) ->
	{stop, normal, Data};
handle_event(Event, State, Data) ->
	unexpected(Event, State, Data).

handle_sync_event(stop, _From, Data, Data) ->
	{stop, normal, ok, Data};
handle_sync_event(Event, _From, State, Data) ->
	?log("Unexpected event ~w in ~w ~w~n", [Event, State, Data]),
	{reply, {error, "Unexpected Event"}, State, Data}.

handle_info(Event, State, Data) ->
	unexpected(Event, State, Data).

unexpected(Event, State, Data) ->
	?log("Unexpected event ~w in ~w ~w~n", [Event, State, Data]),
	{next_state, State, Data}.

code_change(_OldVsn, State, Data, _Extra) ->
	{ok, State, Data}.
