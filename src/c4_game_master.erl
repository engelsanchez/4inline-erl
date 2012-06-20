% @doc Coordinates the creation of games between pairs of players.
% It mostly sits in a loop receiving join requests.  When the first
% join request comes, the player is told to wait for another. When the
% next one comes, the pair are put into a newly created game and
% the process repeats over and over.
-module(c4_game_master).
-behaviour(gen_fsm).
-export([init/1, handle_sync_event/4, handle_event/3, handle_info/3, loop/3, terminate/3, code_change/4]).
-export([start/0, start_link/0, seek/4, cancel_seek/1, register_player/1]).
-record(state, {seeks, seekers, parent, seed=none}).
-include("c4_common.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API

% @doc Starts a standalone c4_game_master process. Mostly for testing.
-spec(start() ->  {ok, pid()} | ignore | {error, binary()}).
start() ->
	gen_fsm:start({local, c4_game_master}, ?MODULE, self(), []).

% @doc Starts a c4_game_master process that is linked to the current
% process.
-spec(start_link() -> {ok, pid()} | ignore | {error, binary()}).
start_link() ->
	gen_fsm:start_link({local, c4_game_master}, ?MODULE, self(), []).

% @doc Registers a player so they can receive seeks
register_player(Pid) ->
	gen_fsm:sync_send_event(?MODULE, {register_player, Pid}).

% @doc Call when a player requests to join a game. 
-spec(seek(pid(), game_type(), game_var(), #board_size{}) -> seek_pending | {new_game, pid(), pos_integer()}).
seek(Pid, Type, Var, BoardSize) ->
	gen_fsm:sync_send_event(?MODULE, {seek, Pid, Type, Var, BoardSize}).

% @doc Call when a player requests to cancel a join request.
-spec(cancel_seek(pid()) -> seek_canceled | no_seek_pending).
cancel_seek(Pid) ->
	gen_fsm:sync_send_event(?MODULE, {cancel_seek, Pid}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FSM callbacks

% @doc Starts trapping exits, creates player and game tables
% and starts FSM in loop state.
% Callback method for the OTP gen_fsm behavior.
init(ParentPid)->
	?log("Starting~n", []),
	process_flag(trap_exit, true),
	ets:new(c4_player_tbl, [named_table, private, set]),
	ets:new(c4_game_tbl, [named_table, private, set]),
	Seeks = dict:new(),
	Seekers = dict:new(),
	{ok, loop, #state{seeks=Seeks, seekers=Seekers, parent=ParentPid, seed=now()}}.

% @doc Deletes player and game tables upon termination.
% Callback function for the OTP gen_fsm behavior.
terminate(_Reason, _Name, _Data) ->
	ets:delete(c4_player_tbl),
	ets:delete(c4_game_tbl),
	ok.

% @doc The main 'loop' state of this FSM handles all messages 
% (seeks, seek cancellations, player registrations ).
loop({seek, Pid, anon, Var, BoardSize}, _From, #state{seeks=Seeks, seekers=Seekers, seed=Seed} = State) ->
	Key = {Var, BoardSize},
	?log("Processing anonymous game seek ~w~n", [Key]),
	% Match with currents seeks. Start game if matched, add to seeks if not
	case dict:find(Key, Seeks) of
		{ok, OPid}  ->
			?log("Seek has match. Starting new game~n", []),
			{ok, GamePid} = c4_game:start_link({OPid, Pid, BoardSize, Var}),
			{GameId, Seed2} = next_game_id(Seed),
			{PlayerId, Seed3} = random:uniform_s(?MAX_PLAYER_ID, Seed2),
			{OPlayerId, Seed4} = random:uniform_s(?MAX_PLAYER_ID, Seed3),
			c4_player:joined(OPid, GamePid, GameId, OPlayerId, your_turn),
			ets:insert(c4_game_tbl, {GameId, GamePid}),
			Seeks2 = dict:erase(Key, Seeks),
			Seekers2 = dict:erase(OPid, Seekers),
			{reply, {new_game, GamePid, PlayerId, other_turn}, loop, State#state{seeks=Seeks2, seekers=Seekers2, seed=Seed4}};
		error ->
			?log("No match. Seek -> Pending ~n", []),
			Seeks2 = dict:store(Key, Pid, Seeks),
			Seekers2 = dict:store(Pid, Key, Seekers),
			{reply, seek_pending, loop, State#state{seeks=Seeks2, seekers=Seekers2}}
	end;
loop({seek, Pid, priv, Var, BoardSize}, _From, #state{seed=Seed} = State) ->
	Key = {Var, BoardSize},
	?log("Processing private game seek ~w~n", [Key]),
	% Add to list of private games (to be started).
	{GameId, Seed2} = next_game_id(Seed),
	ets:insert(c4_game_tbl, {GameId, Var, BoardSize, Pid}),
	{reply, {new_game, wait}, loop, State#state{seed=Seed2}};
loop({cancel_seek, Pid}, _From, #state{seeks=Seeks, seekers=Seekers} = State) when is_pid(Pid) ->
	% @todo This assumes a single seek per client, which should soon change so we'll need
	% to match the exact seek being cancelled or cancel all
	case dict:find(Pid, Seekers) of
		{ok, Key} -> 
			Seeks2 = dict:erase(Key, Seeks),
			Seekers2 = dict:erase(Pid, Seekers),
			{reply, seek_canceled, loop, State#state{seeks=Seeks2, seekers=Seekers2}};
		error ->
			{reply, no_seek_pending, loop, State}
	end;
% Request to join a pending game
loop({join_game, PlayerPid, GameId}, _From, State) ->
	% If found, notify any player already in the game.
	case ets:lookup(c4_game_tbl, GameId) of
		[{GameId, GameVar, BoardSize, Other}] -> 
			{ok, GamePid} = c4_game:start_link({Other, PlayerPid, BoardSize, GameVar}),
			c4_player:joined(Other, GamePid, your_turn),
			ets:delete(c4_game_tbl, GameId),
			{reply, {new_game, GamePid, GameId, other_turn}, loop, State};
		[] -> {reply, no_game, look, State}
	end;
loop({register_player, Pid}, _From, State) when is_pid(Pid) ->
	ets:insert(c4_layer_tbl, {Pid, Pid}),
	erlang:monitor(process, Pid),
	{reply, player_registered, loop, State}.

% @doc Handles game processes ending and users disconnecting.
% Callback function for miscellaneous messages received by the FSM process.
handle_info({'EXIT', Pid, _Reason}, _Name, #state{parent=Pid} = State) ->
	{stop, parent_died, State};
handle_info({'EXIT', _Pid, _Reason}, Name, State) ->
	{next_state, Name, State};
% On user disconnect, remove from player and seeks data.
handle_info({'DOWN', _Ref, process, P1, _Reason}, Name, #state{seeks=Seeks, seekers=Seekers} = State) ->
	ets:delete(c4_player_tbl, P1),
	case dict:find(P1, Seekers) of
		{ok, GameSpec}  -> 
			Seeks2 = dict:erase(GameSpec, Seeks),
			Seekers2 = dict:erase(P1, Seekers),
			{next_state, Name, State#state{seeks=Seeks2, seekers=Seekers2}};
		errors -> 
			{next_state, Name, State}
	end.

handle_event(Event, StateName, State) ->
	?log("Unexpected event ~w in state ~w (~w)~n", [Event, StateName, State]),
	{reply, {error, unexpected, "Unexpected message"}, StateName, State}.

handle_sync_event(Event, _From, StateName, State) ->
	?log("Unexpected event ~w in state ~w (~w)~n", [Event, StateName, State]),
	{reply, {error, unexpected, "Unexpected message"}, StateName, State}.

% @doc Hot code swap callback (does nothing now).
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

-spec(next_game_id({integer(), integer(), integer()}) -> {pos_integer(), {integer(), integer(), integer()}}).
next_game_id(Seed) ->
	{GameId, Seed2} = random:uniform_s(?MAX_GAME_ID, Seed),
	% If in use, try again.
	case ets:member(c4_game_tbl, GameId) of
		true -> next_game_id(Seed2);
		false -> {GameId, Seed2}
	end.
