% @doc Coordinates the creation of games between pairs of players.
% It mostly sits in a loop receiving join requests.  When the first
% join request comes, the player is told to wait for another. When the
% next one comes, the pair are put into a newly created game and
% the process repeats over and over.
-module(c4_game_master).
-behaviour(gen_server).
% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
% Public API
-export([start/0, start_link/0, stop/0,
		 seek/1, cancel_seek/1, cancel_seek/2, accept_seek/1,
		 game_finished/1, 
		 register_for_seeks/1, seek_list/0, game_list/0]).
-include("c4_common.hrl").
-record(state, {seeks, seeks_by_player, seeks_by_id, parent, seed=now()}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API

% @doc Starts a standalone c4_game_master process. Mostly for testing.
-spec(start() ->  {ok, pid()} | ignore | {error, binary()}).
start() ->
	gen_server:start({local, c4_game_master}, ?MODULE, self(), []).

% @doc Starts a c4_game_master process that is linked to the current
% process.
-spec(start_link() -> {ok, pid()} | ignore | {error, binary()}).
start_link() ->
	gen_server:start_link({local, c4_game_master}, ?MODULE, self(), []).

% @doc Call when a player requests to join a game. 
-spec(seek(#seek{}) -> {seek_pending, seek_id()} | {duplicate_seek, seek_id()} | {new_game, #game_info{}, turn(), 1|2 }).
seek(Seek) ->
	gen_server:call(?MODULE, {seek, Seek}, ?INTERNAL_TIMEOUT).

% @doc Call when a player requests to cancel all pending seeks.
-spec(cancel_seek(pid()) -> seek_canceled | no_seek_found).
cancel_seek(Pid) ->
	gen_server:call(?MODULE, {cancel_seek, Pid}, ?INTERNAL_TIMEOUT).

% @doc Call when a player requests to cancel all pending seeks.
-spec(cancel_seek(pid(), seek_id()) -> seek_canceled | no_seek_found).
cancel_seek(Pid, SeekId) ->
	gen_server:call(?MODULE, {cancel_seek, Pid, SeekId}, ?INTERNAL_TIMEOUT).

% @doc User request to accept a pending seek.
-spec(accept_seek(pos_integer()) -> ok | seek_not_found).
accept_seek(SeekId) ->
	gen_server:call(?MODULE, {accept_seek, SeekId}, ?INTERNAL_TIMEOUT).

% @doc Returns a list with info for all current games.
game_list() ->
	gen_server:call(?MODULE, game_list, ?INTERNAL_TIMEOUT).

% @doc Removes game from our database
game_finished(GamePid) ->
	gen_server:call(?MODULE, {game_finished, GamePid}, ?INTERNAL_TIMEOUT).

% @doc Returns the list of all pending seeks.
seek_list() ->
	gen_server:call(?MODULE, seek_list, ?INTERNAL_TIMEOUT).

% @doc Registers a user to receive seek notifications and returns the current list of seeks.
register_for_seeks(PlayerPid) ->
	gen_server:call(?MODULE, {register_for_seeks, PlayerPid}, ?INTERNAL_TIMEOUT).

% @doc Requests the game master process to stop.
-spec(stop() -> ok).
stop() ->
	gen_server:call(?MODULE, stop, ?INTERNAL_TIMEOUT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks

% @doc Starts trapping exits, creates player and game tables
% and starts FSM in loop state.
% Callback method for the OTP gen_fsm behavior.
init(ParentPid)->
	?log("Starting", []),
	process_flag(trap_exit, true),
	ets:new(c4_game_id_tbl, [named_table, private, set]),
	ets:new(c4_game_pid_tbl, [named_table, private, set]),
	Seeks = dict:new(),
	SeeksByPlayer= dict:new(),
	SeeksById = dict:new(),
	erlang:start_timer(?LOG_STATE_INTERVAL, self(), log_state),
	{ok, #state{seeks=Seeks, seeks_by_player=SeeksByPlayer, seeks_by_id=SeeksById, parent=ParentPid, seed=now()}}.

% @doc Deletes player and game tables upon termination.
% Callback function for the OTP gen_fsm behavior.
terminate(_Reason, _Data) ->
	ets:delete(c4_game_id_tbl),
	ets:delete(c4_game_pid_tbl),
	ok.

% @doc The main 'loop' state of this FSM handles all messages 
% (seeks, seek cancellations, player registrations ).
handle_call({seek, #seek{pid=Pid, type=anon, variant=Var, board_size=BoardSize} = Seek}, 
			_From, 
			#state{seeks=Seeks, seeks_by_player=SeeksByPlayer, seeks_by_id=SeeksById, seed=Seed} = State) ->
	% Do not use issuer pid when looking up seeks
	Key = Seek#seek{pid=none},
	?log("Processing anonymous game seek ~w ~w ~w", [Seek, Key, State]),
	% Match with currents seeks. Start game if matched, add to seeks if not
	case dict:find(Key, Seeks) of
		{ok, SeekId} ->
			case dict:find(SeekId, SeeksById) of  
				{ok, #seek{pid=Pid}} ->
					?log("Seek ~w is a duplicate", [SeekId]),
					{reply, {duplicate_seek, SeekId}, State}; 
				{ok, #seek{pid=OPid}}  ->
					?log("Seek has match. Starting new game", []),
					{ok, GamePid} = c4_game:start_link(#game_info{ppid1=OPid, ppid2=Pid, board_size=BoardSize, variant=Var, type=anon}),
					c4_player:joined(OPid, #game_info{pid=GamePid, id=SeekId, board_size=BoardSize, variant=Var, type=anon}, your_turn, 1),
					ets:insert(c4_game_id_tbl, {SeekId, GamePid}),
					ets:insert(c4_game_pid_tbl, {GamePid, SeekId}),
					c4_player_master:unregister_player(OPid),
					c4_player_master:unregister_player(Pid),
					c4_player_master:notify_seek_removed(SeekId, Pid, OPid),
					Seeks2 = dict:erase(Key, Seeks),
					SeeksById2 = dict:erase(SeekId, SeeksById),
					{ok, PlayerSeeks} = dict:find(OPid, SeeksByPlayer),
					case lists:delete(SeekId, PlayerSeeks) of
						[] -> SeeksByPlayer2 = dict:erase(OPid, SeeksByPlayer);
						NewSBPList -> SeeksByPlayer2 = dict:store(OPid, NewSBPList, SeeksByPlayer)
					end,
					NewState = State#state{seeks=Seeks2, seeks_by_id=SeeksById2, seeks_by_player=SeeksByPlayer2},
					%?log("State ~w", [NewState]),
					{
						reply,
						{new_game, #game_info{pid=GamePid, id=SeekId, board_size=BoardSize, variant=Var, type=anon}, other_turn, 2}, 
						NewState
					}
			end;
		error ->
			?log("No match. Seek -> Pending ", []),
			{SeekId, Seed2} = next_game_id(Seed),
			NewSeek = Seek#seek{id=SeekId},
			Seeks2 = dict:store(Key, SeekId, Seeks),
			SeeksByPlayer2 = dict:append(Pid, SeekId, SeeksByPlayer),
			SeeksById2 = dict:store(SeekId, NewSeek, SeeksById),
			c4_player_master:notify_seek_issued(NewSeek),
			NewState = State#state{seeks=Seeks2, seeks_by_player=SeeksByPlayer2, seeks_by_id=SeeksById2, seed=Seed2},
			%?log("State ~w", [NewState]),
			{reply, {seek_pending, SeekId}, NewState}
	end;
% Process play with friend request (private seek).
handle_call({seek, #seek{pid=Pid, type=priv, variant=Var, board_size=BoardSize} = Seek}, _From, #state{seed=Seed} = State) ->
	?log("Processing private game seek ~w", [Seek]),
	% Add to list of private games (to be started).
	{GameId, Seed2} = next_game_id(Seed),
	GameInfo = #game_info{variant=Var, board_size=BoardSize, ppid1=Pid, type=priv},
	ets:insert(c4_game_id_tbl, {GameId, GameInfo}),
	{reply, {new_game, pending, GameInfo, your_turn, 1}, State#state{seed=Seed2}};
handle_call({accept_seek, SeekId}, {Pid, _Tag}, #state{seeks_by_id=SeeksById, seeks=Seeks, seeks_by_player=SeeksByPlayer} = State) ->
	?log("Processing accept seek ~w", [SeekId]),
	case dict:find(SeekId, SeeksById) of
		{ok, #seek{pid=OPid, board_size=BoardSize, variant=Var} = Seek} -> 
			?log("Seek has match. Starting new game", []),
			{ok, GamePid} = c4_game:start_link(#game_info{ppid1=OPid, ppid2=Pid, board_size=BoardSize, variant=Var, type=anon}),
			GameId=SeekId,
			c4_player:joined(OPid, #game_info{pid=GamePid, id=GameId, board_size=BoardSize, variant=Var, type=anon}, your_turn, 1),
			ets:insert(c4_game_id_tbl, {GameId, GamePid}),
			ets:insert(c4_game_pid_tbl, {GamePid, GameId}),
			% Stop bugging these two players with seek notifications
			c4_player_master:unregister_player(OPid),
			c4_player_master:unregister_player(Pid),
			c4_player_master:notify_seek_removed(SeekId, Pid, OPid),
			Seeks2 = dict:erase(Seek#seek{pid=none, id=none}, Seeks),
			{ok, SeekList} = dict:find(OPid, SeeksByPlayer),
			case lists:delete(SeekId, SeekList) of
				[] -> SeeksByPlayer2 = dict:erase(OPid, SeeksByPlayer);
				NewSBP -> SeeksByPlayer2 = dict:store(OPid, NewSBP, SeeksByPlayer)
			end,
			SeeksById2 = dict:erase(SeekId, SeeksById),
			NewState = State#state{seeks=Seeks2, seeks_by_player=SeeksByPlayer2, seeks_by_id=SeeksById2},
			%?log("State ~w", [NewState]),
			{
				reply,
				{new_game, #game_info{pid=GamePid, id=GameId, board_size=BoardSize, variant=Var, type=anon}, other_turn, 2},
				NewState
			};
		error ->
			{reply, no_seek_found, State}
	end;
handle_call({cancel_seek, Pid}, _From, State) when is_pid(Pid) ->
	?log("Canceling seeks for player ~w", [Pid]),
	case remove_player_seeks(Pid, State) of
		{N, NewState} when N > 0 -> 
			?log("Removed ~w seeks found for player ~w ~w", [N, Pid, NewState]),
			{reply, seek_canceled, NewState}; 
		{0, State} ->
			?log("No seeks found for player ~w", [Pid]),
			{reply, no_seek_found, State}
	end;
handle_call({cancel_seek, Pid, SeekId}, _From, State) when is_integer(SeekId) ->
	case do_remove_seek(Pid, SeekId, State) of
		{ok, NewState} -> 
			%?log("State ~w", [NewState]),
			{reply, seek_canceled, NewState};
		no_seek_found -> {reply, no_seek_found, State}
	end;
% Request to join a pending game
handle_call({join_game, PlayerPid, GameId}, _From, State) ->
	% If found, notify any player already in the game.
	case ets:lookup(c4_game_id_tbl, GameId) of
		[{GameId, #game_info{variant=GameVar, board_size=BoardSize, ppid1=Other}}] -> 
			{ok, GamePid} = c4_game:start_link({ppid1=Other, ppid2=PlayerPid, board_size=BoardSize, variant=GameVar}),
			c4_player:joined(Other, GamePid, your_turn),
			ets:insert(c4_game_id_tbl, {GameId, GamePid}),
			ets:insert(c4_game_pid_tbl, {GamePid, GameId}),
			{reply, {new_game, GamePid, GameId, other_turn}, State};
		_ -> {reply, no_game, State}
	end;
handle_call(seek_list, _From, State) ->
	{reply, get_seek_list(State), State};
handle_call({register_for_seeks, PlayerPid}, _From, State) ->
	% Synchronously register the player and then return current seek list
	% To guarantee that player sees all current seeks and any coming afterwards
	c4_player_master:register_player(PlayerPid),
	{reply, get_seek_list(State), State};
handle_call(game_list, _From, State) ->
	% Go over the entire game table and build a list
	GameList = ets:foldl(fun(X, L) -> [X | L] end, [], c4_game_pid_tbl),
	{reply, GameList, State};
handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.

% @doc Handles game processes ending and users disconnecting.
% Callback function for miscellaneous messages received by the FSM process.
handle_info({'EXIT', Pid, _Reason}, #state{parent=Pid} = State) ->
	?log("Parent process went down, goodbye!", []),
	{stop, parent_died, State};
handle_info({'EXIT', GamePid, _Reason}, State) ->
	?log("Game process ~w went down", [GamePid]),
	case ets:lookup(c4_game_pid_tbl, GamePid) of
		[{GamePid, GameId}] -> 
			ets:delete(c4_game_id_tbl, GameId);
		_ -> ok
	end,
	ets:delete(c4_game_pid_tbl, GamePid),
	{noreply, State};
% On user disconnect, remove from player and seeks data.
handle_info({'DOWN', _Ref, process, P1, _Reason}, State) ->
	?log("A player process went down", []),
	{_n, NewState} = remove_player_seeks(P1, State),
	%?log("State ~w", [NewState]),
	{noreply, NewState};
handle_info({timeout, _Ref, log_state}, 
			#state{seeks=Seeks,seeks_by_id=SeeksById, seeks_by_player=SeeksByPlayer} = State) ->
	IdTblSize = ets:info(c4_game_id_tbl, size),
	PidTblSize = ets:info(c4_game_pid_tbl, size),
	S = dict:to_list(Seeks),
	SI = dict:to_list(SeeksById),
	SP = dict:to_list(SeeksByPlayer),
	?log("~w state: ~nid entries = ~w ~npid entries = ~w~nSeeks = ~w~n"
		"Seeks by Id = ~w~nSeeks by player = ~w", 
		[?MODULE, IdTblSize, PidTblSize, S, SI, SP]),
	erlang:start_timer(?LOG_STATE_INTERVAL, self(), log_state),
	{noreply, State}.

% @doc Handles the (asynchronous) stop message
% Generic FSM callback for synchronous messages.
handle_cast(stop, State)->
	{stop, normal, State};
handle_cast(Event, State) ->
	?log("Unexpected event ~w : ~w", [Event, State]),
	{noreply, State}.

% @doc Hot code swap callback (does nothing now).
code_change(_OldVsn, StateData, _Extra) ->
	{ok, StateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions

% @doc Computes the next number to be used as game id, which is guaranteed
% not to be in use at this very moment.
-spec(next_game_id({integer(), integer(), integer()}) -> {pos_integer(), {integer(), integer(), integer()}}).
next_game_id(Seed) ->
	{GameId, Seed2} = random:uniform_s(?MAX_GAME_ID, Seed),
	% If in use, try again.
	case ets:member(c4_game_id_tbl, GameId) of
		true -> next_game_id(Seed2);
		false -> {GameId, Seed2}
	end.

get_seek_list(#state{seeks_by_id=Seeks} = _State) ->
	% Get seeks from seekers dictionary values, as those still have the sender's Pid
	% unlike the keys in the seeks dictionary.
	dict:fold(fun(_K, V, L) -> [V | L] end, [], Seeks).

remove_player_seeks(Pid,#state{seeks=Seeks,seeks_by_id=SeeksById,seeks_by_player=SeeksByPlayer} = State) ->
	case dict:find(Pid, SeeksByPlayer) of
		error -> {0, State};
		{ok, []} -> {0, State};
		{ok, SeekIds} -> 
			?log("Removing seeks ~w for player ~w", [SeekIds, Pid]),
			SeeksByPlayer2 = dict:erase(Pid, SeeksByPlayer),
			SeeksById2 = dict:filter(fun(K,_V) -> not lists:member(K,SeekIds) end, SeeksById),
			Seeks2 = dict:filter(fun(_K,V) -> not lists:member(V, SeekIds) end, Seeks),
			c4_player_master:notify_seek_removed(SeekIds, Pid, none),
			NewState = State#state{seeks=Seeks2, seeks_by_id=SeeksById2, seeks_by_player=SeeksByPlayer2},
			?log("Removed seeks ~w ~w ~w", [Seeks, Seeks2, NewState]),
			{length(SeekIds), NewState}
	end.

-spec(do_remove_seek(pid(), seek_id, #state{}) ->{ok, #state{}} | {no_seek_found, #state{}}).
do_remove_seek(Pid, SeekId, #state{seeks=Seeks,seeks_by_id=SeeksById,seeks_by_player=SeeksByPlayer} = State) ->
	case dict:find(SeekId, SeeksById) of
		{ok, #seek{pid=Pid} = Seek} ->
			SeeksById2 = dict:erase(SeekId, SeeksById),
			Seeks2 = dict:erase(Seek#seek{pid=none, id=none}, Seeks),
			{ok, PlayerSeeks} = dict:find(Pid, SeeksByPlayer),
			PlayerSeeks2 = lists:delete(SeekId, PlayerSeeks),
			SeeksByPlayer2 = 
				case PlayerSeeks2 of
					[] -> dict:erase(Pid, SeeksByPlayer);
					_ -> dict:store(Pid, PlayerSeeks2, SeeksByPlayer)
				end,
			c4_player_master:notify_seek_removed(SeekId, Pid, none),
			NewState = State#state{seeks=Seeks2, seeks_by_id=SeeksById2, seeks_by_player=SeeksByPlayer2},
			%?log("State ~w", [NewState]),
			{ok, NewState};
		{ok, _Seek} -> no_seek_found;
		error -> no_seek_found
	end.
