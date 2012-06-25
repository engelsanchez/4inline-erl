% @doc Coordinates the creation of games between pairs of players.
% It mostly sits in a loop receiving join requests.  When the first
% join request comes, the player is told to wait for another. When the
% next one comes, the pair are put into a newly created game and
% the process repeats over and over.
-module(c4_game_master).
-behaviour(gen_fsm).
% FSM callbacks
-export([init/1, handle_sync_event/4, handle_event/3, handle_info/3, loop/3, terminate/3, code_change/4]).
% Public API
-export([start/0, start_link/0, seek/1, cancel_seek/1, seek_list/0, game_list/0, stop/0]).
-record(state, {seeks, seekers, parent, seed=now(), pseed=now()}).
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

% @doc Call when a player requests to join a game. 
-spec(seek(#seek{}) -> seek_pending | {new_game, #game_info{}, turn(), 1|2 }).
seek(Seek) ->
	gen_fsm:sync_send_event(?MODULE, {seek, Seek}).

% @doc Call when a player requests to cancel a join request.
-spec(cancel_seek(pid()) -> seek_canceled | no_seek_pending).
cancel_seek(Pid) ->
	gen_fsm:sync_send_event(?MODULE, {cancel_seek, Pid}).

% @doc Returns a list with info for all current games.
game_list() ->
	gen_fsm:sync_send_event(?MODULE, game_list).

% @doc Returns the list of all pending seeks.
seek_list() ->
	gen_fsm:sync_send_event(?MODULE, seek_list).

% @doc Requests the game master process to stop.
-spec(stop() -> ok).
stop() ->
	gen_fsm:sync_send_all_state_event(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FSM callbacks

% @doc Starts trapping exits, creates player and game tables
% and starts FSM in loop state.
% Callback method for the OTP gen_fsm behavior.
init(ParentPid)->
	?log("Starting~n", []),
	process_flag(trap_exit, true),
	ets:new(c4_game_tbl, [named_table, private, set]),
	Seeks = dict:new(),
	Seekers = dict:new(),
	{ok, loop, #state{seeks=Seeks, seekers=Seekers, parent=ParentPid, seed=now()}}.

% @doc Deletes player and game tables upon termination.
% Callback function for the OTP gen_fsm behavior.
terminate(_Reason, _Name, _Data) ->
	ets:delete(c4_game_tbl),
	ok.

% @doc The main 'loop' state of this FSM handles all messages 
% (seeks, seek cancellations, player registrations ).
loop({seek, #seek{pid=Pid, game_type=anon, game_var=Var, board_size=BoardSize} = SeekIn}, _From, #state{seeks=Seeks, seekers=Seekers, seed=SeedIn, pseed=PSeed} = State) ->
	{SeekId, Seed} = next_game_id(SeedIn),
	Seek = SeekIn#seek{id=SeekId},
	?log("Processing anonymous game seek ~w~n", [Seek]),
	Key = Seek#seek{pid=none},
	% Match with currents seeks. Start game if matched, add to seeks if not
	case dict:find(Key, Seeks) of
		{ok, OPid}  ->
			?log("Seek has match. Starting new game~n", []),
			{ok, GamePid} = c4_game:start_link(#game_info{ppid1=OPid, ppid2=Pid, board_size=BoardSize, variant=Var}),
			{GameId, Seed2} = next_game_id(Seed),
			{PlayerId, OPlayerId, PSeed2} = next_player_ids(PSeed),
			c4_player:joined(OPid, #game_info{pid=GamePid, id=GameId, pid1=OPlayerId, board_size=BoardSize, variant=Var}, your_turn, 1),
			ets:insert(c4_game_tbl, {GameId, GamePid}),
			Seeks2 = dict:erase(Key, Seeks),
			Seekers2 = dict:erase(OPid, Seekers),
			{
				reply, 
				{new_game, #game_info{pid=GamePid, id=GameId, pid1=PlayerId, board_size=BoardSize, variant=Var}, other_turn, 2}, 
				loop, 
				State#state{seeks=Seeks2, seekers=Seekers2, seed=Seed2, pseed=PSeed2}
			};
		error ->
			?log("No match. Seek -> Pending ~n", []),
			Seeks2 = dict:store(Key, Pid, Seeks),
			Seekers2 = dict:store(Pid, Seek, Seekers),
			c4_player_master:notify_seek_issued(Seek),
			{reply, seek_pending, loop, State#state{seeks=Seeks2, seekers=Seekers2}}
	end;
loop({seek, #seek{pid=Pid, game_type=priv, game_var=Var, board_size=BoardSize} = Seek}, _From, #state{seed=Seed, pseed=PSeed} = State) ->
	?log("Processing private game seek ~w~n", [Seek]),
	% Add to list of private games (to be started).
	{GameId, Seed2} = next_game_id(Seed),
	{PId1, PId2, PSeed2} = next_player_ids(PSeed), 
	GameInfo = #game_info{variant=Var, board_size=BoardSize, pid1=PId1, ppid1=Pid, pid2=PId2},
	ets:insert(c4_game_tbl, {GameId, GameInfo}),
	{reply, {new_game, pending, GameInfo, your_turn, 1}, loop, State#state{seed=Seed2,pseed=PSeed2}};
loop({cancel_seek, Pid}, _From, #state{seeks=Seeks, seekers=Seekers} = State) when is_pid(Pid) ->
	% @todo This assumes a single seek per client, which should soon change so we'll need
	% to match the exact seek being cancelled or cancel all
	case dict:find(Pid, Seekers) of
		{ok, Seek} -> 
			Seeks2 = dict:erase(Seek, Seeks),
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
			{ok, GamePid} = c4_game:start_link({ppid1=Other, ppid2=PlayerPid, board_size=BoardSize, variant=GameVar}),
			c4_player:joined(Other, GamePid, your_turn),
			ets:delete(c4_game_tbl, GameId),
			{reply, {new_game, GamePid, GameId, other_turn}, loop, State};
		[] -> {reply, no_game, look, State}
	end;
loop(seek_list, _From, #state{seeks=Seeks} = Data) ->
	{reply, dict:fetch_keys(Seeks), loop, Data};
loop(game_list, _From, State) ->
	% Go over the entire game table and build a list
	GameList = ets:foldl(fun(X, L) -> [X | L] end, [], c4_game_tbl),
	{reply, GameList, loop, State}.

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
		{ok, Seek}  -> 
			Seeks2 = dict:erase(Seek, Seeks),
			Seekers2 = dict:erase(P1, Seekers),
			{next_state, Name, State#state{seeks=Seeks2, seekers=Seekers2}};
		error -> 
			{next_state, Name, State}
	end.

% @doc Handles the (synchronous) stop message
% Generic FSM callback for synchronous messages.
handle_event(stop, _StateName, State)->
	{stop, normal, State};
handle_event(Event, StateName, State) ->
	?log("Unexpected event ~w in state ~w (~w)~n", [Event, StateName, State]),
	{next_state, StateName, State}.

% @doc Handles the (asynchronous) stop message
% Generic FSM callback for asynchronous messages.
handle_sync_event(stop, _From, _StateName, State) ->
	{stop, normal, ok, State};
handle_sync_event(Event, _From, StateName, State) ->
	?log("Unexpected event ~w in state ~w (~w)~n", [Event, StateName, State]),
	{reply, {error, unexpected, "Unexpected message"}, StateName, State}.

% @doc Hot code swap callback (does nothing now).
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

% @doc Computes the next number to be used as game id, which is guaranteed
% not to be in use at this very moment.
-spec(next_game_id({integer(), integer(), integer()}) -> {pos_integer(), {integer(), integer(), integer()}}).
next_game_id(Seed) ->
	{GameId, Seed2} = random:uniform_s(?MAX_GAME_ID, Seed),
	% If in use, try again.
	case ets:member(c4_game_tbl, GameId) of
		true -> next_game_id(Seed2);
		false -> {GameId, Seed2}
	end.

% @doc Returns a pair of distinct random ids to use as ids for the players.
next_player_ids(Seed) ->
	{Id1, Seed2} = random:uniform_s(?MAX_PLAYER_ID, Seed),
	{Id2, Seed3} = next_player_id(Seed2, Id1),
	{Id1, Id2, Seed3}.

% @doc Returns the next random number in the sequence making sure it's NOT the same
% as the given one (in the very remote case that could happen).
next_player_id(Seed, Id1) ->
	{Id2, Seed2} = Pair = random:uniform_s(?MAX_PLAYER_ID, Seed),
	case Id2 of
		Id1 -> next_player_id(Seed2, Id1);
		_ -> Pair
	end.



