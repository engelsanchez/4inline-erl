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
-export([start/0, start_link/0, seek/1, cancel_seek/1, accept_seek/1, seek_list/0, game_list/0, stop/0]).
-record(state, {seeks, seekers, seeks_by_id, parent, seed=now()}).
-define(NOLOG, true).
-include("c4_common.hrl").

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
-spec(seek(#seek{}) -> seek_pending | {new_game, #game_info{}, turn(), 1|2 }).
seek(Seek) ->
	gen_server:call(?MODULE, {seek, Seek}).

% @doc Call when a player requests to cancel a join request.
-spec(cancel_seek(pid()) -> seek_canceled | no_seek_pending).
cancel_seek(Pid) ->
	gen_server:call(?MODULE, {cancel_seek, Pid}).

% @doc User request to accept a pending seek.
-spec(accept_seek(pos_integer()) -> ok | seek_not_found).
accept_seek(SeekId) ->
	gen_server:call(?MODULE, {accept_seek, SeekId}).

% @doc Returns a list with info for all current games.
game_list() ->
	gen_server:call(?MODULE, game_list).

% @doc Returns the list of all pending seeks.
seek_list() ->
	gen_server:call(?MODULE, seek_list).

% @doc Requests the game master process to stop.
-spec(stop() -> ok).
stop() ->
	gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks

% @doc Starts trapping exits, creates player and game tables
% and starts FSM in loop state.
% Callback method for the OTP gen_fsm behavior.
init(ParentPid)->
	?log("Starting", []),
	process_flag(trap_exit, true),
	ets:new(c4_game_tbl, [named_table, private, set]),
	Seeks = dict:new(),
	Seekers = dict:new(),
	SeeksById = dict:new(),
	{ok, #state{seeks=Seeks, seekers=Seekers, seeks_by_id=SeeksById, parent=ParentPid, seed=now()}}.

% @doc Deletes player and game tables upon termination.
% Callback function for the OTP gen_fsm behavior.
terminate(_Reason, _Data) ->
	ets:delete(c4_game_tbl),
	ok.

% @doc The main 'loop' state of this FSM handles all messages 
% (seeks, seek cancellations, player registrations ).
handle_call({seek, #seek{pid=Pid, type=anon, variant=Var, board_size=BoardSize} = SeekIn}, _From, #state{seeks=Seeks, seekers=Seekers, seeks_by_id=SeeksById, seed=SeedIn} = State) ->
	{SeekId, Seed} = next_game_id(SeedIn),
	Seek = SeekIn#seek{id=SeekId},
	?log("Processing anonymous game seek ~w", [Seek]),
	Key = Seek#seek{pid=none},
	% Match with currents seeks. Start game if matched, add to seeks if not
	case dict:find(Key, Seeks) of
		{ok, OPid}  ->
			?log("Seek has match. Starting new game", []),
			{ok, GamePid} = c4_game:start_link(#game_info{ppid1=OPid, ppid2=Pid, board_size=BoardSize, variant=Var, type=anon}),
			{GameId, Seed2} = next_game_id(Seed),
			c4_player:joined(OPid, #game_info{pid=GamePid, id=GameId, board_size=BoardSize, variant=Var, type=anon}, your_turn, 1),
			ets:insert(c4_game_tbl, {GameId, GamePid}),
			Seeks2 = dict:erase(Key, Seeks),
			Seekers2 = dict:erase(OPid, Seekers),
			{
				reply,
				{new_game, #game_info{pid=GamePid, id=GameId, board_size=BoardSize, variant=Var, type=anon}, other_turn, 2}, 
				State#state{seeks=Seeks2, seekers=Seekers2, seed=Seed2}
			};
		error ->
			?log("No match. Seek -> Pending ", []),
			Seeks2 = dict:store(Key, Pid, Seeks),
			Seekers2 = dict:store(Pid, Seek, Seekers),
			SeeksById2 = dict:store(SeekId, Seek, SeeksById),
			c4_player_master:notify_seek_issued(Seek),
			{reply, seek_pending, State#state{seeks=Seeks2, seekers=Seekers2, seeks_by_id=SeeksById2}}
	end;
handle_call({seek, #seek{pid=Pid, type=priv, variant=Var, board_size=BoardSize} = Seek}, _From, #state{seed=Seed} = State) ->
	?log("Processing private game seek ~w", [Seek]),
	% Add to list of private games (to be started).
	{GameId, Seed2} = next_game_id(Seed),
	GameInfo = #game_info{variant=Var, board_size=BoardSize, ppid1=Pid, type=priv},
	ets:insert(c4_game_tbl, {GameId, GameInfo}),
	{reply, {new_game, pending, GameInfo, your_turn, 1}, State#state{seed=Seed2}};
handle_call({accept_seek, SeekId}, {Pid, _Tag}, #state{seeks_by_id=SeeksById, seeks=Seeks, seekers=Seekers, seed=Seed} = State) ->
	?log("Processing accept seek ~w", [SeekId]),
	case dict:find(SeekId, SeeksById) of
		{ok, #seek{pid=OPid, board_size=BoardSize, variant=Var} = Seek} -> 
			?log("Seek has match. Starting new game", []),
			{ok, GamePid} = c4_game:start_link(#game_info{ppid1=OPid, ppid2=Pid, board_size=BoardSize, variant=Var, type=anon}),
			{GameId, Seed2} = next_game_id(Seed),
			c4_player:joined(OPid, #game_info{pid=GamePid, id=GameId, board_size=BoardSize, variant=Var, type=anon}, your_turn, 1),
			ets:insert(c4_game_tbl, {GameId, GamePid}),
			Seeks2 = dict:erase(Seek#seek{pid=none}, Seeks),
			Seekers2 = dict:erase(OPid, Seekers),
			SeeksById2 = dict:erase(SeekId, SeeksById),
			{
				reply,
				{new_game, #game_info{pid=GamePid, id=GameId, board_size=BoardSize, variant=Var, type=anon}, other_turn, 2},
				State#state{seeks=Seeks2, seekers=Seekers2, seeks_by_id=SeeksById2, seed=Seed2}
			};
		error ->
			{reply, no_seek_found, State}
	end;
handle_call({cancel_seek, Pid}, _From, #state{seeks=Seeks, seekers=Seekers} = State) when is_pid(Pid) ->
	% @todo This assumes a single seek per client, which should soon change so we'll need
	% to match the exact seek being cancelled or cancel all
	case dict:find(Pid, Seekers) of
		{ok, #seek{id=SeekId} = Seek} -> 
			Seeks2 = dict:erase(Seek, Seeks),
			Seekers2 = dict:erase(Pid, Seekers),
			c4_player_master:notify_seek_removed(SeekId, Pid),
			{reply, seek_canceled, State#state{seeks=Seeks2, seekers=Seekers2}};
		error ->
			{reply, no_seek_pending, State}
	end;
% Request to join a pending game
handle_call({join_game, PlayerPid, GameId}, _From, State) ->
	% If found, notify any player already in the game.
	case ets:lookup(c4_game_tbl, GameId) of
		[{GameId, GameVar, BoardSize, Other}] -> 
			{ok, GamePid} = c4_game:start_link({ppid1=Other, ppid2=PlayerPid, board_size=BoardSize, variant=GameVar}),
			c4_player:joined(Other, GamePid, your_turn),
			ets:delete(c4_game_tbl, GameId),
			{reply, {new_game, GamePid, GameId, other_turn}, State};
		[] -> {reply, no_game, State}
	end;
handle_call(seek_list, _From, #state{seekers=Seekers} = Data) ->
	% Get seeks from seekers dictionary values, as those still have the sender's Pid
	% unlike the keys in the seeks dictionary.
	SeekList = dict:fold(fun(_K, V, L) -> [V | L] end, [], Seekers),
	{reply, SeekList, Data};
handle_call(game_list, _From, State) ->
	% Go over the entire game table and build a list
	GameList = ets:foldl(fun(X, L) -> [X | L] end, [], c4_game_tbl),
	{reply, GameList, State};
handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.

% @doc Handles game processes ending and users disconnecting.
% Callback function for miscellaneous messages received by the FSM process.
handle_info({'EXIT', Pid, _Reason}, #state{parent=Pid} = State) ->
	{stop, parent_died, State};
handle_info({'EXIT', _Pid, _Reason}, State) ->
	{noreply, State};
% On user disconnect, remove from player and seeks data.
handle_info({'DOWN', _Ref, process, P1, _Reason}, #state{seeks=Seeks, seekers=Seekers} = State) ->
	ets:delete(c4_player_tbl, P1),
	case dict:find(P1, Seekers) of
		{ok, Seek}  -> 
			Seeks2 = dict:erase(Seek, Seeks),
			Seekers2 = dict:erase(P1, Seekers),
			{noreply, State#state{seeks=Seeks2, seekers=Seekers2}};
		error -> 
			{noreply, State}
	end.

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




