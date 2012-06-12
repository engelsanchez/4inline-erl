% Description: process that receives messages from game client.
-module(c4_player).
-behaviour(gen_fsm).
% State machine exports
-export([init/1, idle/3, handle_event/3, waiting_for_game/3, my_turn/3, other_turn/3, terminate/3]).
% Public interface exports
-export([start/0, start_link/0, join/1, cancel_join/1, joined/3, play/2, played/4, quit_game/1, other_quit/2, quit/1]).
-record(state, {game=none, parent}).
-include("c4_common.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Starts unsupervised c4_player process
start() ->
	gen_fsm:start(?MODULE, {self()}, []).

% @doc Starts c4_player process supervised by and linked to current process
start_link() ->
	gen_fsm:start_link(?MODULE, {self()}, []).

% @doc Player requests to join a game.
% Returns :  join_ack | {new_game, Turn} | {error, Error, ErrorMsg}.
join(Pid) ->
	gen_fsm:sync_send_event(Pid, {join}).

% @doc Impatient player requests to forget about joining a game.
% Returns: join_canceled | no_join_pending
cancel_join(Pid) ->
	gen_fsm:sync_send_event(Pid, {cancel_join}).

joined(Pid, GamePid, Turn) ->
	gen_fsm:sync_send_event(Pid, {new_game, GamePid, Turn}).

% @doc Executes a move for this player
% Returns : play_ok | you_win | {error, ErrorCode, ErrorMsg}
play(Pid, Col) ->
	gen_fsm:sync_send_event(Pid, {play, Col}).

% @doc Notifies this player of a move by the other player.
played(Pid, GamePid, Col, Status) ->
	gen_fsm:sync_send_event(Pid, {c4_game, GamePid, Col, Status}).

quit_game(Pid) ->
	gen_fsm:sync_send_event(Pid, quit_game).

other_quit(Pid, GamePid) ->
	gen_fsm:sync_send_event(Pid, {c4_game, GamePid, other_quit}).

% @doc Call when the player has been disconnected
quit(Pid) when is_pid(Pid) ->
	gen_fsm:send_all_state_event(Pid, player_quit),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FSM functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Initializes state machine to the idle state
init({ParentPid}) ->
	?log("Starting~n", []),
	{ok, idle, #state{parent=ParentPid}}.

handle_event(player_quit, _StateName, #state{game=GamePid} = StateData) when is_pid(GamePid) ->
	c4_game:quit(GamePid, self()), 
	{stop, player_quit, StateData#state{game=none}};
handle_event(player_quit, _StateName, StateData) ->
	{stop, player_quit, StateData};
handle_event(Event, StateName, StateData) ->
	?log("Unexpected event in state ~w : ~w~n", [StateName, Event]),
	{ok, StateName, StateData}.

% @doc No real cleanup upon game end
terminate(_Reason, _StateName, _State) ->
        ok.

% @doc Limbo state when no game is going on and user has not requested to join a game
idle({join}, _From, State)  ->
	?log("Player wants to join a game~n", []),
	Reply = c4_game_master:join(self()),
	case Reply of
		{new_game, GamePid, Status} ->
			?log("New game started right away~n", []),
			new_game(GamePid, Status, State);
		join_pending -> 
			?log("Player will have to wait for another~n", []),
			{reply, join_pending, waiting_for_game, State}
	end;
idle(Event, _From, State) ->
	?log("Unexpected message while idle : ~w~n", [Event]),
	{reply, {error, bad_cmd, "Join a game first"}, idle, State}.

% @doc Waiting for game coordinator to join a game
% or for the user to cancel the request
waiting_for_game({new_game, GamePid, Status} = Event, _From, #state{parent=ParentId} = State) ->
	?log("New game started: ~w~n", [Event]),
	ParentId ! {new_game, Status},
	new_game(GamePid, Status, State);
waiting_for_game({cancel_join}, _From, State) ->
	?log("Player wants to cancel join request~n", []),
	case c4_game_master:cancel_join(self()) of
		join_canceled -> {reply, join_canceled, idle, State};
		% Game started, should receive game started message soon
		no_join_pending -> {reply, no_join_pending, waiting_for_game, State}
	end;
waiting_for_game(Event, _From, State) ->
	?log("Unexpected message while waiting for game : ~w~n", [Event]),
	{reply, {error, bad_cmd, "Waiting for a game now"}, waiting_for_game, State}.

% @doc Waiting for this player to move state.
my_turn({play, Col}, {ParentPid, _Tag}, #state{game=GamePid, parent=ParentPid} = State) ->
	?log("Player played column ~w~n", [Col]),
	case c4_game:play(GamePid, self(), Col) of
		invalid_move -> {reply, {error, invalid_move, <<"Invalid Move">>}, my_turn, State};
		ok -> {reply, play_ok, other_turn, State}
	end;
my_turn({c4_game, GamePid, other_quit}, _From, #state{game=GamePid} = State) ->
	{reply, ok, idle, State#state{game=none}};
my_turn(Event, _From, State) ->
	?log("Unexpected message while waiting for player to move ~w~n", [Event]),
	{reply, {error, bad_cmd, "Waiting for player move"}, my_turn, State}.

% @doc Waiting for other player to move state.
other_turn({c4_game, GamePid, Col, your_turn}, _From, #state{game=GamePid, parent=PPid} = State) ->
	?log("Other player played column ~w~n", [Col]),
	PPid ! {other_played, Col},	
	{reply, ok, my_turn, State};
other_turn({c4_game, GamePid, Col, you_lose}, _From, #state{game=GamePid, parent=PPid} = State) ->
	?log("Other player played column ~w and wins~n", [Col]),
	PPid ! {other_won, Col},
	{reply, ok, idle, State#state{game=GamePid}};
other_turn(Event, _From, State) ->
	?log("Unexpected message while waiting for other player to move ~w~n", [Event]),
	{reply, {error, bad_cmd, "Waiting for other player to move"}, other_turn, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions

% @doc Returns reply when new game started
new_game(GamePid, Status, State) ->
	case Status of
		play -> {reply, {new_game, play}, my_turn, State#state{game=GamePid}};
		wait ->	{reply, {new_game, wait}, other_turn, State#state{game=GamePid}}
	end.
