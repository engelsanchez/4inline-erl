% @doc Protocol independent interface for remote 4 in-line players.
% The functions that handle actions by our player are 
% {@link join/1}, {@link cancel_join/1},
% {@link play/2} and {@link quit_game/1}.
% The functions that handle events from the other player are
% {@link joined/3}, {@link played/4} and {@link other_quit/2}.
% The rest are callback functions as this modules implements the generic
% OTP gen_fsm (State machine) behavior.

-module(c4_player).
-behaviour(gen_fsm).
% FSM callback exports
-export([init/1, idle/3, handle_event/3, handle_sync_event/4, handle_info/3, waiting_for_game/3, waiting_for_reconnect/3, my_turn/3, other_turn/3, terminate/3, code_change/4]).
% Public API exports
-export([start/0, start_link/0, text_cmd/2, text_reply/1, seek/4, cancel_seek/1, joined/5, play/2, 
	played/4, quit_game/1, other_quit/2, other_returned/2, 
	other_disconnected/1, disconnected/1]).
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

parse_board_spec(<<W1:8/integer, W2:8/integer, "x", H1:8/integer, H2:8/integer>>) 
		when W1 >= $0, W1 =< $9, W2 >= $0, W2 =< $9, H1 >= $0, H1 =< $9, H2 >= $0, H2 =< $9 ->
	#board_size{rows= (H1-$0)*10+(H2-$0), cols=(W1-$0)*10+(W2-$0)};
parse_board_spec(_) -> undefined.
	

text_cmd(Pid, <<"SEEK ", BSpec:5/binary, " ", GVar:4/binary, " ", GType:4/binary>>) ->
	BoardSpec = parse_board_spec(BSpec),
	GameVar = case GVar of <<"STD">> -> std; <<"POP">> -> pop; _ -> undefined end,
	GameType = case GType of <<"PRIV">> -> priv; <<"ANON">> -> anon; _ -> undefined end,			
	if
		BoardSpec == undefined;GameVar == undefined;GameType == undefined ->
			<<"INVALID_COMMAND">>;
		true ->
			case c4_player:seek(Pid, GameType, GameVar, BoardSpec) of
				seek_pending -> <<"SEEK_PENDING">>;
				{new_game, play} -> <<"NEW_GAME_PLAY">>;
				{new_game, wait} -> <<"NEW_GAME_WAIT">>
			end
	end;
text_cmd(Pid, <<"CANCEL_SEEK">>) ->
	case c4_player:cancel_seek(Pid) of
		seek_canceled -> <<"SEEK_CANCELED">>;
		no_seek_pending -> <<"NO_SEEK_PENDING">>
	end;
text_cmd(Pid, <<"PLAY ", Col:8/integer>>) ->
	case c4_player:play(Pid, Col - $0) of
		play_ok -> <<"PLAY_OK">>;
		you_win -> <<"YOU_WIN">>
	end;
text_cmd(Pid, <<"QUIT_GAME">>) ->
	case c4_player:quit_game(Pid) of
		game_abandoned -> <<"GAME_ABANDONED">>
	end;
text_cmd(_Pid, _) when is_pid(_Pid) ->
	<<"UNKNOWN_COMMAND">>.

text_reply({new_game, GameId, PlayerId, Turn}) ->
	T = case Turn of your_turn -> $Y; other_turn -> $O; wait -> $W  end,
	GStr = list_to_binary(integer_to_list(GameId)),
	PStr = list_to_binary(integer_to_list(PlayerId)),
	<<"NEW_GAME ", T:8/integer, " ", GStr/binary, " ", PStr/binary>>;
text_reply({other_played, Col}) when is_integer(Col) ->
	<<"OTHER_PLAYED ", (Col+$0)/integer>>;
text_reply({other_won, Col}) when is_integer(Col) ->
	<<"OTHER_WON ", (Col+$0)/integer>>;
text_reply({other_disconnected}) ->
	<<"OTHER_DISCONNECTED" >>;
text_reply({other_returned, Turn}) ->
	T = case Turn of your_turn -> $Y; other_turn -> $O; wait -> $W  end,
	<<"OTHER_RETURNED ", T:8/integer>>.

% @doc Player requests to join a game.
% Returns :  join_ack | {new_game, Turn} | {error, Error, ErrorMsg}.
seek(Pid, GameType, GameVar, BoardSize) ->
	gen_fsm:sync_send_event(Pid, {seek, GameType, GameVar, BoardSize}).

% @doc Impatient player requests to forget about joining a game.
-spec(cancel_seek(pid()) -> seek_canceled | no_seek_pending).
cancel_seek(Pid) ->
	gen_fsm:sync_send_event(Pid, {cancel_seek}).

% @doc Called when paired with another player for a game
-spec(joined(pid(), pid(), pos_integer(), pos_integer(), turn()) -> {new_game, pos_integer(), turn()}).
joined(Pid, GamePid, GameId, PlayerId, Turn) ->
	gen_fsm:sync_send_event(Pid, {new_game, GamePid, GameId, PlayerId, Turn}).

% @doc Executes a move for this player
% Returns : play_ok | you_win | {error, ErrorCode, ErrorMsg}
play(Pid, Col) ->
	gen_fsm:sync_send_event(Pid, {play, Col}).

% @doc Notifies this player of a move by the other player.
played(Pid, GamePid, Col, Status) ->
	gen_fsm:sync_send_event(Pid, {c4_game, GamePid, Col, Status}).

% @doc Called by our player to leave a game.
quit_game(Pid) ->
	gen_fsm:sync_send_event(Pid, quit_game).

% @doc Should be called when the other player just quit the game.
other_quit(Pid, GamePid) ->
	gen_fsm:sync_send_event(Pid, {c4_game, GamePid, other_quit}).

% @doc Called when the player has been disconnected
-spec(disconnected(pid()) -> ok).
disconnected(Pid) when is_pid(Pid) ->
	gen_fsm:sync_send_all_state_event(Pid, player_disconnected).

% @doc Alerts our parent that the other player in a game has disconnected.
-spec(other_disconnected(pid()) -> ok).
other_disconnected(Pid) when is_pid(Pid) ->
	gen_fsm:sync_send_all_state_event(Pid, other_disconnected).

% @doc Alerts our handler that the other player in a game has reconnected.
other_returned(Pid, Turn) when is_pid(Pid) ->
	gen_fsm:sync_send_all_state_Event(Pid, {other_returned, Turn}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FSM functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Initializes state machine to the idle state
init({ParentPid}) ->
	?log("Starting~n", []),
	c4_game_master:register_player(self()),
	{ok, idle, #state{parent=ParentPid}}.

% @doc Does nothing since we don't expect asynchronous events.
% Generic FSM callback for asynchronous events.
handle_event(Event, StateName, StateData) ->
	?log("Unexpected event in state ~w : ~w ~w~n", [StateName, Event, StateData]),
	{ok, StateName, StateData}.

% @doc Handles player disconnect messages.
% Generic callback for synchronous events for all states.
handle_sync_event(player_disconnected, _From, _StateName, #state{game=GamePid} = StateData) when is_pid(GamePid) ->
	c4_game:quit(GamePid, self()), 
	{stop, normal, StateData#state{game=none}};
handle_sync_event(player_disconnected, _From, _StateName, #state{game=GamePid} = StateData) when is_pid(GamePid) ->
	{replay, ok, waiting_for_reconnect, StateData};
handle_sync_event(Event, _From, StateName, StateData) ->
	?log("Unexpected event in state ~w : ~w~n", [StateName, Event]),
	{ok, StateName, StateData}.

handle_info(Msg, StateName, StateData) ->
	?log("Unexpected event ~w in state ~w : ~w~n", [Msg, StateName, StateData]),
	{ok, StateName, StateData}.

% @doc Code hot swapping callback (does nothing now).
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

% @doc No real cleanup upon game end
terminate(_Reason, _StateName, _State) ->
	ok.

% @doc Limbo state when no game is going on and user has not requested to join a game
idle({seek, GameType, GameVar, BoardSize}, _From, State)  ->
	?log("Player wants to join a game~n", []),
	Reply = c4_game_master:seek(self(), GameType, GameVar, BoardSize),
	case Reply of
		{new_game, GamePid, GameId, PlayerId, Status} ->
			?log("New game started right away~n", []),
			new_game(GamePid, GameId, PlayerId, Status, State);
		seek_pending -> 
			?log("Player will have to wait for another~n", []),
			{reply, seek_pending, waiting_for_game, State}
	end;
idle(Event, _From, State) ->
	?log("Unexpected message while idle : ~w~n", [Event]),
	{reply, {error, bad_cmd, "Join a game first"}, idle, State}.

% @doc Waiting for game coordinator to join a game
% or for the user to cancel the request
waiting_for_game({new_game, GamePid, GameId, PlayerId, Status} = Event, _From, #state{parent=ParentId} = State) ->
	?log("New game started: ~w~n", [Event]),
	ParentId ! {new_game, GameId, PlayerId, Status},
	new_game(GamePid, GameId, PlayerId, Status, State);
waiting_for_game({cancel_seek}, _From, State) ->
	?log("Player wants to cancel seek request~n", []),
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

waiting_for_reconnect({other_returned, Turn}, _From, #state{parent=PPid} = State) ->
	NextState = case Turn of your_turn->my_turn;other_turn->other_turn end,
	PPid ! {other_returned, Turn},
	{reply, ok, NextState, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions

% @doc Returns reply when new game started
new_game(GamePid, GameId, PlayerId, Status, State) ->
	case Status of
		play -> {reply, {new_game, GameId, PlayerId, your_turn}, my_turn, State#state{game=GamePid}};
		wait ->	{reply, {new_game, GameId, PlayerId, other_turn}, other_turn, State#state{game=GamePid}}
	end.
