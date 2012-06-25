% @doc Protocol independent interface for remote 4 in-line players.
% The functions that handle actions by our player are 
% {@link seek/1}, {@link cancel_seek/1},
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
-export([start/0, start_link/0, stop/1, text_cmd/2, text_reply/1, get_state/1,
       	seek/2, cancel_seek/1, joined/4, play/3, 
	played/4, quit_game/1, other_quit/2, other_returned/2, 
	other_disconnected/1, disconnected/1, game_started/2, seek_issued/2]).
-include("c4_common.hrl").
-record(state, {game=none, parent}).

% Width of integer fields in input text binaries.
-define(ISIZE, 6).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Starts unsupervised c4_player process
start() ->
	gen_fsm:start(?MODULE, {self()}, []).

% @doc Starts c4_player process supervised by and linked to current process
start_link() ->
	gen_fsm:start_link(?MODULE, {self()}, []).

% @doc Stops the player process
stop(Pid) ->
	gen_fsm:sync_send_all_state_event(Pid, stop).

% @doc Reads board dimensions from text (07x06,08x07,etc).
parse_board_size(<<W1:8/integer, W2:8/integer, "x", H1:8/integer, H2:8/integer>>) 
		when W1 >= $0, W1 =< $9, W2 >= $0, W2 =< $9, H1 >= $0, H1 =< $9, H2 >= $0, H2 =< $9 ->
	#board_size{rows= (H1-$0)*10+(H2-$0), cols=(W1-$0)*10+(W2-$0)};
parse_board_size(_) -> undefined.

% @doc Parses a seek command
-spec(parse_seek(binary()) -> #seek{} | invalid_command).
parse_seek(<<"SEEK ", GType:4/binary, " ", GVar:3/binary, " ", BSpec:5/binary>>) ->
	BoardSpec = parse_board_size(BSpec),
	GameVar = case GVar of <<"STD">> -> std; <<"POP">> -> pop; _ -> undefined end,
	GameType = case GType of <<"PRIV">> -> priv; <<"ANON">> -> anon; _ -> undefined end,
	if
		BoardSpec == undefined;GameVar == undefined;GameType == undefined -> invalid_command;
		true -> #seek{board_size=BoardSpec, game_var=GameVar, game_type=GameType}
	end;
parse_seek(_) -> invalid_command.

% @doc Simple conversion of integer to binary string.
i2b(I) -> list_to_binary(integer_to_list(I)).

% @doc Parses integer from binary string.
-spec(b2i(binary()) -> integer()).
b2i(B) -> list_to_integer(binary_to_list(B)).

% @doc Translates turn atom to character (Y|O|W).
turnc(Turn) -> case Turn of your_turn -> $Y; other_turn -> $O; wait -> $W  end.

% @doc Game variant -> binary string (POP|STD).
var2txt(Variant) -> case Variant of std -> <<"STD">>;pop -> <<"POP">> end.

% @doc Parses a text command and executes the request action (SEEK, PLAY, etc).
text_cmd(Pid, <<"ACCEPT_SEEK ", SeekId:?ISIZE/binary>>) ->
	c4_player:accept_seek(Pid, b2i(SeekId));
text_cmd(Pid, <<"CANCEL_SEEK">>) ->
	c4_player:cancel_seek(Pid);
text_cmd(Pid, <<"CANCEL_SEEK ", SeekId:?ISIZE/binary>>) ->
	c4_player:cancel_seek(Pid, b2i(SeekId));
text_cmd(Pid, <<"JOIN_GAME ", GameId:?ISIZE/binary>>) ->
	c4_player:join_game(Pid, b2i(GameId));
text_cmd(Pid, <<"PLAY ", GameId:?ISIZE/binary, " ", Col:8/integer>>) ->
	c4_player:play(Pid, b2i(GameId), Col - $0);
text_cmd(Pid, <<"QUIT_GAME ", GameId:?ISIZE/binary>>) ->
	c4_player:quit_game(Pid, b2i(GameId));
text_cmd(Pid, <<"SEEK ", _Rest/binary>> = Cmd) ->
	case parse_seek(Cmd) of
		invalid_command -> invalid_command;
		#seek{} = Seek -> c4_player:seek(Pid, Seek)
	end;
text_cmd(_Pid, _) when is_pid(_Pid) ->
	invalid_command.

% @doc Translates a reply to a text command into text for sending to client.
text_reply({new_game, #game_info{id=GameId, pid1=PlayerId, variant=Var, board_size=#board_size{rows=H,cols=W}}, Turn, Color}) ->
	<<"NEW_GAME ",  (i2b(GameId))/binary, " ", (i2b(PlayerId))/binary, " ", (var2txt(Var))/binary, " ", (i2b(W))/binary, "x", (i2b(H))/binary, " ",
		(turnc(Turn)):8/integer, " ", (i2b(Color))/binary>>;
text_reply({seek_removed, SeekId}) ->
	<<"SEEK_REMOVED ", (i2b(SeekId))/binary>>;
text_reply({seek_issued, #seek{id=SeekId, board_size=#board_size{rows=H,cols=W}, game_var=Var}}) ->
	<<"SEEK_ISSUED ", (i2b(W))/binary, "x", (i2b(H))/binary, " ", (var2txt(Var))/binary, " ", (i2b(SeekId))/binary>>;
text_reply({other_played, Col}) when is_integer(Col) ->
	<<"OTHER_PLAYED ", (Col+$0)/integer>>;
text_reply({other_won, Col}) when is_integer(Col) ->
	<<"OTHER_WON ", (Col+$0)/integer>>;
text_reply({other_returned, Turn}) ->
	<<"OTHER_RETURNED ", (turnc(Turn)):8/integer>>;
text_reply(Reply) ->
	case Reply of
		other_disconnected -> <<"OTHER_DISCONNECTED" >>;
		seek_pending -> <<"SEEK_PENDING">>;
		seek_canceled -> <<"SEEK_CANCELED">>;
		no_seek_pending -> <<"NO_SEEK_PENDING">>;
		play_ok -> <<"PLAY_OK">>;
		you_win -> <<"YOU_WIN">>;
		game_abandoned -> <<"GAME_ABANDONED">>;
		invalid_command -> <<"INVALID_COMMND">>;
		{error, _Code, ErrMsg} -> <<"ERROR ", ErrMsg/binary>>;
		_ -> ?log("Unexpected reply ~w~n", [Reply]), <<"INTERNAL_ERROR">>
	end.


% @doc Player requests to join a game.
% Returns :  join_ack | {new_game, Turn} | {error, Error, ErrorMsg}.
seek(Pid, #seek{} = Seek) ->
	gen_fsm:sync_send_event(Pid, {seek, Seek}).

% @doc Impatient player requests to forget about joining a game.
-spec(cancel_seek(pid()) -> seek_canceled | no_seek_pending).
cancel_seek(Pid) ->
	gen_fsm:sync_send_event(Pid, {cancel_seek}).

% @doc Called when paired with another player for a game
-spec(joined(pid(), #game_info{}, turn(), 1|2) -> {new_game, #game_info{}, turn(), 1|2}).
joined(Pid, GameInfo, Turn, Color) ->
	gen_fsm:sync_send_event(Pid, {new_game, GameInfo, Turn, Color}).

% @doc Executes a move for this player
% Returns : play_ok | you_win | {error, ErrorCode, ErrorMsg}
play(Pid, GameId, Col) ->
	gen_fsm:sync_send_event(Pid, {play, GameId, Col}).

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
	gen_fsm:sync_send_all_state_event(Pid, stop).

% @doc Alerts our parent that the other player in a game has disconnected.
-spec(other_disconnected(pid()) -> ok).
other_disconnected(Pid) when is_pid(Pid) ->
	gen_fsm:sync_send_all_state_event(Pid, other_disconnected).

% @doc Alerts our handler that the other player in a game has reconnected.
other_returned(Pid, Turn) when is_pid(Pid) ->
	gen_fsm:sync_send_all_state_event(Pid, {other_returned, Turn}).

% @doc Sends a notification to the player that a game started.
game_started(Pid, GameId) ->
	gen_fsm:sync_send_all_state_event(Pid, {game_started, GameId}).

% @doc Sends a notification to the player that a seek has been issued.
seek_issued(Pid, #seek{} = Seek) ->
	gen_fsm:send_event(Pid, {seek_issued, Seek}).

% @doc Returns the state information of the player process as a tuple
% {StateName, StateData}
-spec(get_state(pid()) -> {string(), #state{}}).
get_state(Pid) ->
	gen_fsm:sync_send_all_state_event(Pid, get_state).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FSM functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Initializes state machine to the idle state
init({ParentPid}) ->
	?log("Starting~n", []),
	c4_player_master:register_player(self()),
	{ok, idle, #state{parent=ParentPid}}.

% @doc Does nothing since we don't expect asynchronous events.
% Generic FSM callback for asynchronous events.
handle_event(Event, StateName, StateData) ->
	?log("Unexpected event in state ~w : ~w ~w~n", [StateName, Event, StateData]),
	{ok, StateName, StateData}.

% @doc Handles player disconnect, game quit and get state requests.
% Generic callback for synchronous events for all states.
handle_sync_event(quit_game, _From, _StateName, #state{game=GamePid} = StateData) when is_pid(GamePid) ->
	c4_game:quit(GamePid, self()), 
	{stop, normal, ok, StateData#state{game=none}};
handle_sync_event(stop, _From, _StateName, State) ->
	{stop, normal, ok, State};
handle_sync_event(get_state, _From, StateName, State) ->
	{reply, {StateName, State}, StateName, State};
handle_sync_event(Event, _From, StateName, StateData) ->
	?log("Unexpected event in state ~w : ~w~n", [StateName, Event]),
	{next_state, StateName, StateData}.

% @doc FSM callback for non FSM messages received by the process (none expected here).
handle_info({seek_issued, #seek{}} = Event, State, #state{parent=ParentId} = Data) ->
	ParentId ! Event,
	{next_state, State, Data};
handle_info(Msg, StateName, StateData) ->
	?log("Unexpected event ~w in state ~w : ~w~n", [Msg, StateName, StateData]),
	{next_state, StateName, StateData}.

% @doc Code hot swapping callback (does nothing now).
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

% @doc No real cleanup upon game end
terminate(_Reason, _StateName, _State) ->
	ok.

% @doc Limbo state when no game is going on and user has not requested to join a game
idle({seek, #seek{} = Seek}, _From, State)  ->
	?log("Player seek : ~w~n", [Seek]),
	Reply = c4_game_master:seek( Seek#seek{pid=self()}),
	case Reply of
		{new_game, #game_info{} = GameInfo, Turn, Color} ->
			?log("New game started right away~n", []),
			new_game(GameInfo, Turn, Color, State);
		seek_pending -> 
			?log("Player will have to wait for another~n", []),
			{reply, seek_pending, waiting_for_game, State}
	end;
idle({new_game, #game_info{pid=GamePid}, _Turn, _Color} = Event, _From, #state{parent=ParentId} = State) ->
	?log("Sending game started notification to user : ~w~n", [Event]),
	ParentId ! Event,
	{reply, ok, idle, State#state{game=GamePid}};
idle({seek_issued, #seek{}} = Event, _From, #state{parent=ParentId} = State) ->
	?log("Sending seek issued notification to user : ~w~n", [Event]),
	ParentId ! Event,
	{reply, ok, idle, State};
idle(Event, _From, State) ->
	?log("Unexpected message while idle : ~w~n", [Event]),
	{reply, {error, bad_cmd, "Join a game first"}, idle, State}.

% @doc Waiting for game coordinator to join a game
% or for the user to cancel the request
waiting_for_game({new_game, #game_info{} = GameInfo, Turn, Color} = Event, _From, #state{parent=ParentId} = State) ->
	?log("New game started: ~w~n", [Event]),
	ParentId ! {new_game, GameInfo#game_info{pid=none}, Turn, Color},
	new_game(GameInfo, Turn, Color, State);
waiting_for_game({cancel_seek}, _From, State) ->
	?log("Player wants to cancel seek request~n", []),
	case c4_game_master:cancel_seek(self()) of
		join_canceled -> {reply, join_canceled, idle, State};
		% Game started, should receive game started message soon
		no_join_pending -> {reply, no_join_pending, waiting_for_game, State}
	end;
waiting_for_game(Event, _From, State) ->
	?log("Unexpected message while waiting for game : ~w~n", [Event]),
	{reply, {error, bad_cmd, "Waiting for a game now"}, waiting_for_game, State}.

% @doc Waiting for this player to move state.
% @todo Map input game id to game, ignoring now assuming single game.
my_turn({play, GameId, Col}, {ParentPid, _Tag}, #state{game=GamePid, parent=ParentPid} = State) ->
	?log("Player played game ~w column ~w~n", [GameId, Col]),
	case c4_game:play(GamePid, self(), Col) of
		invalid_move -> {reply, {error, invalid_move, <<"Invalid Move">>}, my_turn, State};
		not_your_turn -> {reply, {error, not_your_turn, <<"Wait for your turn to move">>}, my_turn, State};
		ok -> {reply, play_ok, other_turn, State};
		you_win -> {reply, you_win, idle, State#state{game=none}}
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
	{reply, ok, idle, State#state{game=none}};
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
new_game(#game_info{pid=GamePid} = GameInfo, Turn, Color, State) ->
	NextState = case Turn of your_turn -> my_turn; other_turn->other_turn;wait->waiting_for_reconnect end,
	{reply, {new_game, GameInfo#game_info{pid=none}, Turn, Color}, NextState, State#state{game=GamePid}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

i2b_test() ->
	?assertEqual(<<"34">>, i2b(34)),
	?assertEqual(<<"4">>, i2b(4)),
	?assertEqual(<<"0">>, i2b(0)),
	?assertEqual(<<"560">>, i2b(560)),
	?assertEqual(<<"388534">>, i2b(388534)).

parse_board_size_test() ->
	?assertEqual(#board_size{cols=7,rows=6}, parse_board_size(<<"07x06">>)),
	?assertEqual(#board_size{cols=8,rows=7}, parse_board_size(<<"08x07">>)),
	?assertEqual(#board_size{cols=9,rows=7}, parse_board_size(<<"09x07">>)),
	?assertEqual(#board_size{cols=10,rows=7}, parse_board_size(<<"10x07">>)).

parse_seek_test() ->
	?assertEqual(#seek{board_size=#board_size{cols=7,rows=6},game_var=std,game_type=anon}, parse_seek(<<"SEEK ANON STD 07x06">>)).

player_test() ->
	{ok, _GMId} = c4_game_master:start(),
	{ok, _PMId} = c4_player_master:start(),
	{ok, P1} = c4_player:start(),
	{ok, P2} = c4_player:start(),
	?debugMsg("Issuing first seek"),
	?assertEqual(seek_pending, c4_player:text_cmd(P1, <<"SEEK ANON STD 07x06">>)),
	receive
		{seek_issued, #seek{id=SeekId}} = Msg1 -> 
			?debugFmt("Received ~w", [Msg1]),
			?assertEqual(<<"SEEK_ISSUED 7x6 STD ", (i2b(SeekId))/binary>>, c4_player:text_reply(Msg1))
	after
		1000 -> SeekId = none, ?assert(no_seek_issued)
	end,
	?assertEqual([#seek{game_var=std, id=SeekId, game_type=anon, board_size=#board_size{cols=7,rows=6}}], c4_game_master:seek_list()),
	?debugMsg("Issuing second seek"),
	R1 = c4_player:text_cmd(P2, <<"SEEK ANON STD 07x06">>), 
	?assertMatch({new_game, #game_info{} , other_turn, 2}, R1),
	?debugFmt("R1 = ~w", [R1]),
	{_, #game_info{id=GameId}, _, _} = R1,
	GIdStr = list_to_binary(io_lib:format("~6..0B", [GameId])),
	?debugFmt("Using Game Id ~s", [GIdStr]),
	receive
		{new_game, #game_info{}, your_turn, 1} = Msg -> 
			?debugFmt("Received ~w", [Msg]),
			?assertMatch(<<"NEW_GAME ", _G:?ISIZE/binary, " ", _P:?ISIZE/binary, " STD 7x6 Y 1">>, c4_player:text_reply(Msg))
	after
		1000 -> ?assert(false)
	end,
	?assertEqual(play_ok, c4_player:play(P1, GameId, 1)), 
	?assertEqual(play_ok, c4_player:play(P2, GameId, 2)), 
	?assertEqual(play_ok, c4_player:play(P1, GameId, 1)), 
	?assertEqual(play_ok, c4_player:play(P2, GameId, 2)), 
	?assertEqual(play_ok, c4_player:play(P1, GameId, 1)), 
	?assertEqual(play_ok, c4_player:play(P2, GameId, 2)), 
	?debugMsg("And now in text"),
	?assertEqual(you_win, c4_player:text_cmd(P1, <<"PLAY ", GIdStr/binary," 1">>)), 
	?debugMsg("First player won. Ship it!"),
	?debugMsg("We finished. Shutting player processes"),
	c4_player:stop(P1),
	c4_player:stop(P2),
	?debugMsg("Shutting master processes"),
	c4_game_master:stop(),
	c4_player_master:stop().
-endif.
