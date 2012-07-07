% @doc Protocol independent interface for remote 4 in-line players.
% The functions that handle actions by our player are 
% {@link seek/1}, {@link cancel_seek/1},
% {@link play/2} and {@link quit_game/1}.
% The functions that handle events from the other player are
% {@link joined/3}, {@link other_played/4} and {@link other_quit/2}.
% The rest are callback functions as this modules implements the generic
% OTP gen_fsm (State machine) behavior.

-module(c4_player).
-behaviour(gen_fsm).
% FSM callback exports
-export([init/1, idle/3, handle_event/3, handle_sync_event/4, handle_info/3, 
		 waiting_for_reconnect/3, my_turn/3, other_turn/3, terminate/3, code_change/4]).
% Public API exports
-export([start/1, start_link/1, text_cmd/2, text_reply/1, get_state/1,
	seek/2, cancel_seek/1, cancel_seek/2, accept_seek/2, joined/4, play/3, 
	other_played/4, quit_game/2, other_quit/2, other_returned/2, 
	other_disconnected/2, disconnected/1, reconnected/2, game_started/2, 
	seek_issued/2, seek_removed/2, quit/1]).
-include("c4_common.hrl").
-record(state, {
		game_pid = none  :: none | pid(), 
		game_id = none	:: none | pos_integer(), 
		parent = none :: none | pid(),
		tref = none :: none | reference(),
		prev_state = idle :: atom()
		}).


-define(ISIZE, 6). % Width of zero padded integers in input text messages.
-define(DISCONNECT_TIMEOUT, 60000). % Time to wait for player to reconnect

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Starts unsupervised c4_player process
start(Args) ->
	gen_fsm:start(?MODULE, Args, []).

% @doc Starts c4_player process supervised by and linked to current process
start_link(Args) ->
	gen_fsm:start_link(?MODULE, Args, []).

% @doc Reads board dimensions from text (07x06,08x07,etc).
parse_board_size(<<W1:8/integer, W2:8/integer, "x", H1:8/integer, H2:8/integer>>) 
		when W1 >= $0, W1 =< $9, W2 >= $0, W2 =< $9, H1 >= $0, H1 =< $9, H2 >= $0, H2 =< $9 ->
	#board_size{rows= (H1-$0)*10+(H2-$0), cols=(W1-$0)*10+(W2-$0)};
parse_board_size(_) -> undefined.

% @doc Parses a seek command
-spec(parse_seek(binary()) -> #seek{} | invalid_command).
parse_seek(<<"SEEK ", GType:4/binary, " C4 ", GVar:3/binary, " ", BSpec:5/binary>>) ->
	BoardSpec = parse_board_size(BSpec),
	GameVar = case GVar of <<"STD">> -> std; <<"POP">> -> pop; _ -> undefined end,
	GameType = case GType of <<"PRIV">> -> priv; <<"ANON">> -> anon; _ -> undefined end,
	if
		BoardSpec == undefined;GameVar == undefined;GameType == undefined -> invalid_command;
		true -> #seek{board_size=BoardSpec, variant=GameVar, type=GameType}
	end;
parse_seek(_) -> invalid_command.

% @doc Simple conversion of integer to binary string.
i2b(I) -> list_to_binary(integer_to_list(I)).

% @doc Integer to string conversion with left zero padding.
i2b(I,Pad) -> list_to_binary(io_lib:format("~" ++  integer_to_list(Pad) ++ "..0B", [I])).

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
text_cmd(Pid, <<"PLAY ", GameId:?ISIZE/binary, " DROP ", Col/binary>>) ->
	c4_player:play(Pid, b2i(GameId), {drop, b2i(Col)});
text_cmd(Pid, <<"QUIT_GAME ", GameId:?ISIZE/binary>>) ->
	c4_player:quit_game(Pid, b2i(GameId));
text_cmd(Pid, <<"QUIT">>) ->
	c4_player:quit(Pid);
text_cmd(Pid, <<"SEEK ", _Rest/binary>> = Cmd) ->
	case parse_seek(Cmd) of
		invalid_command -> invalid_command;
		#seek{} = Seek -> c4_player:seek(Pid, Seek)
	end;
text_cmd(_Pid, _) when is_pid(_Pid) ->
	invalid_command.

% @doc Translates a reply to a text command into text for sending to client.
text_reply({new_game, #game_info{id=GameId, variant=Var, board_size=#board_size{rows=H,cols=W}}, Turn, Color}) ->
	<<"GAME ", (i2b(GameId))/binary, " C4 ", (var2txt(Var))/binary, " ", (i2b(W))/binary, "x", (i2b(H))/binary, " ",
		(turnc(Turn)):8/integer, " ", (i2b(Color))/binary, " NEW">>;
text_reply({game, #game_state{id=GameId, variant=Var, 
							  board_size=#board_size{rows=H,cols=W}, 
							  board=Board, turn=Turn, color=Color}}) ->
	<<"GAME ", (i2b(GameId))/binary, " C4 ", (var2txt(Var))/binary, " ", (i2b(W))/binary, "x", (i2b(H))/binary, " ",
		(turnc(Turn)):8/integer, " ", (i2b(Color))/binary, " BOARD ",
	  (list_to_binary(io_lib:format("~w", [Board])))/binary >>;
text_reply({seek_removed, SeekId}) ->
	<<"SEEK_REMOVED ", (i2b(SeekId))/binary>>;
text_reply({duplicate_seek, SeekId}) ->
	<<"DUPLICATE_SEEK ", (i2b(SeekId))/binary>>;
text_reply({seek_issued, #seek{id=SeekId, board_size=#board_size{rows=H,cols=W}, variant=Var}}) ->
	<<"SEEK_ISSUED ", (i2b(SeekId))/binary, " C4 ", (var2txt(Var))/binary, " ", (i2b(W))/binary, "x", (i2b(H))/binary>>;
text_reply({seek_pending, #seek{id=SeekId,variant=Var,board_size=#board_size{rows=H,cols=W}}}) ->
	<<"SEEK_PENDING ", (i2b(SeekId))/binary, " C4 ", (var2txt(Var))/binary, " ", (i2b(W))/binary, "x", (i2b(H))/binary>>;
text_reply({other_played, GameId, {drop, Col}}) when is_integer(GameId), is_integer(Col) ->
	<<"OTHER_PLAYED ", (i2b(GameId))/binary, " DROP ", (i2b(Col))/binary>>;
text_reply({other_played_no_moves, GameId, {drop, Col}}) when is_integer(GameId), is_integer(Col) ->
	<<"OTHER_NO_MOVES ", (i2b(GameId))/binary, "DROP ", (i2b(Col))/binary>>;
text_reply({other_won, GameId, {drop, Col}}) when is_integer(Col) ->
	<<"OTHER_WON ", (i2b(GameId))/binary, " DROP ", (i2b(Col))/binary>>;
text_reply({other_disconnected, GameId}) ->
	<<"OTHER_DISCONNECTED ", (i2b(GameId))/binary>>;
text_reply({other_returned, GameId}) ->
	<<"OTHER_RETURNED ", (i2b(GameId))/binary>>;
text_reply({you_win, GameId, {drop, Col}}) when is_integer(GameId) ->
	<<"YOU_WIN ", (i2b(GameId))/binary, " DROP ", (i2b(Col))/binary>>;
text_reply({play_ok, GameId, {drop, Col}}) when is_integer(GameId) ->
	<<"PLAY_OK ", (i2b(GameId))/binary, " DROP ", (i2b(Col))/binary>>;
text_reply({invalid_move, GameId}) when is_integer(GameId) ->
	<<"INVALID_MOVE ", (i2b(GameId))/binary>>;
text_reply({leaving_game, GameId}) when is_integer(GameId) ->
	<<"LEAVING_GAME ", (i2b(GameId))/binary>>;
text_reply({no_game, GameId}) when is_integer(GameId) ->
	<<"NO_GAME ", (i2b(GameId))/binary>>;
text_reply({other_quit, GameId}) when is_integer(GameId) ->
	<<"OTHER_QUIT ", (i2b(GameId))/binary>>;
text_reply({seek_canceled, SeekId}) ->
	<<"SEEK_CANCELED ", (i2b(SeekId))/binary>>;
text_reply({no_seek_found, SeekId}) ->
	<<"NO_SEEK_FOUND ", (i2b(SeekId))/binary>>;
text_reply(Reply) ->
	case Reply of
		no_games -> <<"NO_GAMES">>;
		seek_canceled -> <<"SEEK_CANCELED">>;
		no_seek_found -> <<"NO_SEEK_FOUND">>;
		ok_quit -> <<"OK_QUIT">>;
		invalid_command -> <<"INVALID_COMMAND">>;
		{error, _Code, ErrMsg} -> <<"ERROR ", ErrMsg/binary>>;
		_ -> ?log("Unexpected reply ~w", [Reply]), <<"INTERNAL_ERROR">>
	end.


% @doc Player requests to join a game.
% Returns :  join_ack | {new_game, Turn} | {error, Error, ErrorMsg}.
seek(Pid, #seek{} = Seek) ->
	gen_fsm:sync_send_event(Pid, {seek, Seek}, ?INTERNAL_TIMEOUT).

% @doc Impatient player requests to forget about joining a game.
-spec(cancel_seek(pid()) -> seek_canceled | no_seek_pending).
cancel_seek(Pid) ->
	gen_fsm:sync_send_event(Pid, {cancel_seek}, ?INTERNAL_TIMEOUT).

% @doc Impatient player requests to forget about joining a game.
-spec(cancel_seek(pid(), seek_id()) -> {seek_canceled, seek_id()} | {no_seek_pending, seek_id()}).
cancel_seek(Pid, SeekId) ->
	gen_fsm:sync_send_event(Pid, {cancel_seek, SeekId}, ?INTERNAL_TIMEOUT).

% @doc Player request to accept a pending seek.
accept_seek(Pid, SeekId) ->
	gen_fsm:sync_send_event(Pid, {accept_seek, SeekId}, ?INTERNAL_TIMEOUT).

% @doc Called when paired with another player for a game
-spec(joined(pid(), #game_info{}, turn(), 1|2) -> {new_game, #game_info{}, turn(), 1|2}).
joined(Pid, GameInfo, Turn, Color) ->
	gen_fsm:sync_send_event(Pid, {new_game, GameInfo, Turn, Color}, ?INTERNAL_TIMEOUT).

% @doc Executes a move for this player
% Returns : play_ok | you_win | {error, ErrorCode, ErrorMsg}
play(Pid, GameId, Move) ->
	gen_fsm:sync_send_event(Pid, {play, GameId, Move}, ?INTERNAL_TIMEOUT).

% @doc Notifies this player of a move by the other player.
other_played(Pid, GamePid, Move, Status) ->
	gen_fsm:sync_send_event(Pid, {other_played, GamePid, Move, Status}, ?INTERNAL_TIMEOUT).

% @doc Called by our player to leave a game.
-spec(quit_game(pid(), game_id()) -> {no_game, game_id()} | {leaving_game, game_id()}).
quit_game(Pid, GameId) ->
	gen_fsm:sync_send_all_state_event(Pid, {quit_game, GameId}, ?INTERNAL_TIMEOUT).

% @doc Called by our player to leave a game.
-spec(quit(pid) -> ok_quit).
quit(Pid) ->
	gen_fsm:sync_send_all_state_event(Pid, quit, ?INTERNAL_TIMEOUT).

% @doc Should be called when the other player just quit the game.
-spec(other_quit(pid(), pid()) -> ok | no_game).
other_quit(Pid, GamePid) ->
	gen_fsm:sync_send_all_state_event(Pid, {other_quit, GamePid}, ?INTERNAL_TIMEOUT).

% @doc Called when the player has been disconnected
-spec(disconnected(pid()) -> ok).
disconnected(Pid) when is_pid(Pid) ->
	gen_fsm:sync_send_all_state_event(Pid, disconnected, ?INTERNAL_TIMEOUT).

% @doc Called when the player has been disconnected
-spec(reconnected(pid(), pid()) -> ok).
reconnected(Pid, ParentPid) ->
	gen_fsm:sync_send_all_state_event(Pid, {reconnected, ParentPid}, ?INTERNAL_TIMEOUT).

% @doc Alerts our parent that the other player in a game has disconnected.
-spec(other_disconnected(pid(), pid()) -> ok).
other_disconnected(Pid, GamePid) when is_pid(Pid) ->
	gen_fsm:sync_send_all_state_event(Pid, {other_disconnected, GamePid}, ?INTERNAL_TIMEOUT).

% @doc Alerts our handler that the other player in a game has reconnected.
other_returned(Pid, GamePid) when is_pid(Pid), is_pid(GamePid) ->
	gen_fsm:sync_send_event(Pid, {other_returned, GamePid}, ?INTERNAL_TIMEOUT).

% @doc Sends a notification to the player that a game started.
game_started(Pid, GameId) ->
	gen_fsm:sync_send_all_state_event(Pid, {game_started, GameId}, ?INTERNAL_TIMEOUT).

% @doc Asynchronously sends a notification to the player that a seek has been issued,
% no matter what state we are in. The client should ignore them if not expecting
% them at the moment.
-spec(seek_issued(pid(), #seek{}) -> ok).
seek_issued(Pid, #seek{} = Seek) ->
	gen_fsm:send_all_state_event(Pid, {seek_issued, Seek}).

% @doc Sends asynchronous notification of a seek removal to caller process.
-spec(seek_removed(pid(), pos_integer()) -> ok).
seek_removed(Pid, SeekId) ->
	gen_fsm:send_all_state_event(Pid, {seek_removed, SeekId}).

% @doc Returns the state information of the player process as a tuple
% {StateName, Data}
-spec(get_state(pid()) -> {string(), #state{}}).
get_state(Pid) ->
	gen_fsm:sync_send_all_state_event(Pid, get_state, ?INTERNAL_TIMEOUT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FSM functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Initializes state machine to the idle state
init(ParentPid) ->
	?log("Starting attached to caller ~w", [ParentPid]),
	monitor(process, ParentPid),
	{ok, idle, #state{parent=ParentPid}}.

% @doc Handles seek notifications which are asynchronous to minimize blocking when
% doing them in bulk.
% Generic FSM callback for asynchronous events.
handle_event({seek_issued, #seek{} = Seek}, State, #state{parent=ParentPid} = Data) ->
	?log("Notifying player of seek issued ~w", [Seek]),
	do_seek_issued(Seek, ParentPid),
	{next_state, State, Data};
handle_event({seek_removed, SeekId}, State, #state{parent=ParentPid} = Data) ->
	?log("Notifying player of seek removed ~w", [SeekId]),
	do_seek_removed(SeekId, ParentPid),
	{next_state, State, Data};
handle_event(Event, StateName, Data) ->
	?log("Unexpected event in state ~w : ~w ~w", [StateName, Event, Data]),
	{ok, StateName, Data}.

% @doc Handles player disconnect, game quit and get state requests.
% Generic callback for synchronous events for all states.
handle_sync_event(disconnected, _From, State, Data) ->
	?log("Player has disconnected", []),
	NewData = do_disconnected(Data),
	{reply, ok, State, NewData};
handle_sync_event({reconnected, ParentPid}, From, 
				  State, #state{game_pid=GamePid, game_id=GameId} = Data) ->
	?log("Yay! Our player has reconnected ~w", [ParentPid]),
	NewData = do_reconnected(ParentPid, Data),
	% Send current information to new parent: current game if any
	% Assuming that parent is blocking in the reconnect call,
	% These messages should arrive to it after reconnection is complete.
	gen_server:reply(From, ok),
	Self = self(),
	if
		is_pid(GamePid) ->
			GameState = c4_game:game_state(GamePid, self()),
			ParentPid!{game, GameState#game_state{id=GameId}};
		true ->
			ParentPid!no_games,
			% Send seeks to player now
			SeekList = c4_game_master:seek_list(),
			?log("Notifying parent ~w of current seeks ~w", [ParentPid, SeekList]),
			lists:foreach(
				fun(#seek{pid=SeekerPid, id=SeekId, variant=Var,board_size=BoardSize} = Seek) ->
					case SeekerPid of
						Self -> 
							ParentPid ! 
								{
								 seek_pending, 
								 #seek{id=SeekId,
									   variant=Var, 
									   board_size=BoardSize}
								};
						_ -> 
							do_seek_issued(Seek, ParentPid)
					end
				end, SeekList)
	end,
	{next_state, State, NewData};
handle_sync_event({other_disconnected, GamePid}, _From, State, 
				  #state{game_pid=GamePid, game_id=GameId, parent=ParentPid} = Data)
  when is_pid(GamePid) ->
	?log("Other player has disconnected", []),
	if is_pid(ParentPid) -> ParentPid!{other_disconnected, GameId}; true->ok end,
	{reply, ok, waiting_for_reconnect, Data#state{prev_state = State}};
handle_sync_event({quit_game, GameId}, From, _StateName, 
				  #state{game_id = GameId, game_pid=GamePid, parent=ParentPid} = Data) 
  when is_pid(GamePid) ->
	?log("Player is quitting the current game", []),
	c4_game:quit(GamePid, self()),
	gen_fsm:reply(From, {leaving_game, GameId}),
	% @todo When multiple games are allowed, we will only notify when all games are finished.
	SeekList = c4_game_master:register_for_seeks(self()),
	do_seek_issued(SeekList, ParentPid),
	{next_state , idle, Data#state{game_pid=none, game_id=none}};
handle_sync_event({quit_game, GameId}, _From, _StateName, Data) ->
	?log("Received quit_game, but not current game ~w", [GameId]),
	{reply, {no_game, GameId}, idle, Data};
handle_sync_event(quit, _From, _StateName, #state{game_pid=GamePid} = Data) ->
	?log("Player sent quit message, going down", []),
	Self = self(),
	c4_player_master:player_quit(Self),
	if is_pid(GamePid) -> c4_game:quit(GamePid, Self); true -> ok end,
	c4_game_master:cancel_seek(Self),
	{stop, normal, ok_quit, Data};
handle_sync_event({other_quit, GamePid}, _From, _State, 
				  #state{game_pid=GamePid, game_id=GameId, parent=ParentPid} = Data) 
  when is_pid(GamePid) ->
	?log("Other player quit, notifying caller process", []),
	if is_pid(ParentPid)->ParentPid!{other_quit, GameId};true->ok end,
	% @todo When multiple games are allowed, we will only notify when all games are finished.
	SeekList = c4_game_master:register_for_seeks(self()),
	do_seek_issued(SeekList, ParentPid),
	{reply, ok, idle, Data#state{game_id=none, game_pid=none}};
handle_sync_event(get_state, _From, StateName, State) ->
	{reply, {StateName, State}, StateName, State};
handle_sync_event(Event, _From, StateName, Data) ->
	?log("Unexpected event in state ~w : ~w ~w", [StateName, Event, Data]),
	{reply, invalid_command, StateName, Data}.

% @doc Handles death of our caller process (user disconnection). 
% FSM miscellaneous event handling callback.
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State, #state{parent=Pid} = Data) ->
	?log("Calling process has died, going into disconnected mode", []),
	NewData = do_disconnected(Data),
	{next_state, State, NewData};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State, Data) ->
	?log("Ignoring process down message from ~w", [Pid]),
	{next_state, State, Data};
handle_info({timeout, TRef, player_disconnected}, _State, #state{tref=TRef} = Data) ->
	?log("Player disconnection timed out, going down for good", []),
	c4_game_master:cancel_seek(self()),
	{stop, normal, Data#state{tref=none}};
handle_info(Msg, StateName, Data) ->
	?log("Unexpected event ~w in state ~w : ~w", [Msg, StateName, Data]),
	{next_state, StateName, Data}.

% @doc Code hot swapping callback (does nothing now).
code_change(_OldVsn, StateName, Data, _Extra) ->
	{ok, StateName, Data}.

% @doc No real cleanup when player process dies.
terminate(_Reason, _StateName, _State) ->
	ok.

% @doc Non-game state.
idle({seek, Seek}, _From, State)  ->
	do_seek(Seek, State);
idle({accept_seek, SeekId}, _From, State)  ->
	?log("Player wants to accept seek : ~w", [SeekId]),
	Reply = c4_game_master:accept_seek(SeekId),
	case Reply of
		{new_game, #game_info{} = GameInfo, Turn, Color} ->
			?log("New game started right away", []),
			new_game(GameInfo, Turn, Color, State);
		seek_not_found -> 
			?log("Bad seek id or already taken", []),
			{reply, {seek_not_found, SeekId}, idle, State}
	end;
idle({cancel_seek}, _From, State) ->
	?log("Player wants to cancel all seek requests", []),
	Reply = c4_game_master:cancel_seek(self()),
	{reply, Reply, idle, State};
idle({cancel_seek, SeekId}, _From, State) when is_integer(SeekId) ->
	?log("Player wants to cancel seek ~w", [SeekId]),
	Reply = c4_game_master:cancel_seek(self(), SeekId),
	{reply, {Reply, SeekId}, idle, State};
idle({new_game, #game_info{} = GameInfo, Turn, Color} = Event, _From, #state{parent=ParentId} = State) ->
	?log("New game started: ~w", [Event]),
	case ParentId of
		none -> ok;
		_ -> ParentId ! {new_game, GameInfo#game_info{pid=none}, Turn, Color}
	end,
	new_game(GameInfo, Turn, Color, State);
idle(Event, _From, State) ->
	?log("Unexpected message while idle : ~w ~w", [Event, State]),
	{reply, {error, bad_cmd, "Join a game first"}, idle, State}.

% @doc Waiting for this player to move state.
% @todo Map input game id to game, ignoring now assuming single game.
my_turn({play, GameId, Move}, {ParentPid, _Tag} = From, #state{game_pid=GamePid, parent=ParentPid} = State) ->
	?log("Player played game ~w  ~w", [GameId, Move]),
	case c4_game:play(GamePid, self(), Move) of
		invalid_move -> {reply, {error, invalid_move, <<"Invalid Move">>}, my_turn, State};
		not_your_turn -> {reply, {error, not_your_turn, <<"Wait for your turn to move">>}, my_turn, State};
		ok -> {reply, {play_ok, GameId, Move}, other_turn, State};
		you_win ->
			gen_fsm:reply(From, {you_win, GameId, Move}),
			% @todo When multiple games are allowed, we will only notify when all games are finished.
			SeekList = c4_game_master:register_for_seeks(self()),
			do_seek_issued(SeekList, ParentPid),
			{next_state, idle, State#state{game_pid=none}}
	end;
my_turn(Event, _From, State) ->
	?log("Unexpected message while waiting for player to move ~w ~w", [Event, State]),
	{reply, {error, bad_cmd, "Waiting for player move"}, my_turn, State}.

% @doc Waiting for other player to move state.
other_turn({other_played, GamePid, Move, your_turn}, _From, #state{game_pid=GamePid, game_id=GameId, parent=PPid} = State) 
  when is_pid(GamePid), is_pid(PPid) ->
	?log("Other player played  ~w", [Move]),
	PPid ! {other_played, GameId, Move},
	{reply, ok, my_turn, State};
other_turn({other_played, GamePid, Move, no_moves}, _From, #state{game_pid=GamePid, game_id=GameId, parent=PPid} = State) 
  when is_pid(GamePid), is_pid(PPid) ->
	?log("Other player played and board full ~w", [Move]),
	PPid ! {other_played_no_moves, GameId, Move},
	{reply, ok, my_turn, State#state{game_pid=none}};
other_turn({other_played, GamePid, Move, you_lose}, _From, #state{game_pid=GamePid, game_id=GameId, parent=PPid} = State) 
  when is_pid(GamePid), is_pid(PPid) ->
	?log("Other player played ~w and wins", [Move]),
	PPid ! {other_won, GameId, Move},
	% @todo When multiple games are allowed, we will only notify when all games are finished.
	SeekList = c4_game_master:register_for_seeks(self()),
	do_seek_issued(SeekList, PPid),
	{reply, ok, idle, State#state{game_pid=none}};
other_turn(Event, _From, State) ->
	?log("Unexpected message while waiting for other player to move ~w ~w", [Event, State]),
	{reply, {error, bad_cmd, "Waiting for other player to move"}, other_turn, State}.

% @doc Waiting for other player to reconnect.
waiting_for_reconnect({other_returned, GamePid}, _From, 
					  #state{game_pid=GamePid, game_id=GameId, parent=PPid, prev_state=PrevState} = State) ->
	if is_pid(PPid) -> PPid ! {other_returned, GameId}; true->ok end,
	{reply, ok, PrevState, State};
waiting_for_reconnect(Event, _From, State) ->
	?log("Unexpected message while waiting for other player to reconnect ~w ~w", [Event, State]),
	{reply, {error, bad_cmd, "Waiting for other player to reconnect"}, waiting_for_reconnect, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions

% @doc Returns reply when new game started
new_game(#game_info{pid=GamePid, id=GameId} = GameInfo, Turn, Color, State) ->
	NextState = case Turn of your_turn -> my_turn; other_turn->other_turn;wait->waiting_for_reconnect end,
	{reply, {new_game, GameInfo#game_info{pid=none}, Turn, Color}, NextState, State#state{game_pid=GamePid, game_id=GameId}}.

% @doc Notify our current game process of a disconnection
do_disconnected(#state{game_pid=GamePid} = Data) ->
	case GamePid of 
		none -> ok;
		_ -> c4_game:disconnect(GamePid, self())
	end,
	?log("Setting timer to stop player process in ~w seconds", [?DISCONNECT_TIMEOUT/1000]),
	TRef = erlang:start_timer(?DISCONNECT_TIMEOUT, self(), player_disconnected),
	Data#state{parent=none, tref=TRef}.

do_reconnected(ParentPid, #state{game_pid=GamePid, tref=TRef} = Data) when is_pid(ParentPid)->
	if is_pid(GamePid) -> c4_game:reconnect(GamePid, self()); true -> ok end,
	if is_reference(TRef) -> erlang:cancel_timer(TRef); true -> ok end,
	Data#state{parent=ParentPid, tref=none}.

% @doc Forwards seek issued message(s) to controlling process (if any).
do_seek_issued(_Seek, none) ->
	ok;
do_seek_issued(#seek{pid=SeekerId}, _ParentPid) when SeekerId =:= self() ->
	ok;
do_seek_issued(#seek{} = Seek, ParentPid) ->
	?log("Sending seek issued notification to user : ~w ~w", [Seek, ParentPid]),
	ParentPid ! {seek_issued, Seek};
do_seek_issued([], _ParentPid) ->
	ok;
do_seek_issued([#seek{} = Seek | L], ParentPid) ->
	do_seek_issued(Seek, ParentPid),
	do_seek_issued(L, ParentPid).

% @doc Sends our calling process a seek removed message.
do_seek_removed([SeekId | MoreSeeks], ParentId) ->
	do_seek_removed(SeekId, ParentId),
	do_seek_removed(MoreSeeks, ParentId);
do_seek_removed([], _ParentId) ->
	ok;
do_seek_removed(SeekId, ParentPid) ->
	?log("Notifying ~w of removed seek ~w", [ParentPid, SeekId]),
	ParentPid ! {seek_removed, SeekId}.

do_seek(#seek{variant=Var,board_size=BoardSize} = Seek, State) ->
	?log("Player seek : ~w", [Seek]),
	Reply = c4_game_master:seek(Seek#seek{pid=self()}),
	case Reply of
		{new_game, #game_info{} = GameInfo, Turn, Color} ->
			?log("New game started right away", []),
			new_game(GameInfo, Turn, Color, State);
		{seek_pending, SeekId} -> 
			?log("Player will have to wait for another", []),
			{reply, {seek_pending, #seek{id=SeekId, variant=Var, board_size=BoardSize}}, idle, State};
		{duplicate_seek, SeekId} ->
			?log("Silly player issuing the same seek again ~w", [Seek]),
			{reply, {duplicate_seek, SeekId}, idle, State}
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Unit tests
%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

i2b_test() ->
	?assertEqual(<<"34">>, i2b(34)),
	?assertEqual(<<"000034">>, i2b(34,6)),
	?assertEqual(<<"4">>, i2b(4)),
	?assertEqual(<<"0">>, i2b(0)),
	?assertEqual(<<"560">>, i2b(560)),
	?assertEqual(<<"0560">>, i2b(560, 4)),
	?assertEqual(<<"388534">>, i2b(388534)).

parse_board_size_test() ->
	?assertEqual(#board_size{cols=7,rows=6}, parse_board_size(<<"07x06">>)),
	?assertEqual(#board_size{cols=8,rows=7}, parse_board_size(<<"08x07">>)),
	?assertEqual(#board_size{cols=9,rows=7}, parse_board_size(<<"09x07">>)),
	?assertEqual(#board_size{cols=10,rows=7}, parse_board_size(<<"10x07">>)).

parse_seek_test() ->
	?assertEqual(#seek{board_size=#board_size{cols=7,rows=6},variant=std,type=anon}, parse_seek(<<"SEEK ANON C4 STD 07x06">>)).

% @doc Asserts reception of a seek notification and returns the Seek id.
-spec(expect_seek(#seek{}) -> pos_integer()).
expect_seek(#seek{id=InSeekId, board_size=BoardSize, variant=Var, type=Type} = Seek) ->
	?debugFmt("Expecting seek message : ~w", [Seek]),
	receive
		{seek_issued, #seek{id=SeekId, board_size=BoardSize, variant=Var, type=Type}} = Msg1 ->
			?debugFmt("Received ~w", [Msg1]),
			case InSeekId of
				none -> ok;
				_ -> ?assertEqual(InSeekId, SeekId)
			end,
			SeekId
	after
		1000 -> ?assert(no_seek_issued)
	end.

% @doc Asserts reception of a new game notification.
expect_game(GameInfo, Turn, Color) ->
	?debugFmt("Expecting new game message ~w ~w ~w", [GameInfo, Turn, Color]),
	receive
		{new_game, GameInfo, Turn, Color} = Msg -> 
			?debugFmt("Received ~w", [Msg]);
		{new_game, _G2, _T2, _C2} = OMsg ->
			?debugFmt("Received wrong message ~w", [OMsg]),
			?assert(OMsg)
	after
		1000 -> ?assert(no_new_game_msg)
	end.
	
player_test() ->
	error_logger:logfile({open, "c4_server.log"}),
	error_logger:tty(false),
	?debugMsg("Starting Game Master"),
	{ok, _GMId} = c4_game_master:start(),
	?debugMsg("Starting Player Master"),
	{ok, _PMId} = c4_player_master:start(),
	?debugMsg("Starting Player 1"),
	{ok, P1, _P1Str} = c4_player_master:connect(),
	?debugMsg("Starting Player 2"),
	{ok, P2, _P2Str} = c4_player_master:connect(),
	?debugMsg("Issuing first seek"),
	S1 = c4_player:text_cmd(P1, <<"SEEK ANON C4 STD 07x06">>),
	?assertMatch({seek_pending, #seek{variant=std,type=anon,board_size=#board_size{cols=7,rows=6}}}, S1),
	{seek_pending, #seek{id=SeekId1} = Seek1} = S1,
	expect_seek(Seek1), 
	?debugMsg("Player 3 will start, should receive previous seek upon connecting"),
	{ok, P3, _P3Str} = c4_player_master:connect(),
	expect_seek(#seek{id=SeekId1, board_size=#board_size{cols=7,rows=6}, variant=std, type=anon}),
	?debugMsg("Player 3 will issue seek and wait"),
	S2 = c4_player:text_cmd(P3, <<"SEEK ANON C4 STD 08x07">>),
	?assertMatch({seek_pending, #seek{variant=std,type=anon,board_size=#board_size{cols=8,rows=7}}}, S2),
	{seek_pending, #seek{id=SeekId2} = Seek2} = S2,
	expect_seek(Seek2),
	?debugMsg("Checking current seek list"),
	?assertEqual(lists:sort([#seek{pid=P1, variant=std, id=SeekId1, type=anon, board_size=#board_size{cols=7,rows=6}},
			#seek{pid=P3, variant=std, id=SeekId2, type=anon, board_size=#board_size{cols=8,rows=7}}]),
		lists:sort(c4_game_master:seek_list())),
	?debugMsg("Issuing second seek, should match first and start game"),
	R1 = c4_player:text_cmd(P2, <<"SEEK ANON C4 STD 07x06">>), 
	?assertMatch({new_game, #game_info{id=SeekId1, variant=std, type=anon, board_size=#board_size{cols=7,rows=6}} , other_turn, 2}, R1),
	{_, #game_info{id=GameId} = Game1Info, _, _} = R1,
	expect_game(Game1Info, your_turn, 1),
	?debugMsg("Will play the game now"),
	do_moves(GameId, P1, P2, [{drop,1},{drop,2},{drop,1},{drop,2},{drop,1},{drop,2}]),
	?debugMsg("And now, the winning move"),
	do_move(GameId, P1, {you_win, GameId, {drop,1}}, {drop,1}), 
	?debugMsg("First player won. Checking current seek list and reception of current seeks upon game end by both players"),
	?assertEqual([#seek{pid=P3, variant=std, id=SeekId2, type=anon, board_size=#board_size{cols=8,rows=7}}],
		c4_game_master:seek_list()),
	expect_seek(#seek{pid=P3, variant=std, id=SeekId2, type=anon, board_size=#board_size{cols=8,rows=7}}),
	expect_seek(#seek{pid=P3, variant=std, id=SeekId2, type=anon, board_size=#board_size{cols=8,rows=7}}),
	?debugMsg("Now player 1 will accept player 3's seek and play"),
	R2 = c4_player:text_cmd(P1, <<"ACCEPT_SEEK ",(i2b(SeekId2,?ISIZE))/binary>>),
	?assertMatch({new_game, #game_info{id=SeekId2, variant=std, type=anon, board_size=#board_size{cols=8,rows=7}} , other_turn, 2}, R2),
	{_, #game_info{id=Game2Id} = Game2Info, _, _} = R2,
	expect_game(Game2Info, your_turn, 1),
	?debugMsg("Will play game #2 now"),
	do_moves(Game2Id, P3, P1, [{drop,5},{drop,5},{drop,6},{drop,6},{drop,7},{drop,7}]),
	?debugMsg("And now, the winning move"),
	do_move(Game2Id, P3, {you_win, Game2Id, {drop, 8}}, {drop, 8}), 
	?debugMsg("Player 3 won. Checking current seek list and reception of current seeks upon game end by both players"),
	?assertEqual([], c4_game_master:seek_list()),
	?debugMsg("Shutting player processes"),
	c4_player:quit(P1),
	c4_player:quit(P2),
	?debugMsg("Shutting game master process"),
	c4_game_master:stop(),
	?debugMsg("Shutting player master process"),
	c4_player_master:stop().

do_move(GameId, P, Result, Move) ->
	?assertEqual(Result, c4_player:play(P, GameId, Move)),
	ok.

% @doc Helper to perform a bunch of moves in one go.
do_moves(_GameId, _P1, _P2, []) ->
	ok;
do_moves(GameId, P1, P2, [Move | MoreMoves]) ->
	do_move(GameId, P1, {play_ok, GameId, Move}, Move),
	do_moves(GameId, P2, P1, MoreMoves).

%-endif.
