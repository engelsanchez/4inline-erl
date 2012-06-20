%% @doc Game process interface.
%% It receives moves from players and notifies the opponent.
%% It also detects when a game has been won or there are no more 
%% moves.
%% For boards, row 1 is bottom, column 1 is left.
%% @todo Consider moving board code out (to c4_board)
-module(c4_game).
-behaviour(gen_fsm).
% FSM callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, playing/3, disconnected/3, terminate/3, code_change/4]).
% Public API
-export([start/1, start_link/1, play/3, disconnect/2, abandon/1, quit/2, valid_board_size/1]).

% Number of pieces required to be in a line for a win.
-define(NUM_INLINE, 4).
% Returns the piece value on the given row,column
-define(piece(Board, Row, Col),element(Col, element(Row, Board))).
-define(num_rows(Board), erlang:tuple_size(Board)).
-define(num_cols(Board), erlang:tuple_size(element(1, Board))).
-record(state, {p1, id1, p1ref, p1conn=true, color1, p2, id2, p2ref, p2conn=true, color2, board, game_var}).
-include("c4_common.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
% @doc Starts unsupervised game process, mostly for testing.
-spec(start({pid(), pos_integer(), pid(), pos_integer(), #board_size{}, game_var()}) -> {ok, pid()} | ignore | {error, string()}).
start(Args) ->
	gen_fsm:start(?MODULE, Args, []).

% @doc Starts game process linked to the current process.
-spec(start_link({pid(), pos_integer(), pid(), pos_integer(), #board_size{}, game_var()}) -> {ok, pid()} | ignore | {error, string()}).
start_link(Args) ->
	gen_fsm:start_link(?MODULE, Args, []).

% @doc Called when a player makes a move
-spec(play(pid(), pid(), pos_integer()) -> invalid_move | not_your_turn | ok | you_win ).
play(GamePid, PlayerPid, Col) ->
	gen_fsm:sync_send_event(GamePid, {play, PlayerPid, Col}).

% @doc Called when a player has been disconnected (may come back).
-spec(disconnect(pid(), pid()) -> ok).
disconnect(GamePid, PlayerPid) ->
	gen_fsm:sync_send_event(GamePid, {disconnected, PlayerPid}).

% @doc Stops the game
abandon(GamePid) ->
	gen_fsm:sync_send_event(GamePid, {abandon}).

% @doc Called when a player actively quits a game (not just disconnects).
-spec(quit(pid(), pid()) -> ok).
quit(GamePid, PlayerPid) ->
	gen_fsm:sync_send_event(GamePid, {quit, PlayerPid}).

-spec(valid_board_size(#board_size{}) -> boolean()).
valid_board_size(#board_size{rows=Rows, cols=Cols}) when not is_integer(Rows); not is_integer(Cols) -> false;
valid_board_size(#board_size{rows=Rows, cols=Cols}) when Rows < 6; Rows > 7; Cols < 7; Cols > 10 -> false;
valid_board_size(#board_size{rows=Rows, cols=Cols}) when Rows == 6, Cols > 7 -> false;
valid_board_size(#board_size{} = _BoardSize) -> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FSM functions

% @doc Creates the empty board and starts monitoring the player processes.
% FSM initialization callback.
-spec(init({pid(), pos_integer(), pid(), pos_integer(), #board_size{}, game_var()}) -> {ok, playing, #state{}}).
init({P1, Id1, P2, Id2, BoardSize, GameVar}) when is_pid(P1), is_pid(P2)  ->
	?log("Starting~n", []),
	P1Ref = monitor(process, P1),
	P2Ref = monitor(process, P2),
	{ok, playing, #state{p1=P1, id1=Id1, p1ref=P1Ref, color1=first, p2=P2, id2=Id2, p2ref=P2Ref, color2=second, board=board(BoardSize), game_var=GameVar}}.

% @doc Waiting for a move
% returns: 
%   invalid_move
%   not_your_turn
%   ok
%   you_win
% May send message to other player:
%   played, Col, you_lose|your_turn
playing({play, P2, _Col}, _From, #state{p2=P2} = State) ->
	{reply, not_your_turn, playing, State};
playing({play, P1, Col}, _From, #state{p1=P1, color1=Color, p2=P2, board=Board} = State) ->
	?log("Player ~w played ~w~n", [P1, Col]),
	case add_piece(Board, Color, Col) of
		{ok, NewBoard, Row} ->
			?log("Board ~w~n", [NewBoard]),
			case check_win(NewBoard, Row, Col) of
				true ->
					c4_player:played(P2, self(), Col, you_lose), 
					{stop, normal, you_win, State#state{board=NewBoard}};
				false -> 
					c4_player:played(P2, self(), Col, your_turn),
					{reply, ok, playing, turn_change(State#state{board=NewBoard})}
			end;
		invalid_move -> 
			{reply, invalid_move, playing, State}
	end;
playing(Event, _From, State) ->
	?log("Unexpected event ~w~n", [Event]),
	{reply, {error, bad_cmd, "Invalid commnad"}, playing, State}.

% @doc One or both players have disconnected and moves are not allowed.
% Players may reconnect at any point.
disconnected({join, Pid, PlayerId}, _From, #state{p1=P1, id1=Id1, p1conn=P1conn, p2=P2, id2=Id2, p2conn=P2conn} = State) ->
	case PlayerId of
		Id1 ->
			State2 = State#state{p1=Pid,p1conn=true},
			case P2conn of
				true ->
					c4_player:other_returned(P2, self()),
					{reply, {in_game, your_turn}, playing, State2};
				false ->
					{reply, {in_game, wait}, disconnected, State2}
			end;
		Id2 -> 
			State2 = State#state{p2=Pid, p2conn=true},
			case P1conn of
				true ->
					c4_player:other_returned(P1, self()),
					{reply, {in_game, your_turn}, playing, State2};
				false ->
					{reply, {in_game, wait}, disconnected, State2}
			end
	end.


% @doc Handles disconnections or requests to abandon a game.
handle_sync_event({quit, Pid}, _From, _StateName, #state{p1=P1, p2=P2} = State) ->
	c4_player:other_quit(case Pid of P1->P2; P2->P1 end, self()),
	{stop, normal, ok, State};
handle_sync_event({disconnected, Pid}, _From, _StateName, #state{p1=P1, p2=P2} = State) ->
	State2 = case Pid of P1 -> State#state{p1conn=false}; P2 -> State#state{p2conn=false} end,
	{reply, ok, disconnected, State2};
handle_sync_event({abandon}, _From, _StateName, State) ->
	{stop, normal, ok, State}.

handle_event(Event, StateName, StateData) ->
	?log("Unexpected event ~w in state ~w : ~w~n", [Event, StateName, StateData]),
	ok.

handle_info(Event, StateName, StateData) ->
	?log("Unexpected event ~w in state ~w : ~w~n", [Event, StateName, StateData]),
	ok.

% @doc Does nothing as there is no real cleanup needed upon game end.
% Generic FSM termination callback.
terminate(_Reason, _StateName, _State) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions

% @doc Returns a state record with the players swapped 
turn_change(#state { p1=P1, id1=Id1, p1ref=P1Ref, color1=Color1, p2=P2, id2=Id2, p2ref=P2Ref, color2=Color2} = State) ->
	State#state{p1=P2, id1=Id2, p1ref=P2Ref, color1=Color2, p2=P1, id2=Id1, p2ref=P1Ref, color2=Color1}.

% @doc Returns a game board as a tuple containing Nr row tuples 
% each with Nc columns (all zeroes).
board(#board_size{rows=Nr, cols=Nc}) ->
	Row = erlang:make_tuple(Nc, 0),
	erlang:make_tuple(Nr, 0, [{P,Row} || P <- lists:seq(1,Nr)]).

% @doc Drops a piece to the board down the given column, returning
% {ok, NewBoard, Row} if ok
% invalid_move if column full or bad column number
add_piece(Board, _Piece, Col) when Col < 0; Col >= ?num_cols(Board) ->
	invalid_move;
add_piece(Board, Piece, Col) ->
	case free_row(Board, Col) of
		full -> invalid_move;
		Row  -> {ok, set_piece(Board, Row, Col, Piece), Row}
	end.

% @doc returns the row index of the first available slot for a given column.
% Returns full or row index if found.
free_row(Board, Col) ->
	free_row(Board, Col, 1, ?num_rows(Board)).

% @doc recursively find first available slot in column.
free_row(Board, Col, Idx, Nr) ->
	case Idx > Nr of
		true -> full;
		false ->	
			case ?piece(Board, Idx, Col) == 0 of
				true -> Idx;
				false -> free_row(Board, Col, Idx+1, Nr)
			end
	end.

% Changes one piece value in the board
set_piece(Board, Row, Col, Piece) ->
	setelement(Row, Board, setelement(Col, element(Row, Board), Piece)).

% true or false if 4 in line including the given position
% assumed to be the last piece dropped
check_win(Board, Row, Col) ->
	Val = ?piece(Board, Row, Col),
	% Check in all for directions
	check_win(Board, Row, Col, Val, [{-1,1},{0,1},{1,1},{1,0}]).
	
check_win(_Board, _Row, _Col, _Val, []) ->
	false;
check_win(Board, Row, Col, Val, [ {Dr, Dc} | T ]) ->
	case check_win(Board, Row, Col, Val, {Dr, Dc}) of
		true -> true;
		false -> check_win(Board, Row, Col, Val, T)
	end;
check_win(Board, Row, Col, Val, {Dr, Dc}) ->
	count(Board, Row+Dr, Col+Dc, Val, {Dr, Dc})
	+ count(Board, Row-Dr, Col-Dc, Val, {-Dr, -Dc})
	+ 1 >= ?NUM_INLINE.

% @doc Counts the number of pieces of a given value starting
% at a given position and along a given direction.
count(Board, Row, Col, Val, {Dr, Dc}) ->
	count(Board, Row, Col, Val, {Dr, Dc}, 0).

count(Board, Row, Col, _Val, {_Dr, _Dc}, Acc) 
	when
		Row > tuple_size(Board); 
		Row < 1; 
		Col > tuple_size(element(1, Board));
		Col < 1
	-> Acc;
count(Board, Row, Col, Val, {Dr, Dc}, Acc) -> 
	count(Board, Row+Dr, Col+Dc, Val, {Dr, Dc}, 
		Acc + case ?piece(Board, Row, Col) of Val -> 1; _ -> 0 end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit tests
-include_lib("eunit/include/eunit.hrl").

board_test_() ->
	[
		?_assertEqual({{0,0},{0,0}}, board(#board_size{rows=2,cols=2})),
		?_assertEqual({{0,0,0},{0,0,0},{0,0,0}}, board(#board_size{rows=3,cols=3}))
	].

add_piece_test_() ->
	[
		?_assertEqual({ok, {{1,0},{0,0}},1} , add_piece(board(#board_size{rows=2,cols=2}), 1, 1)),
		?_assertEqual({ok, {{0,1,0},{0,0,0},{0,0,0}},1} , add_piece(board(#board_size{rows=3,cols=3}), 1, 2))
	].

set_piece_test_() ->
	[
		?_assertEqual({{0,0},{0,2}}, set_piece({{0,0}, {0,0}}, 2, 2, 2)),
		?_assertEqual({{0,0,0},{0,2,0},{0,0,0}}, set_piece({{0,0,0},{0,0,0},{0,0,0}}, 2, 2, 2))
	].

check_win_test_() ->
	[
		?_assert(check_win({{1,1,1,1},{0,0,0,0},{0,0,0,0},{0,0,0,0}}, 1, 1))
	].

count_test_() ->
	[
		?_assertEqual(4, count({{1,1,1,1},{0,0,0,0},{0,0,0,0},{0,0,0,0}}, 1, 1, 1, {0, 1})),
		?_assertEqual(0, count({{1,1,1,1},{0,0,0,0},{0,0,0,0},{0,0,0,0}}, 1, 1, 2, {0, 1})),
		?_assertEqual(1, count({{1,1,1,1},{0,0,0,0},{0,0,0,0},{0,0,0,0}}, 1, 1, 1, {1, 1})),
		?_assertEqual(1, count({{1,1,1,1},{0,0,0,0},{0,0,0,0},{0,0,0,0}}, 1, 1, 1, {1, 0})),
		?_assertEqual(3, count({{1,1,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,0}}, 1, 1, 1, {1, 1})),
		?_assertEqual(2, count({{1,1,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,0}}, 1, 1, 1, {0, 1})),
		?_assertEqual(1, count({{1,1,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,0}}, 1, 1, 1, {1, 0}))
	].

free_row_test_() ->
	[
		?_assertEqual(1, free_row({{0,0},{0,0}}, 1)),
		?_assertEqual(1, free_row({{0,0},{0,0}}, 2)),
		?_assertEqual(2, free_row({{1,0},{0,0}}, 1)),
		?_assertEqual(1, free_row({{1,0},{0,0}}, 2))
	].

valid_board_size_test_() ->
	[
		?_assertEqual(true, valid_board_size(#board_size{rows=6,cols=7})),
		?_assertEqual(true, valid_board_size(#board_size{rows=7,cols=8})),
		?_assertEqual(true, valid_board_size(#board_size{rows=7,cols=9})),
		?_assertEqual(true, valid_board_size(#board_size{rows=7,cols=10})),
		?_assertEqual(false, valid_board_size(#board_size{rows=5, cols=7})),
		?_assertEqual(false, valid_board_size(#board_size{rows=6, cols=8})),
		?_assertEqual(false, valid_board_size(#board_size{rows=1, cols=3}))
	].
