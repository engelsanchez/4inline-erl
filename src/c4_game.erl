%% Description: Process that handles game play, receives commands from player processes
%% TODO: Consider moving board code out (to c4_board)
%% For boards, row 0 is bottom, column 0 is left.
-module(c4_game).
-behaviour(gen_fsm).
-export([init/1, playing/3, terminate/3]).
-export([start/1, start_link/1, play/3, quit/2]).

% Number of pieces required to be in a line for a win.
-define(NUM_INLINE, 4).
% Returns the piece value on the given row,column
-define(piece(Board, Row, Col),element(Col, element(Row, Board))).
-define(num_rows(Board), erlang:tuple_size(Board)).
-define(num_cols(Board), erlang:tuple_size(element(1, Board))).
-record(state, {p1, p1ref, color1, p2, p2ref, color2, board}).
-include("c4_common.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
% @doc Starts game process
start(Args) ->
	gen_fsm:start(?MODULE, Args, []).

% @doc Starts game process
start_link(Args) ->
	gen_fsm:start_link(?MODULE, Args, []).

% @doc Player drops a piece
play(GamePid, PlayerPid, Col) ->
	gen_fsm:sync_send_event(GamePid, {play, PlayerPid, Col}).

quit(GamePid, PlayerPid) ->
	gen_fsm:sync_send_event(GamePid, {quit, PlayerPid}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FSM functions

init({P1, P2, Nr, Nc}) when is_pid(P1), is_pid(P2), is_integer(Nr), is_integer(Nc), Nr > 0, Nc > 0  ->
	?log("Starting~n", []),
	P1Ref = monitor(process, P1),
	P2Ref = monitor(process, P2),
	{ok, playing, #state{p1=P1, p1ref=P1Ref, color1=first, p2=P2, p2ref=P2Ref, color2=second, board=board(Nr, Nc)}}.

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
					{stop, {win, color1}, you_win, State#state{board=NewBoard}};
				false -> 
					c4_player:played(P2, self(), Col, your_turn),
					{reply, ok, playing, turn_change(State#state{board=NewBoard})}
			end;
		invalid_move -> 
			{reply, invalid_move, playing, State}
	end;
playing({quit, P1}, _From, #state{p1=P1, p2=P2} = State) ->
	c4_player:other_quit(P2, self()),
	{stop, player_quit, ok, State};
playing({quit, P2}, _From, #state{p1=P1, p2=P2} = State) ->
	c4_player:other_quit(P1, self()),
	{stop, player_quit, ok, State};
playing(Event, _From, State) ->
	?log("Unexpected event ~w~n", [Event]),
	{reply, {error, bad_cmd, "Invalid commnad"}, playing, State}.


% @doc No real cleanup upon game end
terminate(_Reason, _StateName, _State) ->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions

% @doc Switches players around in state data.
turn_change(#state { p1=P1, p1ref=P1Ref, color1=Color1, p2=P2, p2ref=P2Ref, color2=Color2} = State) ->
	State#state{p1=P2, p1ref=P2Ref, color1=Color2, p2=P1, p2ref=P1Ref, color2=Color1}.

% @doc Returns a game board as a tuple containing Nr row tuples 
% each with Nc columns (all zeroes).
board(Nr, Nc) ->
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
		?_assertEqual({{0,0},{0,0}}, board(2,2)),
		?_assertEqual({{0,0,0},{0,0,0},{0,0,0}}, board(3,3))
	].

add_piece_test_() ->
	[
		?_assertEqual({ok, {{1,0},{0,0}},1} , add_piece(board(2,2), 1, 1)),
		?_assertEqual({ok, {{0,1,0},{0,0,0},{0,0,0}},1} , add_piece(board(3,3), 1, 2))
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
