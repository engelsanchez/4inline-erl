%% @doc 4inline board creation and manipulation.
%% For boards, row 1 is bottom, column 1 is left.
-module(c4_board).
-export([new/1, valid_size/1, add_piece/3, check_win/3, is_full/1]).

% Number of pieces required to be in a line for a win.
-define(NUM_INLINE, 4).
% % Returns the piece value on the given row,column
-define(piece(Board, Row, Col),element(Col, element(Row, Board))).
-define(num_rows(Board), erlang:tuple_size(Board)).
-define(num_cols(Board), erlang:tuple_size(element(1, Board))).
-include("c4_common.hrl").

% @doc Checks if a board size is supported (7x6, 8x7, 9x7, 10x7)
-spec(valid_size(#board_size{}) -> boolean()).
valid_size(#board_size{rows=Rows, cols=Cols}) when not is_integer(Rows); not is_integer(Cols) -> false;
valid_size(#board_size{rows=Rows, cols=Cols}) when Rows < 6; Rows > 7; Cols < 7; Cols > 10 -> false;
valid_size(#board_size{rows=Rows, cols=Cols}) when Rows == 6, Cols > 7 -> false;
valid_size(#board_size{} = _BoardSize) -> true.

% @doc Checks if a board is already full
is_full(Board) ->
	is_full(Board, 1).

is_full(Board, Col) when Col > ?num_cols(Board) -> true;
is_full(Board, Col) ->
	case free_row(Board, Col) of
		full -> is_full(Board, Col+1);
		_ -> false
	end.

% @doc Returns a game board as a tuple containing Nr row tuples 
% each with Nc columns (all zeroes).
-spec(new(#board_size{})->board()).
new(#board_size{rows=Nr, cols=Nc}) ->
	Row = erlang:make_tuple(Nc, 0),
	erlang:make_tuple(Nr, 0, [{P,Row} || P <- lists:seq(1,Nr)]).

% @doc Drops a piece to the board down the given column, returning
% {ok, NewBoard, Row} if ok
% invalid_move if column full or bad column number
-spec(add_piece(board(), term(), pos_integer()) -> invalid_move | {ok, board(), pos_integer()}).
add_piece(Board, _Piece, Col) when Col =< 0; Col > ?num_cols(Board) ->
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
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_full_test() ->
	{ok, B, _} = add_piece(new(#board_size{rows=2,cols=2}),1,1),
	?assertEqual(false, is_full(B)),
	{ok, B2, _} = add_piece(B,1,2),
	?assertEqual(false, is_full(B2)),
	{ok, B3, _} = add_piece(B2,2,1),
	?assertEqual(false, is_full(B3)),
	{ok, B4, _} = add_piece(B3,2,2),
	?assertEqual(true, is_full(B4)).

board_test_() ->
	[
		?_assertEqual({{0,0},{0,0}}, new(#board_size{rows=2,cols=2})),
		?_assertEqual({{0,0,0},{0,0,0},{0,0,0}}, new(#board_size{rows=3,cols=3}))
	].

add_piece_test() ->
	B1 = c4_board:new(#board_size{rows=2,cols=2}),
	?assertEqual(invalid_move, add_piece(B1, 1, 0)),
	?assertEqual(invalid_move, add_piece(B1, 1, 3)),
	?assertEqual({ok, {{1,0},{0,0}},1} , add_piece(B1, 1, 1)),
	{ok, B2, 1} = add_piece(B1, 1, 1),
	?assertEqual({ok, {{1,0},{1,0}}, 2}, add_piece(B2, 1, 1)),
	{ok, B3, 2} = add_piece(B2, 1, 1),
	?assertEqual(invalid_move, add_piece(B3, 1, 1)),
	?assertEqual({ok, {{0,1,0},{0,0,0},{0,0,0}},1} , add_piece(new(#board_size{rows=3,cols=3}), 1, 2)).

set_piece_test_() ->
	[
		?_assertEqual({{0,0},{0,2}}, set_piece({{0,0}, {0,0}}, 2, 2, 2)),
		?_assertEqual({{0,0,0},{0,2,0},{0,0,0}}, set_piece({{0,0,0},{0,0,0},{0,0,0}}, 2, 2, 2))
	].

check_win_test_() ->
	[
		?_assertEqual(true, check_win({{1,1,1,1},{0,0,0,0},{0,0,0,0},{0,0,0,0}}, 1, 1)),
		?_assertEqual(true, check_win({{1,0,1,1},{1,0,0,0},{1,0,0,0},{1,0,0,0}}, 2, 1)),
		?_assertEqual(false, check_win({{1,1,1,0},{0,0,0,0},{0,0,0,0},{0,0,0,0}}, 1, 1)),
		?_assertEqual(false, check_win({{1,1,1,0},{1,1,0,0},{1,0,1,0},{0,0,0,0}}, 1, 1))
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

valid_size_test_() ->
	[
		?_assertEqual(false, valid_size(#board_size{rows="6",cols=7})),
		?_assertEqual(true, valid_size(#board_size{rows=6,cols=7})),
		?_assertEqual(true, valid_size(#board_size{rows=7,cols=8})),
		?_assertEqual(true, valid_size(#board_size{rows=7,cols=9})),
		?_assertEqual(true, valid_size(#board_size{rows=7,cols=10})),
		?_assertEqual(false, valid_size(#board_size{rows=5, cols=7})),
		?_assertEqual(false, valid_size(#board_size{rows=6, cols=8})),
		?_assertEqual(false, valid_size(#board_size{rows=1, cols=3}))
	].
-endif.
