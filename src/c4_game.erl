%% Description: Process that handles game play, receives commands from player processes
-module(c4_game).
-export([start/5,start_loop/1]).

% Number of pieces required to be in a line for a win.
-define(NUM_INLINE, 4).
% Returns the piece value on the given row,column
-define(piece(Board, Row, Col),element(Col, element(Row, Board))).
-define(num_rows(Board), erlang:tuple_size(Board)).
-define(num_cols(Board), erlang:tuple_size(element(1, Board))).

% @doc Starts game process
% @todo who decides who playes first?
start(P1, P2, PTurn, Nr, Nc) when PTurn == P1; PTurn == P2 ->
	spawn_link(?MODULE, start_loop, [{P1, P2, PTurn, board(Nr, Nc)}]).

% @doc Returns an  board as a tuple containing Nr row tuples 
% each with Nc columns (all zeroes).
board(Nr, Nc) ->
	Row = erlang:make_tuple(Nc, 0),
	erlang:make_tuple(Nr, 0, [{P,Row} || P <- lists:seq(1,Nr)]).

% Sets up monitors for player processes and starts main loop.
start_loop({P1, P2, PTurn, Board}) ->
	P1Ref = monitor(process, P1),
	P2Ref = monitor(process, P2),
	POther = other_player(P1, P2, PTurn),
	PTurn ! {new_game, play, self()},
	POther ! {new_game, wait, self()},
	loop({P1, P1Ref, P2, P2Ref, PTurn, Board}).
	
% @doc Receives messages from both players, monitors them
loop({P1, P1Ref, P2, P2Ref, PTurn, Board}) ->
	Piece = case PTurn of P1 -> 1; P2 -> 2 end,
	receive
		{'DOWN', P1Ref, process, _, _} -> 
			P2 ! {game, player_left};
		{'DOWN', P2Ref, process, _, _} -> 
			P1 ! {game, player_left};
		{PTurn, drop, Col} ->
			NewP = other_player(P1, P2, PTurn),
			case add_piece(Board, Piece, Col) of
				{ok, NewBoard, Row} ->
					case check_win(NewBoard, Row, Col) of
						true -> 
							PTurn ! {self(), you_win},
							NewP ! {self(), dropped_won, Col};
						false -> 
							NewP ! {self(), dropped, Col},
							loop({P1, P1Ref, P2, P2Ref, NewP, NewBoard})
					end;
				{full, _} ->
					PTurn ! {self(), bad_move},
					NewP ! {self(), player_left}
			end
	end.

other_player(P1, P2, Player) ->
	case Player of
		P1 -> P2;
		P2 -> P1
	end.

% @doc Drops a piece to the board down the given column, returning
% {ok, NewBoard} if ok
% {full, Board} if column full
add_piece(Board, Piece, Col) ->
	case free_row(Board, Col) of
		% @todo look into not handling full and let game die on bad move.
	    full -> {full, Board};
		Row  -> {ok, set_piece(Board, Row, Col, Piece), Row}
	end.

% @doc returns the row index of the first available slot for a given column.
% Returns full or row index if found.
free_row(Board, Col) ->
	free_row(Board, Col, 1, ?num_rows(Board)).

% @doc recursively find first available slot in column.
free_row(Board, Col, Idx, Nr) ->
	case Idx >= Nr of
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
		false -> check_win(Board, Row, Col, Val, [T])
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
