%% @doc Game process interface.
%% It receives moves from players and notifies the opponent of the move
%% and whether the game has been won or the board is full.
%% The public functions are {@link start/1}, {@link start_link/1},
%% {@link play/3}, {@link disconnect/2}, {@link abandon/1} and {@link quit/2}.
%% The other functions are FSM callbacks.
-module(c4_game).
-behaviour(gen_fsm).
% FSM callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, playing/3, disconnected/3, terminate/3, code_change/4]).
% Public API
-export([start/1, start_link/1, play/3, disconnect/2, abandon/1, quit/2]).

% FSM State data structure
-record(state, {p1, id1, color1, p2, id2, color2, board, game_var}).
-include("c4_common.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
% @doc Starts unsupervised game process, mostly for testing.
-spec(start(#game_info{}) -> {ok, pid()} | ignore | {error, string()}).
start(GameInfo) ->
	gen_fsm:start(?MODULE, GameInfo, []).

% @doc Starts game process linked to the current process.
-spec(start_link(#game_info{}) -> {ok, pid()} | ignore | {error, string()}).
start_link(GameInfo) ->
	gen_fsm:start_link(?MODULE, GameInfo, []).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FSM functions

% @doc Creates the empty board and starts monitoring the player processes.
% FSM initialization callback.
-spec(init(#game_info{}) -> {ok, playing, #state{}}).
init(#game_info{ppid1=P1, pid1=Id1, ppid2=P2, pid2=Id2, board_size=BoardSize, variant=GameVar}) when is_pid(P1), is_pid(P2)  ->
	?log("Starting~n", []),
	monitor(process, P1),
	monitor(process, P2),
	{ok, playing, #state{p1=P1, id1=Id1, color1=first, p2=P2, id2=Id2, color2=second, board=c4_board:new(BoardSize), game_var=GameVar}}.

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
	case c4_board:add_piece(Board, Color, Col) of
		{ok, NewBoard, Row} ->
			?log("Board ~w~n", [NewBoard]),
			case c4_board:check_win(NewBoard, Row, Col) of
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
disconnected({join, Pid, PlayerId}, _From, #state{p1=P1, id1=Id1, p2=P2, id2=Id2} = State) 
		when is_pid(Pid) ->
	case PlayerId of
		Id1 ->
			State2 = State#state{p1=Pid},
			case P2 of
				none ->
					{reply, {in_game, wait}, disconnected, State2};
				_Pid when is_pid(_Pid) ->
					c4_player:other_returned(P2, self()),
					{reply, {in_game, your_turn}, playing, State2}
			end;
		Id2 -> 
			State2 = State#state{p2=Pid},
			case P1 of
				none ->
					{reply, {in_game, wait}, disconnected, State2};
				_Pid when is_pid(_Pid)  ->
					c4_player:other_returned(P1, self()),
					{reply, {in_game, your_turn}, playing, State2}
			end
	end.


% @doc Handles disconnections or requests to abandon a game.
handle_sync_event({quit, Pid}, _From, _StateName, #state{p1=P1, p2=P2} = State) ->
	c4_player:other_quit(case Pid of P1->P2; P2->P1 end, self()),
	{stop, normal, ok, State};
handle_sync_event({disconnected, Pid}, _From, _StateName, #state{p1=P1, p2=P2} = State) ->
	State2 = case Pid of P1 -> State#state{p1=none}; P2 -> State#state{p2=none} end,
	{reply, ok, disconnected, State2};
handle_sync_event({abandon}, _From, _StateName, State) ->
	{stop, normal, ok, State}.

% @doc Generic FSM callback for asynchronous events, but this module does not expect any.
handle_event(Event, State, Data) ->
	?log("Unexpected event ~w in state ~w : ~w~n", [Event, State, Data]),
	{next_state, State, Data}.

% @doc Reports any message as unexpected as we don't use any.
% FSM callback for non-FSM process messages.
handle_info({'DOWN', _Ref, process, Pid, _Reason}, _State, #state{p1=Pid} = Data) ->
	{next_state, disconnected, Data#state{p1=none}};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, _State, #state{p2=Pid} = Data) ->
	{next_state, disconnected, Data#state{p2=none}};
handle_info(Event, State, Data) ->
	?log("Unexpected event ~w in state ~w : ~w~n", [Event, State, Data]),
	{next_state, State, Data}.

% @doc Does nothing as there is no real cleanup needed upon game end.
% Generic FSM termination callback.
terminate(_Reason, _StateName, _State) ->
	ok.
% @doc Code hot swapping callback, does nothing.
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions

% @doc Returns a state record with the players swapped 
turn_change(#state { p1=P1, id1=Id1, color1=Color1, p2=P2, id2=Id2, color2=Color2} = State) ->
	State#state{p1=P2, id1=Id2, color1=Color2, p2=P1, id2=Id1, color2=Color1}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

turn_change_test() ->
	S1 = #state{p1=p1,id1=id1,color1=color1,p2=p2,id2=id2,color2=color2},
	?assertEqual(#state{p1=p2,id1=id2,color1=color2,p2=p1,id2=id1,color2=color1}, turn_change(S1)).

-endif.
