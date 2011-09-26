%% Description: TODO: Add description to c4
-module(c4).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0,start/1,handle_connection/1,start_loop/1]).

%%
%% API Functions
%%

%%
%% Entities:
%% 1 server process (or multiple)
%% 1 game coordinator (child of server)
%% * game processes (child of game coordinator, monitored by players, monitors players)
%% * player handler processes (children of the server)
%%  
%% Player Handler States:
%%
%% Idle
%%   - JOIN -> WaitingForGame
%%   - QUIT -> Goodbye
%%   - Disconnect -> Abort
%%   - Connection error -> Abort
%%
%% WaitingForGame
%%   Contact game coordinator, request join
%%   - Player QUIT -> AbortGame, Goodbye
%%   - Game Master  {Join, GamePid}  -> InGame
%%
%% InGame
%%   Wait for game result
%%   - Disconnect -> AbortGame, Abort
%%   - Connection error -> AbortGame, Abort
%%   - Player QUIT -> AbandoningGame
%%   - Game error -> GameError, Idle
%% 
%%  When a game crashes or exits, player processes get notified
%% When a player process crashes or exits, game processes must come down too.
%%
%% AbandoningGame
%% Send Abandon msg to game process, wait for confirmation.
%%   - Game Exit:  -> Idle
%%
%% Game Process. 
%% State data:
%%   Board with pieces
%%   [PlayerPid, PlayerPid]
%%   TurnPlayerIdx
%% Receives messages from both players, monitors them
%%
%%  GameLoop:
%%   - current player DROP N: update board, if win notify,exit else switch turns 



%%
%% Local Functions
%%

start() ->
	io:format("Will Start", []),
	start(8080).

%% Starts the server socket and spawns the connection accepting process
start(PortNumber) ->
   Pid = spawn(c4, start_loop, [PortNumber]),
   io:format("Opened process ~w ~n", [Pid]).

start_loop(PortNumber) ->
   {ok, LSock} = gen_tcp:listen(PortNumber, [binary, {backlog, 5}, {active, false}]),
   io:format("Listening on port ~w ~n", [PortNumber]),
   loop(LSock).
	
%% Process a new connection by spawning a process to talk to it.
process(Socket) ->
	spawn(c4, handle_connection, [Socket]).

handle_connection(Socket) ->
	ok = gen_tcp:send(Socket, "CONNECT4"),
	case gen_tcp:recv(Socket, 0) of
		{ok, <<"CONNECT4;">>} -> handle_idle(Socket);
		{ok, _} -> handle_bad_msg(Socket)
	end.  

handle_bad_msg(Socket) ->
	gen_tcp:send(Socket, "BAD COMMAND"),
	gen_tcp:close(Socket).

handle_idle(Socket)  ->
	ok = gen_tcp:send(Socket, "OK THEN"),
	gen_tcp:close(Socket).

loop(LSock) ->
	case gen_tcp:accept(LSock) of
		{ok,S} -> process(S);
		{error, closed} -> exit(closed_socket)
	end,
	loop(LSock).





