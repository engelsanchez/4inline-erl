% @doc 4 inline listening server.
%
% Entities:
% <ul>
% <li> 1 server process accepting connections </li>
% <li> 1 game coordinator (linked to server) </li>
% <li> 1 player process per connected client (children of the server) </li>
% <li> 1 game processes for each pair of clients playing (child of game coordinator, monitored by players, monitors players) </li>
% </ul>
%
-module(c4_server).
-export([start/0,start/1,start_loop/1]).

-define(DEFAULT_PORT,8080).
-define(DEFAULT_ROWS, 7).
-define(DEFAULT_COLS, 6).

% @doc Starts the server process using the default port.
start() ->
	start(?DEFAULT_PORT).

%% Starts the server socket and spawns the connection accepting process
start(Port) ->
	io:format("Will Start on port ~w ~n", [Port]),
	Pid = spawn(c4_server, start_loop, [Port]),
	io:format("Opened process ~w ~n", [Pid]).

start_loop(Port) ->
	{ok, LSock} = gen_tcp:listen(
		Port, 
		[binary, {backlog, 5}, {active, false}, {nodelay, true}]),
	io:format("Listening on port ~w ~n", [Port]),
	% Spawn game master process
	spawn_link(c4_game_master, start, [?DEFAULT_ROWS, ?DEFAULT_COLS]),
	loop(LSock).

%% Main server loop, accepting incoming connections.   
loop(LSock) ->
	{ok, S} = gen_tcp:accept(LSock),
	Pid = spawn(c4_player, start, [S]),
	% Safely transfering msg reception to player process, which should expect a first message
	% if the new process does this first, we could end up with an unexpected tcp msg in our inbox.
	gen_tcp:controlling_process(S, Pid),
	inet:setops(S, {active, once}),
	loop(LSock).
