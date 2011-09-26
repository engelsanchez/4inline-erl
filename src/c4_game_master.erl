% The game master sits in a loop spawning game processes when requested
-module(c4_game_master).
-export([start/2,loop/1]).

% Spawns and registers game master process
start(Nc, Nr) ->
	register(?MODULE, spawn(c4_game_master, loop, [{Nr, Nc, none}])).
	
% @doc Game master loop. Receives request from player processes to join
% a game or forget about joining a game.
loop({Nr, Nc, P1}) ->
	receive
		{join, P2} -> 
			NewP = 
				case P1 of 
					none -> P2;
					_ -> 
						c4_game:start(P1, P2, P1, Nc, Nr),
						none
				end,
			loop({Nr, Nc, NewP});
		% Current player wants to forget about joining
		{forget_game, P1} -> 
			P1 ! {game_forgotten, P1},
			loop({Nr, Nc, none});
		% Player already in a game sent request to forget about it.
		{forget_game, OldP} ->
			OldP ! {game_started, OldP},
			loop({Nr, Nc, P1})
	end.
