
-define(log(Format, Args), io:format("~w ~w : " ++ Format, [?MODULE, self()] ++ Args)).
%-define(log(Format, Args), ok).
-define(ends_with(Bin, Str), Str == binary_part(Bin, {byte_size(Bin), -byte_size(Str)})).
-define(INTERNAL_TIMEOUT, 10000).
-define(MAX_GAME_ID,1000000).
-define(MAX_PLAYER_ID,1000000).
-record(board_size, {rows=7, cols=6}).
-type game_var() :: std | pop.
%% Game Variant
-type game_state() :: playing | disconnected.
-type game_type() :: anon | priv.
-type turn() :: your_turn | other_turn.
-opaque board() :: tuple().
-record(game_info,{
		id :: pos_integer(),
		pid=none :: pid() | none, 
		type :: game_type(), 
		variant :: game_var(), 
		board_size :: #board_size{},
		pid1 :: pos_integer(),
		ppid1 = none :: none | pid(),
		pid2 :: pos_integer(),
		ppid2  = none :: none | pid()
		}). 
-record(seek, {
		id :: pos_integer(),
		pid = none :: none | pid(),
		board_size :: #board_size{}, 
		game_var=std :: game_var(), 
		game_type=anon :: game_type()
		}).

