

-ifndef(NOLOG).
-define(log(Format, Args), error_logger:info_msg("~w:~w ~w : " ++ Format ++ "~n", [?MODULE, ?LINE, self()] ++ Args)).
-else.
nolog(_Format, _Args) -> ok.
-define(log(Format, Args), nolog(Format, Args)).
-endif.

-define(ends_with(Bin, Str), Str == binary_part(Bin, {byte_size(Bin), -byte_size(Str)})).
-define(INTERNAL_TIMEOUT, 1000000).
-define(MAX_GAME_ID,1000000).
-define(MAX_PLAYER_ID,1000000).
-record(board_size, {rows=7, cols=6}).
-type game_var() :: std | pop.
%% Game Variant
-type game_state() :: playing | disconnected.
-type game_type() :: anon | priv.
-type turn() :: your_turn | other_turn.
-type seek_id() :: pos_integer().
-type game_id() :: pos_integer().
-opaque board() :: tuple().
-record(game_info,{
		id :: pos_integer(),
		pid=none :: pid() | none, 
		type :: game_type(), 
		variant :: game_var(), 
		board_size :: #board_size{},
		ppid1 = none :: none | pid(),
		ppid2  = none :: none | pid()
		}). 
-record(seek, {
		id = none :: none | pos_integer(),
		pid = none :: none | pid(),
		board_size :: #board_size{}, 
		variant = std :: game_var(), 
		type = anon :: game_type()
		}).

