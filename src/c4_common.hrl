
-define(log(Format, Args), io:format("~w ~w : " ++ Format, [?MODULE, self()] ++ Args)).
%-define(log(Format, Args), ok).
-define(ends_with(Bin, Str), Str == binary_part(Bin, {byte_size(Bin), -byte_size(Str)})).
-define(INTERNAL_TIMEOUT, 10000).
-define(MAX_GAME_ID,1000000).
-define(MAX_PLAYER_ID,1000000).
-record(board_size, {rows=7, cols=6}).
-type game_var() :: std | pop.
-type game_type() :: anon | priv.
-type turn() :: your_turn | other_turn.

