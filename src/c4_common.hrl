
-define(log(Format, Args), io:format("~w ~w : " ++ Format, [?MODULE, self()] ++ Args)).
%-define(log(Format, Args), ok).
-define(ends_with(Bin, Str), Str == binary_part(Bin, {byte_size(Bin), -byte_size(Str)})).
-define(INTERNAL_TIMEOUT, 10000).


