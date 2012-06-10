% The game master sits in a loop spawning game processes when requested
-module(c4_game_master).
-behaviour(gen_fsm).
-export([init/1, handle_info/3, loop/3, terminate/3]).
-export([start/0, start_link/0, join/1, cancel_join/1]).
-record(state, {rows=6, cols=7, pending=none, pref=none, parent}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API

% Returns {ok, Pid} | ignore | {error, Error}
start() ->
	gen_fsm:start({local, c4_game_master}, ?MODULE, self(), []).

% Returns {ok, Pid} | ignore | {error, Error}
start_link() ->
	gen_fsm:start_link({local, c4_game_master}, ?MODULE, self(), []).

% @doc Join request
% Returns join_pending | {new_game, GamePid}
join(Pid) ->
	gen_fsm:sync_send_event(?MODULE, {join, Pid}).

% @doc Cancel pending join request
% Returns : join_canceled | no_join_pending 
cancel_join(Pid) ->
	gen_fsm:sync_send_event(?MODULE, {cancel_join, Pid}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FSM callbacks

init(ParentPid)->
	process_flag(trap_exit, true),
	{ok, loop, #state{parent=ParentPid}}.

terminate(_Reason, _Name, _Data) ->
	ok.

loop({join, Pid}, _From, #state{rows=Rows, cols=Cols, pending=Pending} = State) ->
	io:format("~w Processing Join~n", [?MODULE]),
	case Pending of
		none ->
			io:format("Join pending~n"), 
			Ref = erlang:monitor(Pid),
			io:format("Monitoring ~w~n", [Pid]),
			{reply, join_pending, loop, State#state{pending=Pid, pref=Ref}};
		_ -> 
			{ok, GamePid} = c4_game:start_link({Pending, Pid, Rows, Cols}),
			erlang:demonitor(Pending), 
			{reply, {new_game, GamePid, wait}, loop, State#state{pending=none, pref=none}}
	end;
loop({cancel_join, Pid}, _From, #state{pending=Pid} = State) when is_pid(Pid) ->
	{reply, join_canceled, loop, State#state{pending=none, pref=none}};
loop({cancel_join, Pid}, _From, State) when is_pid(Pid) ->
	{reply, no_join_pending, loop, State}.

% @doc Handles game processes ending, join pending users disconnecting
handle_info({'EXIT', Pid, _Reason}, _Name, #state{parent=Pid} = State) ->
	{stop, parent_died, State};
handle_info({'EXIT', _Pid, _Reason}, Name, State) ->
	{next_state, Name, State};
% Join pending user disconnects, forget join
handle_info({'DOWN', Ref, process, P1, _Reason}, Name, #state{pending=P1, pref=Ref} = Data) ->
	{next_state, Name, Data#state{pending=none, pref=none}};
% User that previously was trying to join disconnects (game started), nothing to do
handle_info({'DOWN', _Ref, process, _P2, _Reason}, Name, State) ->
	{next_state, Name, State}.

 
