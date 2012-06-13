% @doc Coordinates the creation of games between pairs of players.
% It mostly sits in a loop receiving join requests.  When the first
% join request comes, the player is told to wait for another. When the
% next one comes, the pair are put into a newly created game and
% the process repeats over and over.
-module(c4_game_master).
-behaviour(gen_fsm).
-export([init/1, handle_info/3, loop/3, terminate/3]).
-export([start/0, start_link/0, join/1, cancel_join/1]).
-record(state, {rows=6, cols=7, pending=none, pref=none, parent}).
-include("c4_common.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API

% @doc Starts a standalone c4_game_master process. Mostly for testing.
-spec(start() ->  {ok, pid()} | ignore | {error, binary()}).
start() ->
	gen_fsm:start({local, c4_game_master}, ?MODULE, self(), []).

% @doc Starts a c4_game_master process that is linked to the current
% process.
-spec(start_link() -> {ok, pid()} | ignore | {error, binary()}).
start_link() ->
	gen_fsm:start_link({local, c4_game_master}, ?MODULE, self(), []).

% @doc Call when a player requests to join a game. 
-spec(join(pid()) -> join_pending | {new_game, pid()}).
join(Pid) ->
	gen_fsm:sync_send_event(?MODULE, {join, Pid}).

% @doc Call when a player requests to cancel a join request.
-spec(cancel_join(pid()) -> join_canceled | no_join_pending).
cancel_join(Pid) ->
	gen_fsm:sync_send_event(?MODULE, {cancel_join, Pid}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FSM callbacks

% @doc Starts trapping exits and starts FSM in loop state.
% Callback method for the OTP gen_fsm behavior.
init(ParentPid)->
	process_flag(trap_exit, true),
	?log("Starting~n", []),
	{ok, loop, #state{parent=ParentPid}}.

% @doc Does nothing as there is nothing to clean up upon termination.
% Callback function for the OTP gen_fsm behavior.
terminate(_Reason, _Name, _Data) ->
	ok.

% @doc The main 'loop' state of this FSM handles all messages (joins and join cancellations).
loop({join, Pid}, _From, #state{rows=Rows, cols=Cols, pending=Pending, pref=PRef} = State) ->
	?log("Processing Join~n", []),
	case Pending of
		none ->
			?log("Join pending 2 ~n", []), 
			Ref = erlang:monitor(process, Pid),
			?log("Monitoring ~w~n", [Pid]),
			{reply, join_pending, loop, State#state{pending=Pid, pref=Ref}};
		_ -> 
			{ok, GamePid} = c4_game:start_link({Pending, Pid, Rows, Cols}),
			erlang:demonitor(PRef), 
			c4_player:joined(Pending, GamePid, play),
			{reply, {new_game, GamePid, wait}, loop, State#state{pending=none, pref=none}}
	end;
loop({cancel_join, Pid}, _From, #state{pending=Pid} = State) when is_pid(Pid) ->
	{reply, join_canceled, loop, State#state{pending=none, pref=none}};
loop({cancel_join, Pid}, _From, State) when is_pid(Pid) ->
	{reply, no_join_pending, loop, State}.

% @doc Handles game processes ending, join pending users disconnecting.
% Callback function for miscellaneous messages received by the FSM process.
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

 
