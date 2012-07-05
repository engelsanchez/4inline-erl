% @doc Handles the table of players registered to receive notifications
-module(c4_player_master).
-behaviour(gen_server).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
% Public API
-export([start/0, start_link/0, stop/0, connect/0, connect/1, register_player/1, 
		 unregister_player/1, notify_seek_removed/3, notify_seek_issued/1,
		 player_quit/1]).
-include("c4_common.hrl").
-record(state, {parent}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
    
% @doc Starts a standalone c4_game_master process. Mostly for testing.
-spec(start() ->  {ok, pid()} | ignore | {error, binary()}).
start() ->
	gen_server:start({local, c4_player_master}, ?MODULE, self(), []).

% @doc Starts a c4_game_master process that is linked to the current
% process.
-spec(start_link() -> {ok, pid()} | ignore | {error, binary()}).
start_link() ->
	gen_server:start_link({local, c4_player_master}, ?MODULE, self(), []).

% @doc Stops the player master process
-spec(stop() -> ok).
stop() ->
	gen_server:call(?MODULE, stop, ?INTERNAL_TIMEOUT).

% @doc Creates a new player process 
-spec(connect(binary()) -> {ok, pid(), binary()}).
connect(<<PlayerId:36/binary>>) ->
	gen_server:call(?MODULE, {connect, PlayerId}, ?INTERNAL_TIMEOUT).

% @doc Creates a new player process 
-spec(connect() -> {ok, pid(), binary()}).
connect() ->
	gen_server:call(?MODULE, connect, ?INTERNAL_TIMEOUT).

% @doc Registers a player so they can receive seek notifications
register_player(Pid) ->
	gen_server:call(?MODULE, {register_player, Pid}, ?INTERNAL_TIMEOUT).
	  
% @doc Registers a player so they can receive seek notifications
unregister_player(Pid) ->
	gen_server:call(?MODULE, {unregister_player, Pid}, ?INTERNAL_TIMEOUT).

-spec(player_quit(pid()) -> ok | no_player).
player_quit(Pid) ->
	gen_server:call(?MODULE, {player_quit, Pid}, ?INTERNAL_TIMEOUT).

% @doc Sends a seek removed notification to all players 
% registered to listen to then.
notify_seek_removed(SeekId, P1, P2) ->
	gen_server:call(?MODULE, {notify_seek_removed, SeekId, P1, P2}, ?INTERNAL_TIMEOUT).

% @doc Sends a seek issued notification to all players 
% registered to listen to then.
notify_seek_issued(#seek{} = Seek) ->
	gen_server:call(?MODULE, {notify_seek_issued, Seek}, ?INTERNAL_TIMEOUT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks

% @doc Creates the player table. 
% gen_server initialization callback. 
init(ParentId) ->
	process_flag(trap_exit, true),
	ets:new(c4_player_notify_tbl, [named_table, private, set]),
	ets:new(c4_player_id_tbl, [named_table, private, set]),
	ets:new(c4_player_pid_tbl, [named_table, private, set]),
	{ok, #state{parent=ParentId}}.

% @doc Deletes the player table.
% gen_server termination callback.
terminate(_Reason, _State) ->
	ets:delete(c4_player_notify_tbl),
	ets:delete(c4_player_pid_tbl),
	ets:delete(c4_player_id_tbl),
	ok.

% @doc Creates a new child player process and assigns it a
% unique ID.
new_player({ParentPid, _Tag} = From, State) ->
	{ok, Pid} = c4_player:start_link(ParentPid),
	monitor(process, Pid),
	do_register_player(Pid),
	?log("Started new player ~w", [Pid]),
	PlayerId = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
	ets:insert(c4_player_id_tbl, {PlayerId, Pid}),
	ets:insert(c4_player_pid_tbl, {Pid, PlayerId}),
	gen_server:reply(From, {ok, Pid, PlayerId}),
	% Send seeks to player now
	SeekList = c4_game_master:seek_list(),
	?log("Notifying of current seeks ~w", [SeekList]),
	lists:foreach(
		fun(#seek{pid=SeekerPid} = Seek) ->
			case SeekerPid of
				Pid -> ok;
				_ -> c4_player:seek_issued(Pid, Seek)
			end
		end, SeekList),
	{noreply, State}.

do_register_player(Pid) ->
	ets:insert(c4_player_notify_tbl, {Pid}).

% @doc 
% gen_server synchronous request callback.
handle_call(connect, From, State) ->
	?log("Processing CONNECT", []),
	new_player(From, State);
handle_call({connect, PlayerId}, {CallerId, _Tag} = From, State) ->
	?log("Processing CONNECT AS ~s", [PlayerId]),
	% Look up player id and process associated to it.
	case ets:lookup(c4_player_id_tbl, PlayerId) of 
		[] -> 
			?log("Player not found, creating new (~s)", [PlayerId]),
			new_player(From, State);
		[{_PlayerId, Pid}] -> 
			?log("Player reconnected as ~s", [PlayerId]),
			c4_player:reconnected(Pid, CallerId),
			{reply, {ok, Pid, PlayerId}, State}
	end;
handle_call({register_player, Pid}, _From, State) ->
	do_register_player(Pid),
	{reply, ok, State};
handle_call({unregister_player, Pid}, _From, State) ->
	?log("Removing player ~w from notification list", [Pid]),
	ets:delete(c4_player_notify_tbl, Pid),
	{reply, ok, State};
handle_call({player_quit, Pid}, _From, State) ->
	{reply, do_player_quit(Pid), State};
handle_call({notify_seek_issued, Seek}, _From, State) ->
	send_seek_issued(Seek),
	{reply, ok, State};
handle_call({notify_seek_removed, SeekId, P1, P2}, _From, State) ->
	send_seek_removed(SeekId, P1, P2),
	{reply, ok, State};
handle_call(stop, _From, State) ->
	?log("Received STOP message", []),
	{stop, normal, ok, State}.

% @doc Handles player notification requests.
% Asynchronous request callback.
handle_cast({notify_seek_removed, SeekId, P1, P2}, State) ->
	send_seek_removed(SeekId, P1, P2),
	{noreply, State}; 
handle_cast({notify_seek_issued, Seek}, State) ->
	send_seek_issued(Seek),
	{noreply, State}. 

% @doc Asynchronously sends a seek issued message to all registered players
% except for the one issuing the seek.
send_seek_issued(#seek{pid=SPid} = Seek) ->
	?log("Sending seek issued msg to everyone but ~w", [SPid]),
	ets:foldl(
		fun({Pid}, []) -> 
			case Pid of 
				SPid -> ok; 
				_ -> c4_player:seek_issued(Pid, Seek)
			end, 
			[] 
		end, 
		[], 
		c4_player_notify_tbl
		),
	ok.

% @doc Asynchronously sends a seek removal message to all registered players
% except for the one issuing the seek.
send_seek_removed(SeekId, P1, P2) ->
	?log("Sending seek removed message to everyone but ~w and ~w", [P1, P2]),
	ets:foldl(
		fun({Pid}, []) -> 
			case Pid of 
				P1 -> ok; 
				P2 -> ok; 
				_ -> c4_player:seek_removed(Pid, SeekId)
			end,
			[] 
		end,
		[], 
		c4_player_notify_tbl
		),
	ok.

% @doc Callback for misc events (does nothing).
handle_info({'EXIT', Pid, _Reason}, #state{parent=Pid} = State) ->
	{stop, parent_die, State};
handle_info({'EXIT', Pid, _Reason}, State) when is_pid(Pid) ->
	% @todo Remove player entries.
	case ets:lookup(c4_player_pid_tbl, Pid) of
		[] -> {noreply, State}; 
		[{Pid, PlayerId}] -> 
			ets:delete(c4_player_pid_tbl, Pid),
			ets:delete(c4_player_id_tbl, PlayerId),
			{noreply, State}
	end;
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
	do_player_quit(Pid),
	{noreply, State};
handle_info(Event,  State) ->
	unexpected(Event, State).

unexpected(Event, State) ->
	?log("Unexpected event ~w : ~w", [Event, State]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

do_player_quit(Pid) ->
	case ets:lookup(c4_player_pid_tbl, Pid) of
		[{Pid, PlayerId}] ->
			ets:delete(c4_player_notify_tbl, Pid),
			ets:delete(c4_player_pid_tbl, Pid),
			ets:delete(c4_player_id_tbl, PlayerId),
			ok;
		[] -> no_player
	end.

