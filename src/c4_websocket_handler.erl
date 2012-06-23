% @doc Handles websocket text messages and translates them
% into c4_player commands (join a game, play, quit, etc).

-module(c4_websocket_handler).
-behavior(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
        websocket_info/3, websocket_terminate/3]).
-record(state, {c4_player :: pid()}).
-include("c4_common.hrl").

% @doc Callback that handles new HTTP request by 
% upgrading them to the websocket protocol if Upgrade header is present.
init({_Any, http}, Req, []) ->
        case cowboy_http_req:header('Upgrade', Req) of
                {undefined, Req2} -> {shutdown, Req2, undefined};
                {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
                {<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
        end.


handle(Req, State) ->
	{ok, Req, State}.

% @doc Termination callback. It does nothing since there is
% nothing to cleanup in this app.
-spec(terminate(term(), term()) -> ok).
terminate(_Req, _State) ->
        ok.

% @doc When the websocket is created, a child c4_player process is spawned to which we'll forward
% the user requests.
-spec(websocket_init(term(), term(), term()) -> {ok, term(), #state{}} ).
websocket_init(_Any, Req, []) ->
        Req2 = cowboy_http_req:compact(Req),
	?log("Starting~n", []),
	% Create c4_player process
	{ok, Pid} = c4_player:start_link(),
        {ok, Req2, #state{c4_player=Pid}}.

% @doc Translates websocket messages from the client into c4_player commands.	
% Messages: SEEK, CANCEL_SEEK, PLAY, QUIT_GAME.
websocket_handle({text, Msg}, Req, #state{c4_player=Pid} = State) ->
	reply(c4_player:text_reply(c4_player:text_cmd(Pid, Msg)), Req, State).

% @doc Handles messages sent from c4_player process and replies to
% websocket client.
websocket_info(Event, Req, State) ->
        reply(c4_player:text_reply(Event), Req, State).

% @doc Convenience function to log a reply and return it
% in the format that the websocket handlers need to return.
reply(Msg, Req, State) ->
	?log("Sending message ~s~n", [Msg]),
	{reply, {text, Msg}, Req, State}.

% @doc It terminates the child c4_player process when the websocket is closed.
websocket_terminate(_Reason, _Req, #state{c4_player=Pid}) ->
        c4_player:disconnected(Pid),
	ok.
