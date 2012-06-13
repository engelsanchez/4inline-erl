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
                {undefined, Req2} -> {ok, Req2, undefined};
                {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
                {<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
        end.

% @doc Handles a rogue HTTP request to our websocket endpoint
% by sending a 404 response.
handle(Req, State) ->
	{ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}],
	<<"<html><body>Nothing to see here</body></html>">>, Req),
        {ok, Req2, State}.

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

% @doc Generic handling of error messages from c4_player process
-spec(handle_other({error, atom(), binary()}, term(), term()) -> {reply, {text, binary()}, term(), #state{}}).
handle_other({error, _ErrCode, ErrMsg}, Req, State) ->
	{reply, {text, ErrMsg}, Req, State}.

% @doc Translates websocket messages from the client into c4_player commands.	
% Messages: JOIN, CANCEL_JOIN, PLAY, QUIT_GAME.
websocket_handle({text, <<"JOIN">>}, Req, #state{c4_player=Pid} = State) ->
	case c4_player:join(Pid) of
		join_pending -> reply(<<"JOIN_PENDING">>, Req, State);
		{new_game, play} -> reply(<<"NEW_GAME_PLAY">>, Req, State);
		{new_game, wait} -> reply(<<"NEW_GAME_WAIT">>, Req, State);
		Other -> handle_other(Other, Req, State)
	end;
websocket_handle({text, <<"CANCEL_JOIN">>}, Req, #state{c4_player=Pid} = State) ->
	case c4_player:cancel_join(Pid) of
		join_canceled -> reply(<<"JOIN_CANCELED">>, Req, State);
		no_join_pending -> {ok, Req, State};
		Other -> handle_other(Other, Req, State)
	end;
websocket_handle({text, <<"PLAY ", Col:8/integer>>}, Req, #state{c4_player=Pid} = State) ->
	case c4_player:play(Pid, Col - $0) of
		play_ok -> reply(<<"PLAY_OK">>, Req, State);
		you_win -> reply(<<"YOU_WIN">>, Req, State);
		Other -> handle_other(Other, Req, State)
	end;
websocket_handle({text, <<"QUIT_GAME">>}, Req, #state{c4_player=Pid} = State) ->
	case c4_player:quit_game(Pid) of
		game_abandoned -> reply(<<"GAME_ABANDONED">>, Req, State);
		Other -> handle_other(Other, Req, State)
	end;
websocket_handle(_Any, Req, State) ->
        reply(<<"UNKNOWN_COMMAND">>, Req, State).


% @doc Handles messages sent from c4_player process and replies to
% websocket client.
websocket_info({new_game, play}, Req, State) ->
	reply(<<"NEW_GAME_PLAY">>, Req, State);
websocket_info({new_game, wait}, Req, State) ->
	reply(<<"NEW_GAME_WAIT">>, Req, State);
websocket_info({other_played, Col}, Req, State) when is_integer(Col) ->
	reply(<<"OTHER_PLAYED ", (Col+$0)/integer>>, Req, State);
websocket_info({other_won, Col}, Req, State) when is_integer(Col) ->
	reply(<<"OTHER_WON ", (Col+$0)/integer>>, Req, State); 
websocket_info({other_quit}, Req, State) ->
	reply(<<"OTHER_QUIT">>, Req, State);
websocket_info(Event, Req, State) ->
	?log("Unexpected message to ws handler : ~w~n", [Event]),
        {ok, Req, State}.

% @doc Convenience function to log a reply and return it
% in the format that the websocket handlers need to return.
reply(Msg, Req, State) ->
	?log("Sending message ~s~n", [Msg]),
	{reply, {text, Msg}, Req, State}.

% @doc It terminates the child c4_player process when the websocket is closed.
websocket_terminate(_Reason, _Req, #state{c4_player=Pid}) ->
        c4_player:quit(Pid),
	ok.
