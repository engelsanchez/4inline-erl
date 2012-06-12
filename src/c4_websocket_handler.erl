-module(c4_websocket_handler).
-behavior(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
        websocket_info/3, websocket_terminate/3]).
-record(state, {c4_player}).
-include("c4_common.hrl").

init({_Any, http}, Req, []) ->
        case cowboy_http_req:header('Upgrade', Req) of
                {undefined, Req2} -> {ok, Req2, undefined};
                {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
                {<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
        end.

handle(Req, State) ->
        {ok, Req2} = cowboy_http_req:reply(403, [{'Content-Type', <<"text/html">>}],
<<"Nothin to see here">>, Req),
        {ok, Req2, State}.

terminate(_Req, _State) ->
        ok.

websocket_init(_Any, Req, []) ->
        Req2 = cowboy_http_req:compact(Req),
	?log("Starting~n", []),
	% Create c4_player process
	{ok, Pid} = c4_player:start_link(),
        {ok, Req2, #state{c4_player=Pid}}.

% @doc Generic handling of error messages
handle_other({error, _ErrCode, ErrMsg}, Req, State) ->
	{reply, {text, ErrMsg}, Req, State}.
	
% Messages: JOIN, CANCEL_JOIN, PLAY, QUIT_GAME
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


% @doc Handles messages sent from c4_player process
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

reply(Msg, Req, State) ->
	?log("Sending message ~s~n", [Msg]),
	{reply, {text, Msg}, Req, State}.

websocket_terminate(_Reason, _Req, #state{c4_player=Pid}) ->
        c4_player:quit(Pid),
	ok.
