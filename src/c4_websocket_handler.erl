-module(c4_websocket_handler).
-behavior(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
        websocket_info/3, websocket_terminate/3]).
-record(state, {c4_player}).

init({_Any, http}, Req, []) ->
        case cowboy_http_req:header('Upgrade', Req) of
                {undefined, Req2} -> {ok, Req2, undefined};
                {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
                {<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
        end.

handle(Req, State) ->
        {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"text/html">>}],
%% HTML code taken from misultin's example file.
<<"<html>
<head>
<script type=\"text/javascript\">
function addStatus(text){
        var date = new Date();
        document.getElementById('status').innerHTML
                = document.getElementById('status').innerHTML
                + date + \": \" + text + \"<br/>\";
}
function ready(){
        if (\"MozWebSocket\" in window) {
                WebSocket = MozWebSocket;
        }
        if (\"WebSocket\" in window) {
                // browser supports websockets
                var ws = new WebSocket(\"ws://localhost:8080/websocket\");
                ws.onopen = function() {
                        // websocket is connected
                        addStatus(\"websocket connected!\");
                        // send hello data to server.
                        ws.send(\"hello server!\");
                        addStatus(\"sent message to server: 'hello server'!\");
                };
                ws.onmessage = function (evt) {
                        var receivedMsg = evt.data;
                        addStatus(\"server sent the following: '\" + receivedMsg + \"'\");
                };
                ws.onclose = function() {
                        // websocket was closed
                        addStatus(\"websocket was closed\");
                };
        } else {
                // browser does not support websockets
                addStatus(\"sorry, your browser does not support websockets.\");
        }
}
</script>
</head>
<body onload=\"ready();\">
Hi!
<div id=\"status\"></div>
</body>
</html>">>, Req),
        {ok, Req2, State}.

terminate(_Req, _State) ->
        ok.

websocket_init(_Any, Req, []) ->
        Req2 = cowboy_http_req:compact(Req),
	% Create c4_player process
	{ok, Pid} = c4_player:start_link(),
        {ok, Req2, #state{c4_player=Pid}}.

% @doc Generic handling of error messages
handle_other({error, _ErrCode, ErrMsg}, Req, State) ->
	{reply, {text, ErrMsg}, Req, State}.
	
% Messages: JOIN, CANCEL_JOIN, PLAY, QUIT_GAME
websocket_handle({text, <<"JOIN">>}, Req, #state{c4_player=Pid} = State) ->
	case c4_player:join(Pid) of
		join_pending -> {reply, {text, <<"JOIN_PENDING">>}, Req, State};
		{new_game, play} -> {reply, {text, <<"NEW_GAME_PLAY">>}, Req, State};
		{new_game, wait} -> {reply, {text, <<"NEW_GAME_WAIT">>}, Req, State};
		Other -> handle_other(Other, Req, State)
	end;
websocket_handle({text, <<"CANCEL_JOIN">>}, Req, #state{c4_player=Pid} = State) ->
	case c4_player:cancel_join(Pid) of
		join_canceled -> {reply, {text, <<"JOIN_CANCELED">>}, Req, State};
		no_join_pending -> {ok, Req, State};
		Other -> handle_other(Other, Req, State)
	end;
websocket_handle({text, <<"PLAY ", Col:8/integer>>}, Req, #state{c4_player=Pid} = State) ->
	case c4_player:play(Pid, Col - $0) of
		play_ok -> {reply, {text, <<"PLAY_OK">>}, Req, State};
		you_win -> {reply, {text, <<"YOU_WIN">>}, Req, State};
		Other -> handle_other(Other, Req, State)
	end;
websocket_handle({text, <<"QUIT_GAME">>}, Req, #state{c4_player=Pid} = State) ->
	case c4_player:quit_game(Pid) of
		game_abandoned -> {reply, {text, <<"GAME_ABANDONED">>}, Req, State};
		Other -> handle_other(Other, Req, State)
	end;
websocket_handle(_Any, Req, State) ->
        {reply, {text, <<"UNKNOWN_COMMAND">>}, Req, State}.


% @doc Handles messages sent from c4_player process
websocket_info({other_played, Col}, Req, State) when is_integer(Col) ->
	{reply, {text, <<"OTHER_PLAYED ", (Col+$0)/integer>>}, Req, State};
websocket_info({other_won, Col}, Req, State) when is_integer(Col) ->
	{reply, {text, <<"OTHER_WON ", (Col+$0)/integer>>}, Req, State};
websocket_info({other_quit}, Req, State) ->
	{reply, {text, <<"OTHER_QUIT">>}, Req, State};
websocket_info(_Info, Req, State) ->
        {ok, Req, State}.

websocket_terminate(_Reason, _Req, #state{c4_player=Pid}) ->
        c4_player:quit(Pid),
	ok.
