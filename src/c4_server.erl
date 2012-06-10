-module(c4_server).
-behavior(application).
-behavior(supervisor).

-export([start/0, start/2, stop/1]).
-export([init/1]).
-export([start_link/0]).


start() ->
        application:start(crypto),
        application:start(public_key),
        application:start(ssl),
        application:start(cowboy),
        application:start(c4_server).

start(_Type, _Args) ->
        Dispatch = [
                {'_', [
                        {[<<"websocket">>], c4_websocket_handler, []},
                        {'_', default_handler, []}
                ]}
        ],
        cowboy:start_listener(my_http_listener, 100,
                cowboy_tcp_transport, [{port, 8080}],
                cowboy_http_protocol, [{dispatch, Dispatch}]
        ),
        cowboy:start_listener(my_https_listener, 100,
                cowboy_ssl_transport, [
                        {port, 8443}, {certfile, "priv/ssl/cert.pem"},
                        {keyfile, "priv/ssl/key.pem"}, {password, "cowboy"}],
                cowboy_http_protocol, [{dispatch, Dispatch}]
        ),
        start_link().

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
	ok.

init(_Args) ->
	SupSpec = {one_for_one, 1, 1},
	GameMasterSpec = {c4_game_master, {c4_game_master, start_link, []}, permanent, 2000, worker, [c4_game_master]},
	{ok, {SupSpec, [GameMasterSpec]}}.	
