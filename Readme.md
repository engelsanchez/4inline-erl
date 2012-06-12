4 in-line game server written in Erlang
=======================================

Two players can connect to the server and play against each other.
Uses [cowboy](https://github.com/extend/cowboy) as the server layer
to allow for multiple protocols (raw sockets, websockets and possibly
later other http push technologies as fallback)
