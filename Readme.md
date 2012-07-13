4 in-line game server written in Erlang
=======================================

You can [try the game online right now](http://4inline.engelsanchez.net).

Two players can connect to the server and play against each other.
Uses [cowboy](https://github.com/extend/cowboy) as the server layer
to allow for multiple protocols (raw sockets, websockets and possibly
later other http push technologies as fallback)

Building
--------

You will need to have git installed (to fetch the cowboy server dependency)
as well as Erlang, of course.  Simply go to the main directory and type:

   make

EDoc documentation
------------------

To generate detailed source code documentation of the modules in this 
application, simply go to the base directory and type: 

    make doc

The HTML documentation is placed in doc/index.html
