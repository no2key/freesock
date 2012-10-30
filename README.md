freesock
========

freesock is an socks5 server.

The client is [here](https://github.com/goofansu/freeclient).

You must use client with freesock.

# Implementation #

freeclient and freesock build a tunnel to transfer data each other to pass through the firewall (E.g, GFW in China). SSL can be used to encrypt the data which is going to be implemented.

Default server port is 8080 which can be customized in `freesock/apps/fs/src/echo.erl`

# Building #

To build freesock you will need a working installation of Erlang R15B (or later).

R14B may be supported but not tested.

Should you want to clone the freesock repository, you will also require git.

# Install #

* `git clone git://github.com/goofansu/freesock.git`

* `cd freesock`

* `./rebar compile`

* `./rebar generate` will install freesock to `/usr/local/freesock`

# Usage #

* `/usr/local/freesock/bin/fs start`
 
* `/usr/local/freesock/bin/fs stop`
    
# Todos #

* Add support to SSL





