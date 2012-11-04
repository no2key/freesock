freesock
========

freesock is a socks5 server. Inspired by [shadowsocks](https://github.com/clowwindy/shadowsocks).

# Notice #

Branch `master` is stable with production quality.

Branch `ssl-socks` is in beta. Welcome to checkout and take a try.

You can use freesock alone, or with [freeclient](https://github.com/goofansu/freeclient) (see Implementation).

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

* ~~Add support to SSL~~ (Supported, please checkout ssl-socks branch.)

# How to generate SSL test certificate #

```
openssl genrsa -out key.pem 1024

openssl req -new -key key.pem -out request.pem

openssl x509 -req -days 30 -in request.pem -signkey key.pem -out cert.pem

```

# Contact #

[My blog](http://goofansu.com)

@[goofansu](http://twitter.com/goofansu)
