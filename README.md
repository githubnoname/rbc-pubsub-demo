Chat demo
=========

![Chat](/chat.png)

Simple publish-subscribe backend with message filtering by channels. Frontend is ugly web1.0-like chat, but on WebSockets.

Backend contains stupid flood bot to spam all channels.

Requirements
------------

* [erlang OTP 19](http:erlang.org). Should work on 17+, but was not tested yet.
* [rebar3](http://www.rebar3.org).

Build and check
---------------

        make build && make check

Run
---

To run with Erlang console

        make run

By default backend starts with two channels and on 8080 port. To change port, edit `config/sys.config` and specify `{cowboy_port, <PORT YOU WANT>}` there. To add channel, type in erlang console:

        pubsub:create_channel('<CHANNEL NAME>')

Access web UI
-------------

If you run backend on localhost, open http://127.0.0.1:8080 in your favorite browser. Safari 9.1.2, Chrome 51, and Firefox 45 were tested.


Web ui manual
-------------

1. To change channel, select interested channel from list and click on `Change it` button.
2. Chat doesn't auto refresh channel lists, you can do it manually by presssing on `Update room list`.
3. Feel free to specify you name instead 'Noname'.
4. And chat, chat, chat!

... with yourself :(
