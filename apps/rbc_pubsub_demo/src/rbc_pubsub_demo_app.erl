%%%-------------------------------------------------------------------
%% @doc ps public API
%% @end
%%%-------------------------------------------------------------------

-module(rbc_pubsub_demo_app).

-behaviour(application).

-include("pubsub.hrl").

%% Application callbacks
-export([start/2, stop/1]).
-export([start_flood/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Workers = application:get_env(rbc_pubsub_demo, cowboy_workers, 10),
    Port = application:get_env(rbc_pubsub_demo, cowboy_port, 8080),
    start_cowboy(Workers, Port),
    Result = pubsub_sup:start_link(),
    pubsub:create_channel(channel1),
    pubsub:create_channel(channel42),
    {ok, _} = supervisor:start_child(pubsub_sup, #{id => pubsub_flood,
                                                   start => {rbc_pubsub_demo_app, start_flood, []}}),
    Result.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

start_cowboy(Workers, Port) ->
    Dispatch = cowboy_router:compile(
                 [{'_', [
                         {"/", cowboy_static, {priv_file, rbc_pubsub_demo, "static/index.html"}},
                         {"/scripts/[...]", cowboy_static, {priv_dir, rbc_pubsub_demo, "static/scripts"}},
                         {"/ws", ws_handler, []}
                        ]}
                 ]),
    {ok, _} = cowboy:start_http(http, Workers, [{port, Port}],
                               [{env, [{dispatch, Dispatch}]}]).


start_flood() ->
    io:format("START~n"),
    Pid = spawn(fun() -> 
        flood([
            "Hi, there!",
            "How are you?",
            "What's up?",
            "How you doin'?"
          ], [])
    end),
    link(Pid),
    {ok, Pid}.


flood(List, []) ->
    flood(List, List);
flood(List, [H|T]) ->
    Channels = pubsub:list_channels(),
    lists:foreach(fun(C) ->
        pubsub:publish(#pubsub_message{sender = 'Flooder', data = H}, C)
    end, Channels),
    timer:sleep(15000 + rand:uniform(15000)),
    flood(List, T).
