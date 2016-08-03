%%%-------------------------------------------------------------------
%% @doc ps public API
%% @end
%%%-------------------------------------------------------------------

-module(rbc_pubsub_demo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Workers = application:get_env(rbc_pubsub_demo, cowboy_workers, 10),
    Port = application:get_env(rbc_pubsub_demo, cowboy_port, 8080),
    start_cowboy(Workers, Port),
    pubsub_sup:start_link().

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
