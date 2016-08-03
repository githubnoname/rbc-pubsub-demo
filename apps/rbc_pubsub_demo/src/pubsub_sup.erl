%%%-------------------------------------------------------------------
%% @doc ps top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pubsub_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { #{strategy => one_for_all}, 
	   [#{id => pubsub,
	      start => {pubsub, start_link, []}},
            #{id => pubsub_flood,
              start => {rbc_pubsub_demo_app, start_flood, []}}
           ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
