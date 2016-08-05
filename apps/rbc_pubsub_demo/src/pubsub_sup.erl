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
-define(CHILD(M,T), #{id => M, start => {M, start_link, []}, type => T}).

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
    {ok, { #{strategy => one_for_one}, 
	   [?CHILD(channels_sup, supervisor),
            ?CHILD(pubsub, worker)
           ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
