%%%-------------------------------------------------------------------
%% @doc ps top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ps_sup).

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
	   [#{id => ps,
	      start => {ps, start_link, []}}]} }.

%%====================================================================
%% Internal functions
%%====================================================================
