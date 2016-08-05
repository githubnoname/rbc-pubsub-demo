-module(channels_sup).
-behavioud(supervisor).

% API

-export([start_link/0, create_channel/1, destroy_channel/1]).


% supervisor

-export([init/1]).


% API

start_link() ->
    io:format("channels_sup:start_link ~p~n", [self()]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


create_channel(ChannelId) ->
    supervisor:start_child(?MODULE, #{id => ChannelId, start => {channel, start_link, [ChannelId]},
                                      shutdown => brutal_kill}).


destroy_channel(ChannelId) ->
    supervisor:terminate_child(?MODULE, ChannelId),
    supervisor:delete_child(?MODULE, ChannelId).


% supervisor

init([]) ->
    io:format("channels_sup:init ~p~n", [self()]),
    {ok, { #{strategy => one_for_one}, 
            []} }.
