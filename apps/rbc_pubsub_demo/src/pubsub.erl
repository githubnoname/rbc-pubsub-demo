-module(pubsub).
-behaviour(gen_server).

-export([start_link/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3, handle_cast/2]).

% pubsub API
-export([publish/2, subscribe/2, unsubscribe/2, create_channel/1, list_channels/0]).

-record(state, {channels :: ets:tab()}).
-record(channel, {id :: atom(), pid :: pid()}).

-type state() :: #state{}.
-type channel() :: #channel{}.
-type channelid() :: atom().

-type bad_channel() :: {error, channel_doesnt_exist, [channelid()]}.
-type channel_exists() :: {error, channel_already_exists, [channelid()]}.

-include("pubsub.hrl").


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_Args) ->
    io:format("pubsub:init ~p~n", [self()]),
    {ok, #state{channels=ets:new(channels, [set, private, {keypos, 2}])}}.


% pubsub API

-spec publish(pubsub_message(), channelid()) -> ok | bad_channel().
publish(Message, ChannelId) ->
    gen_server:call(?MODULE, {publish, Message, ChannelId}).


-spec subscribe(pid(), channelid()) -> ok | bad_channel() | already_subscribed().
subscribe(Pid, ChannelId) ->
    gen_server:call(?MODULE, {subscribe, Pid, ChannelId}).


-spec unsubscribe(pid(), channelid()) -> ok | bad_channel() | is_not_subscribed().
unsubscribe(Pid, ChannelId) ->
    gen_server:call(?MODULE, {unsubscribe, Pid, ChannelId}).


-spec create_channel(channelid()) -> pid() | channel_exists().
create_channel(ChannelId) ->
    gen_server:call(?MODULE, {create, ChannelId}).


-spec list_channels() -> [channelid()].
list_channels() ->
    gen_server:call(?MODULE, list).


% gen_server API

handle_call({publish, Message, ChannelId}, _From, State) ->
    {Result, NewState} = publish_ll(State, ChannelId, Message),
    {reply, Result, NewState};
handle_call({subscribe, Pid, ChannelId}, _From, State) ->
    {Result, NewState} = subscribe_ll(State, ChannelId, Pid),
    {reply, Result, NewState};
handle_call({unsubscribe, Pid, ChannelId}, _From, State) ->
    {Result, NewState} = unsubscribe_ll(State, ChannelId, Pid),
    {reply, Result, NewState};
handle_call({create, ChannelId}, _From, State) ->
    {Result, NewState} = create_ll(State, ChannelId),
    {reply, Result, NewState};
handle_call(list, _From, State) ->
    {reply, utils:ets_list(State#state.channels), State}.


handle_info(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, State) ->
    ets:delete(State#state.channels),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


% Internal functions

-spec find_channel(state(), channelid()) -> {ok, channel()} | error.
find_channel(State, ChannelId) ->
    case ets:lookup(State#state.channels, ChannelId) of
        [] -> error;
        [C] -> {ok, C}
    end.


-spec with_channel(state(), channelid(), fun((channel()) -> any())) -> any().
with_channel(State, ChannelId, Fun) ->
    case find_channel(State, ChannelId) of
        error ->
            {{error, channel_doesnt_exist, [ChannelId]}, State};
        {ok, Channel} ->
            Fun(Channel)
    end.


-spec publish_ll(state(), channelid(), pubsub_message()) -> {ok, state()} | {bad_channel(), state()}.
publish_ll(State, ChannelId, Message) ->
    with_channel(State, ChannelId, fun(Ch) ->
        channel:publish(Ch#channel.pid, Message),
        {ok, State}
    end).


-spec subscribe_ll(state(), channelid(), pid()) -> {ok, state()} | {bad_channel() | already_subscribed(), state()}.
subscribe_ll(State, ChannelId, Pid) ->
    with_channel(State, ChannelId, fun(Ch) ->
        {channel:subscribe(Ch#channel.pid, Pid), State}
    end).


-spec unsubscribe_ll(state(), channelid(), pid()) -> {ok, state()} | {bad_channel() | is_not_subscribed(), state()}.
unsubscribe_ll(State, ChannelId, Pid) ->
    with_channel(State, ChannelId, fun(Ch) ->
        {channel:unsubscribe(Ch#channel.pid, Pid), State}
    end).


-spec create_ll(state(), channelid()) -> {pid(), state()} | {channel_exists() | {error, any(), [channelid()]}, state()}.
create_ll(State, ChannelId) ->
    case find_channel(State, ChannelId) of
        error ->
            case channels_sup:create_channel(ChannelId) of
                {ok, Pid} ->
                    Channel = #channel{id=ChannelId, pid=Pid},
                    ets:insert(State#state.channels, Channel),
                    {Pid, State};
                {error, Reason} ->
                    {{error, Reason, [ChannelId]}, State}
            end;
        _ ->
            {{error, channel_already_exists, [ChannelId]}, State}
    end.
