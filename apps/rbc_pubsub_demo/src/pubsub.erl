-module(pubsub).
-behaviour(gen_server).

-export([start_link/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2]).

% module api
-export([publish/3, subscribe/2, unsubscribe/2, create/1, list_channels/0]).

-export([printer/2]).

-record(state, {channels=maps:new()}).
-record(channel, {id, subscribers}).
-record(subscriber, {pid, ref}).
-record(message, {sender, data}).

printer(Prefix, Repeats) ->
    spawn(fun () ->
        lists:foreach(fun(N) ->
            receive
                Msg ->
                    io:format("(~p) ~p: ~p~n", [N, Prefix, Msg])
            end
        end, lists:seq(1, Repeats))
    end).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{channels=maps:new()}}.

publish(Sender, Message, ChannelId) ->
    gen_server:call(?MODULE, {publish, #message{sender=Sender, data=Message}, ChannelId}).

subscribe(Pid, ChannelId) ->
    gen_server:call(?MODULE, {subscribe, Pid, ChannelId}).

unsubscribe(Pid, ChannelId) ->
    gen_server:call(?MODULE, {unsubscribe, Pid, ChannelId}).

create(Channel) ->
    gen_server:call(?MODULE, {create, Channel}).

list_channels() ->
    gen_server:call(?MODULE, list).

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
    {reply, maps:keys(State#state.channels), State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    NewState = maps:fold(fun(ChannelId, _, S) -> 
                                 case unsubscribe_ll(S, ChannelId, Pid) of
                                     {ok, NewState} -> NewState;
                                     _ -> S
                                 end
                         end, State, subscribed_channels(State#state.channels, Pid)),
    {noreply, NewState};
handle_info(Info, State) ->
    io:format("INFO: ~p~n", [Info]),
    {noreply, State}.

% Internal functions

find_channel(State, ChannelId) ->
    maps:find(ChannelId, State#state.channels).

with_channel(State, ChannelId, Fun) ->
    case find_channel(State, ChannelId) of
        error ->
            {{error, channel_doesnt_exists, [ChannelId]}, State};
        {ok, Channel} ->
            Fun(Channel)
    end.

publish_ll(State, ChannelId, Message) ->
    with_channel(State, ChannelId, fun(Ch) ->
        io:format("Notice ~p subscribers...~n", [length(Ch#channel.subscribers)]),
        lists:foreach(fun(#subscriber{pid=Pid}) ->
            Pid ! Message 
        end, Ch#channel.subscribers),
        {ok, State}
    end).

subscribe_ll(State, ChannelId, Pid) ->
    with_channel(State, ChannelId, fun(Ch) ->
        case add_subscriber(Ch#channel.subscribers, Pid) of
            {ok, Subscribers} ->
                Channel = Ch#channel{subscribers=Subscribers},
                NewState = State#state{channels=maps:put(ChannelId, Channel, State#state.channels)},
                io:format("CHANNELS = ~p~n", [subscribed_channels(NewState#state.channels, Pid)]),
                {ok, NewState};
            Error -> {Error, State}
        end
    end).

unsubscribe_ll(State, Channel, Pid) when is_record(Channel, channel) ->
    case remove_subscriber(Channel#channel.subscribers, Pid) of
        {ok, Subscribers} ->
            NewChannel = Channel#channel{subscribers=Subscribers},
            NewState = State#state{channels=maps:put(Channel#channel.id, NewChannel, State#state.channels)},
            {ok, NewState};
        Error -> {Error, State}
    end;
unsubscribe_ll(State, ChannelId, Pid) ->
    with_channel(State, ChannelId, fun(Ch) ->
        unsubscribe_ll(State, Ch, Pid)
    end).

create_ll(State, ChannelId) ->
    case find_channel(State, ChannelId) of
        error ->
            Channel = #channel{id=ChannelId, subscribers=[]},
            NewState = State#state{channels=maps:put(ChannelId, Channel, State#state.channels)},
            {ok, NewState};
        _ ->
            {{error, channel_already_exists, [ChannelId]}, State}
    end.

add_subscriber(Subscribers, Pid) ->
    case find_subscriber(Subscribers, Pid) of
        false ->
            Ref = monitor(process, Pid),
            {ok, Subscribers ++ [#subscriber{pid=Pid, ref=Ref}]};
        _ ->
            {error, pid_is_already_subscribed, [Pid]}
    end.

remove_subscriber(Subscribers, Pid) ->
    case find_subscriber(Subscribers, Pid) of
        false ->
            {error, pid_is_not_subscribed};
        #subscriber{ref=Ref} ->
            demonitor(Ref),
            {ok, lists:keydelete(Pid, 2, Subscribers)}
    end.

find_subscriber(Subscribers, Pid) ->
    lists:keyfind(Pid, 2, Subscribers).

subscribed_channels(Channels, Pid) ->
    maps:filter(fun(_, C) ->
        case find_subscriber(C#channel.subscribers, Pid) of
            false -> false;
            _ -> true
        end
    end, Channels).
