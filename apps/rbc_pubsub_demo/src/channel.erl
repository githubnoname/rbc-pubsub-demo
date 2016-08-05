-module(channel).
-behaviour(gen_server).

-export([start_link/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3, handle_cast/2]).

% channel API
-export([publish/2, subscribe/2, unsubscribe/2]).

-include("pubsub.hrl").

-record(state, {subscribers=[] :: [subscriber()]}).
-record(subscriber, {pid :: pid()}).

-type state() :: #state{}.
-type subscriber() :: #subscriber{}.


start_link(ChannelId) ->
    Name = atom_to_list(?MODULE) ++ atom_to_list(ChannelId),
    gen_server:start_link({local, list_to_atom(Name)}, ?MODULE, [], []).


init(_Args) ->
    {ok, #state{}}.


% channel API

-spec publish(pid(), pubsub_message()) -> ok.
publish(ChannelPid, Message) ->
    gen_server:cast(ChannelPid, {publish, Message}).


-spec subscribe(pid(), pid()) -> ok | already_subscribed().
subscribe(ChannelPid, SubPid) ->
    gen_server:call(ChannelPid, {subscribe, SubPid}).


-spec unsubscribe(pid(), pid()) -> ok | is_not_subscribed().
unsubscribe(ChannelPid, SubPid) ->
    gen_server:call(ChannelPid, {unsubscribe, SubPid}).


% gen_server

handle_call({subscribe, Pid}, _From, State) ->
    {Result, NewState} = subscribe_ll(State, Pid),
    {reply, Result, NewState};
handle_call({unsubscribe, Pid}, _From, State) ->
    {Result, NewState} = unsubscribe_ll(State, Pid),
    {reply, Result, NewState}.


handle_cast({publish, Message}, State) ->
    NewState = publish_ll(State, Message),
    {noreply, NewState}.


%% Remove down process from subscriptions
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    {_, NewState} = unsubscribe_ll(State, Pid),
    {noreply, NewState}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal functions

-spec subscribe_ll(state(), pid()) -> {ok, state()} | {already_subscribed(), state()}.
subscribe_ll(State, Pid) ->
    case add_subscriber(State#state.subscribers, Pid) of
        {ok, Subscribers} ->
            NewState = State#state{subscribers=Subscribers},
            {ok, NewState};
        Error -> {Error, State}
    end.


-spec unsubscribe_ll(state(), pid()) -> {ok, state()} | {is_not_subscribed(), state()}.
unsubscribe_ll(State, Pid) ->
    case remove_subscriber(State#state.subscribers, Pid) of
        {ok, Subscribers} ->
            NewState = State#state{subscribers=Subscribers},
            {ok, NewState};
        Error -> {Error, State}
    end.


-spec publish_ll(state(), pubsub_message()) -> state().
publish_ll(State, Message_) ->
    Message = Message_#pubsub_message{date=erlang:localtime()},
    lists:foreach(fun(#subscriber{pid=Pid}) -> 
        Pid ! Message
    end, State#state.subscribers),
    State.


-spec add_subscriber([subscriber()], pid()) -> {ok, [subscriber()]} | already_subscribed().
add_subscriber(Subscribers, Pid) ->
    case find_subscriber(Subscribers, Pid) of
        false ->
            {ok, Subscribers ++ [#subscriber{pid=Pid}]};
        _ ->
            {error, pid_is_already_subscribed, [Pid]}
    end.


-spec remove_subscriber([subscriber()], pid()) -> is_not_subscribed() | {ok, [subscriber()]}.
remove_subscriber(Subscribers, Pid) ->
    case find_subscriber(Subscribers, Pid) of
        false ->
            {error, pid_is_not_subscribed, [Pid]};
        _ ->
            {ok, lists:keydelete(Pid, 2, Subscribers)}
    end.


-spec find_subscriber([subscriber()], pid()) -> subscriber() | false.
find_subscriber(Subscribers, Pid) ->
    lists:keyfind(Pid, 2, Subscribers).
