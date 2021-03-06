-module(pubsub_tests).
-include_lib("eunit/include/eunit.hrl").
-include("pubsub.hrl").

channels_test() ->
    %% start pubsub
    pubsub_sup:start_link(),
    %% no one channels on start
    ?assertEqual([], pubsub:list_channels()),
    %% create first channel
    ?assertMatch(Pid when is_pid(Pid), pubsub:create_channel(channel1)),
    %% create first channel again...
    ?assertEqual({error, channel_already_exists, [channel1]}, pubsub:create_channel(channel1)),
    %% create second channel
    ?assertMatch(Pid when is_pid(Pid), pubsub:create_channel(channel2)),
    %% check channels
    ?assertEqual(sets:from_list([channel1, channel2]), sets:from_list(pubsub:list_channels())),
    %% stop pubsub
    stop_pubsub().
    

subscribe_test() ->
    %% start pubsub
    pubsub_sup:start_link(),
    %% create process
    S = subscriber(),
    ?assert(is_pid(S)),
    %% subscribe to non-existent channel
    ?assertEqual({error, channel_doesnt_exist, [channel]}, pubsub:subscribe(S, channel)),
    %% create channel
    ?assertMatch(Pid when is_pid(Pid), pubsub:create_channel(channel)),
    %% subscribe to the channel
    ?assertEqual(ok, pubsub:subscribe(S, channel)),
    %% subscribe to the channel again
    ?assertEqual({error, pid_is_already_subscribed, [S]}, pubsub:subscribe(S, channel)),
    %% stop pubsub
    stop_pubsub().


unsubscribe_test() ->
    %% start pubsub
    pubsub_sup:start_link(),
    %% create process
    S = subscriber(),
    ?assert(is_pid(S)),
    %% unsubscribe from non-existent channel
    ?assertEqual({error, channel_doesnt_exist, [channel]}, pubsub:unsubscribe(S, channel)),
    %% create channel
    ?assertMatch(Pid when is_pid(Pid), pubsub:create_channel(channel)),
    %% unsubscribe from non-subscribed channel
    ?assertEqual({error, pid_is_not_subscribed, [S]}, pubsub:unsubscribe(S, channel)),
    %% subscribe to the channel
    ?assertEqual(ok, pubsub:subscribe(S, channel)),
    %% unsubscribe from them
    ?assertEqual(ok, pubsub:unsubscribe(S, channel)),
    %% unsubscribe again?
    ?assertEqual({error, pid_is_not_subscribed, [S]}, pubsub:unsubscribe(S, channel)),
    %% stop pubsub
    stop_pubsub().


publish_test() ->
    %% start pubsub
    pubsub_sup:start_link(),
    %% create process
    S = subscriber(),
    ?assert(is_pid(S)),
    %% publish to non-existen channel
    ?assertEqual({error, channel_doesnt_exist, [channel]}, pubsub:publish(msg(), channel)),
    %% create channel
    ?assertMatch(Pid when is_pid(Pid), pubsub:create_channel(channel)),
    %% publish to the non-subscribed channel
    ?assertEqual(ok, pubsub:publish(msg(), channel)),
    %% subscribe to the channel
    ?assertEqual(ok, pubsub:subscribe(S, channel)),
    %% check process is alive
    ?assert(process_info(S) /= undefined),
    %% publish to the channel
    ?assertEqual(ok, pubsub:publish(msg(), channel)),
    %% check process is stopped, that mean it has received message
    wait_pid(S, 1000),
    ?assertEqual(undefined, process_info(S)),
    %% stop pubsub
    stop_pubsub().


% Helpers

subscriber() ->
    spawn(fun() ->
        receive
            _ -> ok
        end
    end).

msg() ->
    #pubsub_message{data = hello, sender = world}.

stop_pubsub() ->
    Pid = whereis(pubsub_sup),
    unlink(Pid),
    exit(Pid, shutdown).

wait_pid(Pid, Timeout) ->
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    after Timeout ->
            ok
    end.
