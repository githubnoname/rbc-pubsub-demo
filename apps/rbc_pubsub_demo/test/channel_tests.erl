-module(channel_tests).
-include_lib("eunit/include/eunit.hrl").
-include("pubsub.hrl").

%% Test just a channel module.
%% So, we have to directly start and stop channel process


subscribe_test() ->
    %% Start a process
    {ok, Pid} = channel:start_link(channel),
    %% Create subscriber
    S = subscriber(),
    %% Subscribe on the channel
    ?assertEqual(ok, channel:subscribe(Pid, S)),
    %% Subscribe the same pid to the channel
    ?assertEqual({error, pid_is_already_subscribed, [S]}, channel:subscribe(Pid, S)),
    %% Subscribe another pid to the channel
    ?assertEqual(ok, channel:subscribe(Pid, subscriber())),
    %% Stop the process
    gen_server:stop(Pid).


unsubscribe_test() ->
    {ok, Pid} = channel:start_link(channel),
    S = subscriber(),
    %% Unsubscribe non-subscribed pid
    ?assertEqual({error, pid_is_not_subscribed, [S]}, channel:unsubscribe(Pid, S)),
    %% Subscribe
    ?assertEqual(ok, channel:subscribe(Pid, S)),
    %% Unsubscribe
    ?assertEqual(ok, channel:unsubscribe(Pid, S)),
    gen_server:stop(Pid).


publish_test() ->
    {ok, Pid} = channel:start_link(channel),
    S = subscriber(),
    %% Subscribe
    ?assertEqual(ok, channel:subscribe(Pid, S)),
    %% Check subscriber alive
    ?assert(process_info(S) /= undefined),
    %% Publish message into the channel
    ?assertEqual(ok, channel:publish(Pid, msg())),
    %% Subscribe should receive the message and die
    wait_pid(S, 1000),
    ?assertEqual(undefined, process_info(S)),
    gen_server:stop(Pid).


%% Helpers

subscriber() ->
    spawn(fun() ->
        receive
            _ -> ok
        end
    end).

msg() ->
    #pubsub_message{data = hello, sender = world}.

wait_pid(Pid, Timeout) ->
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    after Timeout ->
            ok
    end.
