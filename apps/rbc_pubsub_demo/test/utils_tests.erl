-module(utils_tests).
-include_lib("eunit/include/eunit.hrl").

-record(r, {k, v}).

ets_test() ->
    Tab = ets:new(test_tab, [private, set, {keypos, 2}]),
    Records = [#r{k=a, v=1},
               #r{k=b, v=2},
               #r{k=xx, v=3},
               #r{k=c, v=4}],
    Keys_orig = lists:map(fun(X) -> ets:insert(Tab, X), X#r.k end, Records),
    Keys_find = utils:ets_list(Tab),
    ?assertEqual(sets:from_list(Keys_orig), sets:from_list(Keys_find)),
    ok.
