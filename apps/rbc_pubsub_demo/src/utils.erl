-module(utils).

-export([ets_list/1]).

-spec ets_list(ets:tab()) -> list().
ets_list(Tab) ->
    ets_list(Tab, ets:first(Tab), []).


ets_list(_Tab, '$end_of_table', Acc) ->
    Acc;
ets_list(Tab, Key, Acc) ->
    ets_list(Tab, ets:next(Tab, Key), [Key|Acc]).
