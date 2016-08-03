-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3, websocket_handle/3, websocket_terminate/3, websocket_info/3]).

-include("pubsub.hrl").


init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.


websocket_init(_Transport, Req, _Opts) ->
    {ok, Req, undefined_state}.


websocket_handle({text, Msg}, Req, State) ->
    Data = jsx:decode(Msg, [return_maps]),
    Out = jsx:encode(process_data(Data)),
    {reply, {text, Out}, Req, State}.


websocket_terminate(_Reason, _Req, _State) ->
    ok.


websocket_info(#pubsub_message{sender=Sender, data=Data, date=Date}, Req, State) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = Date,
    DateStr = io_lib:format("~2..0w/~2..0w/~w ~2..0w:~2..0w:~2..0w", [Month, Day, Year, Hour, Min, Sec]),
    Response = #{<<"action">> => <<"message">>,
                 <<"message">> => #{
                   <<"sender">> => to_binary(Sender),
                   <<"data">> => to_binary(Data),
                   <<"date">> => to_binary(DateStr)
                  }
                },
    {reply, {text, jsx:encode(Response)}, Req, State}.


process_data(#{<<"action">> := <<"list_channels">>} = Data) ->
    Data#{<<"channels">> => lists:map(fun to_binary/1, pubsub:list_channels())};
process_data(#{<<"action">> := <<"subscribe">>,
               <<"channel">> := Channel} = Data) ->
    Result = pubsub:subscribe(self(), to_atom(Channel)),
    result(Data, Result);
process_data(#{<<"action">> := <<"unsubscribe">>,
               <<"channel">> := Channel} = Data) ->
    Result = pubsub:unsubscribe(self(), to_atom(Channel)),
    result(Data, Result);
process_data(#{<<"action">> := <<"publish">>,
               <<"channel">> := Channel,
               <<"message">> := #{
                 <<"sender">> := Sender,
                 <<"data">> := Message
                }} = Data) ->
    Result = pubsub:publish(#pubsub_message{
                               sender = Sender,
                               data = Message
                              }, to_atom(Channel)),
    result(Data, Result).


result(Data, Result) ->
    Data#{<<"result">> => to_map(Result)}.


to_map(ok) ->
    <<"ok">>;
to_map({error, Reason, _Extra}) when is_atom(Reason) ->
    #{<<"error">> => to_binary(Reason)}.


to_binary(X) when is_atom(X) ->
    atom_to_binary(X, utf8);
to_binary(X) when is_list(X) ->
    list_to_binary(X);
to_binary(X) when is_binary(X) ->
    X.


to_atom(X) when is_binary(X) ->
    binary_to_atom(X, utf8).
