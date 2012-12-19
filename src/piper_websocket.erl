%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Christopher Meiklejohn
%% @doc Piper application.

-module(piper_websocket).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-export([init/3]).

-export([websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    Message = {struct, [{type, generic}, {payload, <<"Hello!">>}]},
    EncodedMessage = mochijson2:encode(Message),
    erlang:start_timer(1000, self(), EncodedMessage),
    {ok, Req, undefined_state}.

websocket_handle({text, _Msg}, Req, State) ->
    Message = {struct, [{type, generic}, {payload, <<"Got it!">>}]},
    EncodedMessage = mochijson2:encode(Message),
    {reply, {text, EncodedMessage}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    Message = {struct, [{type, generic}, {payload, <<"How' you doin'?">>}]},
    EncodedMessage = mochijson2:encode(Message),
    erlang:start_timer(1000, self(), EncodedMessage),
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Taken from Joe Armstrong's EZWebFrame
%%      https://github.com/joearms/ezwebframe
atomize({struct,L}) ->
    {struct, [{binary_to_atom(I), atomize(J)} || {I, J} <- L]};
atomize(L) when is_list(L) ->
    [atomize(I) || I <- L];
atomize(X) ->
    X.

%% @doc Taken from Joe Armstrong's EZWebFrame
%%      https://github.com/joearms/ezwebframe
binary_to_atom(B) ->
    list_to_atom(binary_to_list(B)).
