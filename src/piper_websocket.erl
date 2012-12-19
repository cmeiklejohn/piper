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

%% @doc Register client with the gen_server initially when connected.
websocket_init(_TransportName, Req, _Opts) ->
    ok = piper_session:register_client(self()),
    {ok, Req, undefined_state}.

%% @doc Each time a message is received from the client, decode the
%% message and send to the gen server.
websocket_handle({text, Msg}, Req, State) ->
    Res = case mochijson2:decode(Msg) of
        {struct, _} = Struct ->
            {struct, Atomized} = atomize(Struct),
            RawMessage = proplists:get_value(message, Atomized),
            Message = binary_to_atom(RawMessage),
            gen_server:cast(piper_session, {Message, self()}),
            ok;
        _ ->
            ok
    end,
    {Res, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

%% @doc When we receive a response, turn the X-tuple into a message back
%% to the client over the websocket channel.
websocket_info({Type, Message}, Req, State) ->
    Response = mochijson2:encode({struct, [{type, Type}, {message, Message}]}),
    {reply, {text, Response}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok = piper_session:deregister_client(self()),
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
