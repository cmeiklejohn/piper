%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Christopher Meiklejohn
%% @doc Piper application.

-module(piper_app).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-behaviour(application).

%% API.
-export([start/2, stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for piper.
start(_Type, _StartArgs) ->
    Listener = http,
    Port = 8080,
    NumberOfAcceptors = 100,
    Dispatch = [
        {'_', [
            {[<<"websocket">>], piper_websocket, []},
            {['...'], cowboy_static, [
                {directory, {priv_dir, piper, []}},
                {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
            ]}
        ]}
    ],
    Status = cowboy:start_http(Listener,
                               NumberOfAcceptors,
                               [{port, Port}],
                               [{dispatch, Dispatch}]),
    case Status of
        {error, _} ->
            lager:info("Application failed to start on port ~p", [Port]),
            ok;
        {ok, _Pid} ->
            lager:info("Application listening on port ~p", [Port]),
            piper_sup:start_link()
    end.

%% @doc application stop callback for piper.
stop(_State) ->
    ok.
