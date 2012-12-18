%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Christopher Meiklejohn
%% @doc Piper application.

-module(piper_app).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-behaviour(application).

%% API.
-export([start/2, stop/1]).

%% API.
start(_Type, _Args) ->
    Listener = http,
    Port = 8080,
    NumberOfAcceptors = 100,
    Dispatch = [
        {'_', [
            {[<<"websockets">>], piper_websockets_handler, []},
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

stop(_State) ->
    ok.
