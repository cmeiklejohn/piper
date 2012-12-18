%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Christopher Meiklejohn
%% @doc Piper application.

-module(piper_app).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
    Port = 8080,
    Dispatch = [
        {'_', [
            {['...'], cowboy_static, [
                {directory, {priv_dir, piper, []}},
                {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
            ]}
        ]}
    ],
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
        {dispatch, Dispatch}
    ]),
    lager:info("Application listening on port ~p", [Port]),
    piper_sup:start_link().

stop(_State) ->
    ok.
