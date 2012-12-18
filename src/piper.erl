%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Christopher Meiklejohn
%% @doc Piper.

-module(piper).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(crypto),
    ensure_started(ranch),
    ensure_started(cowboy),
    piper_sup:start_link().

%% @spec start() -> ok
%% @doc Start the piper server.
start() ->
    ensure_started(crypto),
    ensure_started(ranch),
    ensure_started(cowboy),
    application:start(piper).

%% @spec stop() -> ok
%% @doc Stop the piper server.
stop() ->
    Res = application:stop(piper),
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(crypto),
    Res.
