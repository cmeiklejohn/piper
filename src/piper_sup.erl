%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Christopher Meiklejohn
%% @doc Piper supervisor.

-module(piper_sup).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Procs = [],
    {ok, {{one_for_one, 10, 10}, Procs}}.
