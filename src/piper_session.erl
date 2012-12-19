%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Christopher Meiklejohn
%% @doc Piper session manager.

-module(piper_session).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-behaviour(gen_server).

%% API
-export([start_link/0,
         clients/0,
         register_client/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {clients=undefined}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register_client(Pid) ->
    gen_server:call(?MODULE, {register_client, Pid}, infinity).

clients() ->
    gen_server:call(?MODULE, clients, infinity).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Clients = sets:new(),
    {ok, #state{clients=Clients}}.

handle_call(count_clients, _From, State) ->
    Count = count_clients(State#state.clients),
    {reply, {ok, Count}, State};
handle_call({register_client, Pid}, _From, State) ->
    NewClients = register_client(Pid, State#state.clients),
    {reply, ok, State#state{clients=NewClients}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @spec register_client(pid(), set()) -> set()
%% @doc Register a new client.
register_client(Pid, Clients) ->
    sets:add_element(Pid, Clients).

%% @spec count_clients(set()) -> number()
%% @doc Count clients.
count_clients(Clients) ->
    sets:size(Clients).
