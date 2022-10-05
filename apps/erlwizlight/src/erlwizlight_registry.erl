-module(erlwizlight_registry).

-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3, terminate/2]).
-export([start_link/1]).
-export([add/2, get/1, get_all/0]).

init(_Args) ->
    {ok, #{}}.

handle_cast({add, Name, Bulb}, State) ->
    {noreply, State#{Name => Bulb}}.

handle_call({get, Name}, _From, State) ->
    {reply, maps:get(Name, State, no_bulb())};
handle_call(get_all, _From, State) ->
    {reply, State, State}.

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ok, Opts).

terminate(_Reason, _State) ->
    ok.

add(Name, Bulb) ->
    gen_server:cast(?MODULE, {add, Name, Bulb}).

get(Name) ->
    gen_server:call(?MODULE, {get, Name}).

get_all() ->
    gen_server:call(?MODULE, get_all).

no_bulb() ->
    undefined.
