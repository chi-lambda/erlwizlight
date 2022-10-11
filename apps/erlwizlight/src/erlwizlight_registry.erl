-module(erlwizlight_registry).

-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3, terminate/2]).
-export([start_link/1]).
-export([register_name/2, unregister_name/1, whereis_name/1, send/2, get_all/0]).

init(_Args) ->
    {ok, #{}}.

handle_cast({send, Name, Msg}, State) ->
    case maps:is_key(Name, State) of
        true ->
            maps:get(Name, State) ! Msg;
        false ->
            ok
    end,
    {noreply, State}.

handle_call({register, Name, Pid}, _From, State) ->
    case maps:is_key(Name, State) of
        false ->
            {reply, yes, State#{Name => Pid}};
        true ->
            {reply, no, State}
    end;
handle_call({unregister, Name}, _From, State) ->
    {reply, ok, maps:remove(Name, State)};
handle_call({whereis, Name}, _From, State) ->
    {reply, maps:get(Name, State, undefined), State};
handle_call(get_all, From, State) ->
    spawn(fun() ->
             gen_server:reply(From,
                              lists:map(fun(Pid) -> erlwizlight_bulb:get_info(Pid) end,
                                        maps:values(State)))
          end),
    {noreply, State}.

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ok, Opts).

terminate(_Reason, _State) ->
    ok.

register_name(Name, Pid) ->
    gen_server:call(?MODULE, {register, Name, Pid}).

unregister_name(Name) ->
    gen_server:call(?MODULE, {unregister, Name}).

whereis_name(Name) ->
    gen_server:call(?MODULE, {whereis, Name}).

send(Name, Msg) ->
    gen_server:cast(?MODULE, {send, Name, Msg}).

get_all() ->
    gen_server:call(?MODULE, get_all).
