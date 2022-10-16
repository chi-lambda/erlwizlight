-module(erlwizlight_registry).

-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3]).
-export([start_link/1]).
-export([register_name/2, unregister_name/1, whereis_name/1, send/2, get_all/0]).

-type state() :: #{string => pid()}.

-spec init(any()) -> {ok, state()}.
init(_Args) ->
    {ok, #{}}.

-spec handle_cast({send, string(), any()}, state()) -> {noreply, state()}.
handle_cast({send, Name, Msg}, State) when is_map_key(Name, State) ->
    maps:get(Name, State) ! Msg,
    {noreply, State};
handle_cast(Msg, State) ->
    logger:warning("Unhandled cast ~p with state ~p", [Msg, State]),
    {noreply, State}.

-spec handle_call(get_all |
                  {unregister, string()} |
                  {whereis, string()} |
                  {register, string(), pid()},
                  gen_server:from(),
                  state()) ->
                     {noreply, state()} | {reply, pid() | ok | yes | no, state}.
handle_call({register, Name, _Pid}, _From, State) when is_map_key(Name, State) ->
    {reply, no, State};
handle_call({register, Name, Pid}, _From, State) ->
    {reply, yes, State#{Name => Pid}};
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

-spec start_link([gen_server:start_opt()]) -> gen_server:start_ret().
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ok, Opts).

-spec register_name(string(), pid()) -> yes | no.
register_name(Name, Pid) when is_list(Name), is_pid(Pid) ->
    gen_server:call(?MODULE, {register, Name, Pid}).

-spec unregister_name(list()) -> any().
unregister_name(Name) when is_list(Name) ->
    gen_server:call(?MODULE, {unregister, Name}).

-spec whereis_name(list()) -> any().
whereis_name(Name) when is_list(Name) ->
    gen_server:call(?MODULE, {whereis, Name}).

-spec send(list(), _) -> ok.
send(Name, Msg) when is_list(Name) ->
    gen_server:cast(?MODULE, {send, Name, Msg}).

-spec get_all() -> [erlwizlight_bulb:bulb()].
get_all() ->
    gen_server:call(?MODULE, get_all).
