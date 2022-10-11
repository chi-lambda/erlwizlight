-module(erlwizlight_bulb).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, start_link/2]).
-export([on/1, off/1, update/2, get_info/1]).

-define(GETPILOT, "{\"method\":\"getPilot\", \"params\":{}}").
-define(PORT, 38899).

%%% gen_server functions

init(Bulb) ->
    {ok, Socket} = gen_udp:open(0), % open ephemeral port
    {ok, TimerRef} = timer:send_interval(60000, refresh),
    {ok,
     #{socket => Socket,
       bulb => Bulb,
       timer_ref => TimerRef}}.

handle_cast({update, Bulb}, State) ->
    {noreply, State#{bulb => Bulb}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(get_info, _From, #{bulb := Bulb} = State) ->
    {reply, Bulb, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_info(refresh, #{socket := Socket, bulb := #{ip := IpAddress}} = State) ->
    gen_udp:send(Socket, IpAddress, ?PORT, ?GETPILOT),
    {noreply, State};
handle_info({udp, Socket, _IpAddress, _Port, _Msg}, #{socket := Socket} = State) ->
    {noreply, State};
handle_info(Msg, State) ->
    logger:warning("You moron, you need to handle ~p", [Msg]),
    {noreply, State}.

start_link(#{mac := Mac} = Bulb, Opts) ->
    gen_server:start_link({via, erlwizlight_registry, Mac}, ?MODULE, Bulb, Opts).

%%% interface function

on(Mac) when is_list(Mac) ->
    gen_server:call({via, erlwizlight_registry, Mac}, on).

off(Mac) when is_list(Mac) ->
    gen_server:call({via, erlwizlight_registry, Mac}, off).

update(Pid, Bulb) when is_pid(Pid) ->
    gen_server:call(Pid, {update, Bulb}).

get_info(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, get_info);
get_info(Mac) when is_list(Mac) ->
    gen_server:call({via, erlwizlight_registry, Mac}, get_info).
