-module(erlwizlight_discovery).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([start_link/1]).
-export([discover/0]).

-define(REGISTER_MESSAGE, "{\"method\":\"getPilot\", \"params\":{}}").
-define(PORT, 38899).

%%% gen_server callbacks

-spec init(any()) -> {ok, gen_udp:socket()}.
init(_Args) ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, true}]),
    spawn(fun() -> discover() end),
    {ok, Socket}.

-spec handle_call(any(), gen_server:from(), gen_udp:socket()) ->
                     {reply, ok, gen_udp:socket()}.
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

-spec handle_cast({discover, inet:ip4_address()}, gen_udp:socket()) ->
                     {noreply, gen_udp:socket()}.
handle_cast({discover, {_Ip1, _Ip2, _Ip3, 255} = IpAddress}, Socket) ->
    discovery(IpAddress, Socket),
    {noreply, Socket};
handle_cast(Msg, State) ->
    logger:warning("Unhandled cast: ~p", [Msg]),
    {noreply, State}.

-spec handle_info({udp,
                   gen_udp:socket(),
                   inet:ip_address(),
                   inet:port_number(),
                   binary()},
                  gen_udp:socket()) ->
                     {noreply, gen_udp:socket()}.
handle_info({udp, Socket, IpAddress, _Port, Msg}, Socket) ->
    logger:debug("Received from ~p: ~p~n", [IpAddress, Msg]),
    add_bulb(jsone:decode(Msg, [{object_format, map}]), IpAddress),
    {noreply, Socket};
handle_info(Msg, State) ->
    logger:warning("You moron, you need to handle ~p~n", [Msg]),
    {noreply, State}.

-spec start_link([gen_server:start_opt()]) -> gen_server:start_ret().
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ok, Opts).

%%% interface functions

-spec discover() -> ok.
discover() ->
    {ok, BroadcastAddress} = application:get_env(erlwizlight, broadcast_address),
    gen_server:cast(?MODULE, {discover, BroadcastAddress}).

%%% private functions

-spec discovery(inet:ip4_address(), gen_udp:socket()) -> ok.
discovery({Ip1, Ip2, Ip3, 255} = _IpAddress, Socket) ->
    {ok, MinAddress} = application:get_env(erlwizlight, min_address),
    {ok, MaxAddress} = application:get_env(erlwizlight, max_address),
    logger:debug("Discovering ~p.~p.~p.~p..~p", [Ip1, Ip2, Ip3, MinAddress, MaxAddress]),
    lists:foreach(fun(Ip4) -> ok = send_discovery(Socket, {Ip1, Ip2, Ip3, Ip4}) end,
                  lists:seq(MinAddress, MaxAddress)).

-spec send_discovery(gen_udp:socket(), inet:ip4_address()) -> ok | {error, atom()}.
send_discovery(Socket, IpAddress) ->
    gen_udp:send(Socket, IpAddress, ?PORT, ?REGISTER_MESSAGE).

-spec add_bulb(map(), inet:ip_address()) -> any().
add_bulb(#{<<"result">> := Bulb}, IpAddress) ->
    logger:debug("Adding bulb ~p at ~p.~n", [Bulb, IpAddress]),

    start_or_update_bulb(erlwizlight_bulb:json_to_bulb(Bulb#{ip => IpAddress}));
add_bulb(_, _) ->
    ok.

-spec start_or_update_bulb(erlwizlight_bulb:bulb()) -> any().
start_or_update_bulb(#{mac := Mac} = Bulb) ->
    case erlwizlight_registry:whereis_name(Mac) of
        undefined ->
            erlwizlight_bulb_sup:start_child(Bulb);
        Pid ->
            erlwizlight_bulb:update(Pid, Bulb)
    end.
