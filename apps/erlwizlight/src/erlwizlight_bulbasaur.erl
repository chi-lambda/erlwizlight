-module(erlwizlight_bulbasaur).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([start_link/1]).
-export([discover/0, on/1, off/1, set_dimming/2]).

-define(REGISTER_MESSAGE, "{\"method\":\"getPilot\", \"params\":{}}").
-define(PORT, 38899).

%%% gen_server callbacks

init(_Args) ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, true}]),
    {ok, Socket}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({discover, {_Ip1, _Ip2, _Ip3, 255} = IpAddress}, Socket) ->
    discovery(IpAddress, Socket),
    {noreply, Socket};
handle_cast({on, Key}, Socket) ->
    #{ip := Ip} = erlwizlight_registry:get(Key),
    ok =
        gen_udp:send(Socket, Ip, ?PORT, "{\"method\":\"setPilot\",\"params\":{\"state\":true}}"),
    {noreply, Socket};
handle_cast({off, Key}, Socket) ->
    #{ip := Ip} = erlwizlight_registry:get(Key),
    ok =
        gen_udp:send(Socket, Ip, ?PORT, "{\"method\":\"setPilot\",\"params\":{\"state\":false}}"),
    {noreply, Socket};
handle_cast({dimming, Key, Dimming}, Socket)
    when is_integer(Dimming), Dimming =< 100, Dimming >= 10 ->
    #{ip := Ip} = erlwizlight_registry:get(Key),
    ok =
        gen_udp:send(Socket,
                     Ip,
                     ?PORT,
                     io_lib:format("{\"method\":\"setPilot\",\"params\":{\"dimming\":~B}}",
                                   [max(10, Dimming)])),
    {noreply, Socket};
handle_cast(Msg, State) ->
    logger:warning("Unhandled cast: ~p", [Msg]),
    {noreply, State}.

handle_info({udp, Socket, IpAddress, _Port, Msg}, Socket) ->
    logger:debug("Received from ~p: ~p~n", [IpAddress, Msg]),
    add_bulb(jsone:decode(Msg, [{object_format, map}]), IpAddress),
    {noreply, Socket};
handle_info(Msg, State) ->
    logger:warning("You moron, you need to handle ~p~n", [Msg]),
    {noreply, State}.

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ok, Opts).

%%% interface functions

discover() ->
    {ok, BroadcastAddress} = application:get_env(erlwizlight, broadcast_address),
    gen_server:cast(?MODULE, {discover, BroadcastAddress}).

on(Key) ->
    gen_server:cast(?MODULE, {on, Key}).

off(Key) ->
    gen_server:cast(?MODULE, {off, Key}).

set_dimming(Key, Dimming) ->
    gen_server:cast(?MODULE, {dimming, Key, Dimming}).

%%% private functions

discovery({Ip1, Ip2, Ip3, 255} = _IpAddress, Socket) ->
    {ok, MinAddress} = application:get_env(erlwizlight, min_address),
    {ok, MaxAddress} = application:get_env(erlwizlight, max_address),
    logger:debug("Discovering ~p.~p.~p.~p..~p", [Ip1, Ip2, Ip3, MinAddress, MaxAddress]),
    lists:foreach(fun(Ip4) -> ok = send_discovery(Socket, {Ip1, Ip2, Ip3, Ip4}) end,
                  lists:seq(MinAddress, MaxAddress)).

send_discovery(Socket, IpAddress) ->
    gen_udp:send(Socket, IpAddress, ?PORT, ?REGISTER_MESSAGE).

add_bulb(#{<<"result">> :=
               #{<<"mac">> := Mac,
                 <<"state">> := State,
                 <<"sceneId">> := SceneId,
                 <<"r">> := R,
                 <<"g">> := G,
                 <<"b">> := B,
                 <<"c">> := C,
                 <<"w">> := W,
                 <<"dimming">> := Dimming}} =
             _Response,
         IpAddress) ->
    logger:debug("Adding bulb ~p at ~p.~n", [Mac, IpAddress]),

    start_or_update_bulb(#{ip => IpAddress,
                           mac => binary_to_list(Mac),
                           state => State,
                           sceneId => SceneId,
                           rgbcw => {R, G, B, C, W},
                           dimming => Dimming});
add_bulb(#{<<"result">> :=
               #{<<"mac">> := Mac,
                 <<"state">> := State,
                 <<"sceneId">> := SceneId,
                 <<"temp">> := Temp,
                 <<"dimming">> := Dimming}} =
             _Response,
         IpAddress) ->
    logger:debug("Adding bulb ~p at ~p.~n", [Mac, IpAddress]),
    start_or_update_bulb(#{ip => IpAddress,
                           mac => binary_to_list(Mac),
                           state => State,
                           sceneId => SceneId,
                           temp => Temp,
                           dimming => Dimming});
add_bulb(#{<<"result">> :=
               #{<<"mac">> := Mac,
                 <<"state">> := State,
                 <<"sceneId">> := SceneId}} =
             _Response,
         IpAddress) ->
    logger:debug("Adding bulb ~p at ~p.~n", [Mac, IpAddress]),
    start_or_update_bulb(#{ip => IpAddress,
                           mac => binary_to_list(Mac),
                           state => State,
                           sceneId => SceneId});
add_bulb(_, _) ->
    ok.

start_or_update_bulb(#{mac := Mac} = Bulb) ->
    case erlwizlight_registry:whereis_name(Mac) of
        undefined ->
            erlwizlight_bulb_sup:start_child(Bulb);
        Pid ->
            gen_server:call(Pid, {update, Bulb})
    end.
