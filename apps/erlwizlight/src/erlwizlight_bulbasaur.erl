-module(erlwizlight_bulbasaur).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([start_link/1]).
-export([discover/0]).

-define(REGISTER_MESSAGE, "{\"method\":\"getPilot\", \"params\":{}}").

init(_Args) ->
    {ok, Socket} = gen_udp:open(38899, [binary, {active, false}]),
    {ok, Socket}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({discover, {Ip1, Ip2, Ip3, 255} = _IpAddress}, Socket) ->
    {ok, MinAddress} = application:get_env(erlwizlight, min_address),
    {ok, MaxAddress} = application:get_env(erlwizlight, max_address),
    logger:debug("Discovering ~p.~p.~p.~p..~p", [Ip1, Ip2, Ip3, MinAddress, MaxAddress]),
    inet:setopts(Socket, [{active, false}]),
    lists:foreach(fun(Ip4) -> ok = send_discovery(Socket, {Ip1, Ip2, Ip3, Ip4}) end,
                  lists:seq(MinAddress, MaxAddress)),
    inet:setopts(Socket, [{active, true}]),
    {noreply, Socket};
handle_cast(_Msg, State) ->
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

discover() ->
    {ok, BroadcastAddress} = application:get_env(erlwizlight, broadcast_address),
    gen_server:cast(?MODULE, {discover, BroadcastAddress}).

send_discovery(Socket, IpAddress) ->
    gen_udp:send(Socket, IpAddress, 38899, ?REGISTER_MESSAGE).

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
    erlwizlight_registry:add(binary_to_list(Mac),
                             #{ip => IpAddress,
                               state => State,
                               sceneId => SceneId,
                               rgbcw => {R, G, B, C, W},
                               dimming => Dimming});
add_bulb(#{<<"result">> :=
               #{<<"mac">> := Mac,
                 <<"state">> := State,
                 <<"sceneId">> := SceneId}} =
             _Response,
         IpAddress) ->
    logger:debug("Adding bulb ~p at ~p.~n", [Mac, IpAddress]),
    erlwizlight_registry:add(binary_to_list(Mac),
                             #{ip => IpAddress,
                               state => State,
                               sceneId => SceneId});
add_bulb(_, _) ->
    ok.
