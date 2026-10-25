-module(erlwizlight_bulb).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, start_link/2]).
-export([on/1, off/1, set_dimming/2, update/2, get_info/1, set_name/2]).
-export([get_configs/1]).
-export([debug_send/2]).
-export([json_to_bulb/1]).

-define(GETPILOT, "{\"method\":\"getPilot\", \"params\":{}}").
-define(GETSYSTEMCONFIG, "{\"method\":\"getSystemConfig\", \"params\":{}}").
-define(GETUSERCONFIG, "{\"method\":\"getUserConfig\", \"params\":{}}").
-define(GETMODELCONFIG, "{\"method\":\"getModelConfig\", \"params\":{}}").
-define(PORT, 38899).
-define(JSON_MAPPING,
        [{<<"mac">>, mac},
         {<<"state">>, state},
         {<<"sceneId">>, scene_id},
         {<<"dimming">>, dimming},
         {<<"temp">>, temp},
         {ip, ip}]).

-type bulb() ::
    #{mac := string(),
      name := string(),
      ip := inet:ip_address()}.
-type state() ::
    #{bulb := bulb(),
      socket := gen_udp:socket(),
      timer_ref := timer:tref()}.

%%% gen_server functions

-spec init(bulb()) ->
              {ok,
               #{bulb := bulb(),
                 socket := gen_udp:socket(),
                 timer_ref := timer:tref()}}.
init(#{mac := Mac} = Bulb) ->
    {ok, Socket} = gen_udp:open(0, [binary]), % open ephemeral port
    {ok, TimerRef} = timer:send_interval(60000, refresh),
    Name = erlwizlight_storage:get_name(Mac),
    get_configs(Mac),
    {ok,
     #{socket => Socket,
       bulb => Bulb#{name => Name},
       timer_ref => TimerRef}}.

-spec handle_cast({update, bulb()} | on | off | {set_name, string()}, state()) ->
                     {noreply, state()}.
handle_cast({update, Bulb}, State) ->
    {noreply, State#{bulb => Bulb}};
handle_cast(on, #{socket := Socket, bulb := #{ip := Ip}} = State) ->
    ok =
        gen_udp:send(Socket, Ip, ?PORT, "{\"method\":\"setPilot\",\"params\":{\"state\":true}}"),
    {noreply, State};
handle_cast(off, #{socket := Socket, bulb := #{ip := Ip}} = State) ->
    ok =
        gen_udp:send(Socket, Ip, ?PORT, "{\"method\":\"setPilot\",\"params\":{\"state\":false}}"),
    {noreply, State};
handle_cast({dimming, Dimming}, #{socket := Socket, bulb := #{ip := Ip} = Bulb} = State)
        when is_integer(Dimming), Dimming =< 100, Dimming >= 0 ->
    MinDimming = maps:get(min_dimming, Bulb, 10),
    ok =
        gen_udp:send(Socket,
                     Ip,
                     ?PORT,
                     io_lib:format("{\"method\":\"setPilot\",\"params\":{\"dimming\":~B}}",
                                   [math:max(Dimming, MinDimming)])),
    {noreply, State};
handle_cast({set_name, Name}, #{bulb := #{mac := Mac} = Bulb} = State) ->
    erlwizlight_storage:set_name(Mac, Name),
    {noreply, State#{bulb => Bulb#{name => Name}}};
handle_cast(get_pilot, #{socket := Socket, bulb := #{ip := IpAddress}} = State) ->
    ok = gen_udp:send(Socket, IpAddress, ?PORT, ?GETPILOT),
    {noreply, State};
handle_cast(get_system_config, #{socket := Socket, bulb := #{ip := IpAddress}} = State) ->
    ok = gen_udp:send(Socket, IpAddress, ?PORT, ?GETSYSTEMCONFIG),
    {noreply, State};
handle_cast(get_user_config, #{socket := Socket, bulb := #{ip := IpAddress}} = State) ->
    ok = gen_udp:send(Socket, IpAddress, ?PORT, ?GETUSERCONFIG),
    {noreply, State};
handle_cast(get_model_config, #{socket := Socket, bulb := #{ip := IpAddress}} = State) ->
    ok = gen_udp:send(Socket, IpAddress, ?PORT, ?GETMODELCONFIG),
    {noreply, State};
handle_cast({debug_send, Object},
            #{socket := Socket, bulb := #{ip := IpAddress}} = State) ->
    Json = jsone:encode(Object),
    gen_udp:send(Socket, IpAddress, ?PORT, Json),
    {noreply, State};
handle_cast(Msg, State) ->
    logger:warning("You moron, you didn't handle ~p with state ~p", [Msg, State]),
    {noreply, State}.

-spec handle_call(get_info, gen_server:from(), state()) -> {reply, bulb(), state()}.
handle_call(get_info, _From, #{bulb := Bulb} = State) ->
    {reply, Bulb, State};
handle_call(Msg, From, State) ->
    logger:warning("Called ~p with message ~p and state ~p", [?MODULE, Msg, State]),
    exit(From, badarg),
    {noreply, State}.

-spec handle_info(refresh |
                  {udp, gen_udp:socket(), inet:ip_address(), inet:port_number(), binary()},
                  state()) ->
                     {noreply, state()}.
handle_info(refresh, #{bulb := #{mac := Mac}} = State) ->
    spawn(fun() -> gen_server:cast({via, erlwizlight_registry, Mac}, get_pilot) end),
    get_configs(Mac),
    {noreply, State};
handle_info({udp, Socket, _IpAddress, _Port, Msg},
            #{socket := Socket, bulb := #{name := Name} = Bulb} = State) ->
    logger:debug("Bulb ~p received ~p", [Name, Msg]),
    NewBulb = update_bulb(Bulb, jsone:decode(Msg, [{object_format, map}])),
    {noreply, State#{bulb := NewBulb}};
handle_info(Msg, State) ->
    logger:warning("Unhandled message ~p with state ~p", [Msg, State]),
    {noreply, State}.

-spec start_link(bulb(), [gen_server:start_opt()]) -> gen_server:start_ret().
start_link(#{mac := Mac} = Bulb, Opts) ->
    gen_server:start_link({via, erlwizlight_registry, Mac}, ?MODULE, Bulb, Opts).

%%% interface function

-spec on(string()) -> ok.
on(Mac) when is_list(Mac) ->
    gen_server:cast({via, erlwizlight_registry, Mac}, on).

-spec off(string()) -> ok.
off(Mac) when is_list(Mac) ->
    gen_server:cast({via, erlwizlight_registry, Mac}, off).

-spec set_dimming(string(), integer()) -> ok.
set_dimming(Mac, Dimming) ->
    gen_server:cast({via, erlwizlight_registry, Mac}, {dimming, Dimming}).

-spec update(pid(), bulb()) -> any().
update(Pid, Bulb) when is_pid(Pid) ->
    gen_server:call(Pid, {update, Bulb}).

-spec get_info(pid() | string()) -> bulb().
get_info(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, get_info);
get_info(Mac) when is_list(Mac) ->
    gen_server:call({via, erlwizlight_registry, Mac}, get_info).

-spec set_name(string(), string()) -> ok.
set_name(Mac, Name) ->
    gen_server:cast({via, erlwizlight_registry, Mac}, {set_name, Name}).

debug_send(Mac, Object) ->
    gen_server:cast({via, erlwizlight_registry, Mac}, {debug_send, Object}).

-spec get_configs(string()) -> ok.
get_configs(Mac) ->
    spawn(fun() ->
             gen_server:cast({via, erlwizlight_registry, Mac}, get_system_config),
             gen_server:cast({via, erlwizlight_registry, Mac}, get_user_config),
             gen_server:cast({via, erlwizlight_registry, Mac}, get_model_config)
          end),
    ok.

%%% Utility functions

-spec json_to_bulb(#{binary() | atom() => binary()}) -> bulb().
json_to_bulb(Json) ->
    Mapped =
        lists:foldl(fun ({JsonKey, BulbKey}, Acc)
                            when is_map_key(JsonKey, Json), BulbKey == mac ->
                            [{BulbKey, binary_to_list(maps:get(JsonKey, Json))} | Acc];
                        ({JsonKey, BulbKey}, Acc) when is_map_key(JsonKey, Json) ->
                            [{BulbKey, maps:get(JsonKey, Json)} | Acc];
                        (_, Acc) ->
                            Acc
                    end,
                    [],
                    ?JSON_MAPPING),
    MappedWithRgbcw = add_rgbcw(Mapped, Json),
    maps:from_list(MappedWithRgbcw).

-spec add_rgbcw([{atom(), any()}], #{binary() | atom() => any()}) -> any().
add_rgbcw(List,
          #{<<"r">> := R,
            <<"g">> := G,
            <<"b">> := B,
            <<"c">> := C,
            <<"w">> := W}) ->
    [{rgbcw, {R, G, B, C, W}} | List];
add_rgbcw(List, _) ->
    List.

-spec update_bulb(bulb(), #{}) -> bulb().
update_bulb(Bulb, #{<<"method">> := <<"getPilot">>}) ->
    Bulb;
update_bulb(Bulb, #{<<"method">> := <<"setPilot">>}) ->
    Bulb;
update_bulb(Bulb, #{<<"method">> := <<"getSystemConfig">>, <<"result">> := Result}) ->
    add_system_config(Bulb, Result);
update_bulb(Bulb, #{<<"method">> := <<"getUserConfig">>, <<"result">> := Result}) ->
    add_user_config(Bulb, Result);
update_bulb(Bulb, #{<<"method">> := <<"getModelConfig">>, <<"result">> := Result}) ->
    add_model_config(Bulb, Result),
    Bulb;
update_bulb(#{name := Name, mac := Mac} = Bulb, Msg) ->
    logger:warning(["~p (~p) received unknown message ~p"], [Name, Mac, Msg]),
    Bulb.

-spec add_system_config(bulb(), map()) -> bulb().
add_system_config(Bulb,
                  #{<<"homeId">> := HomeId,
                    <<"roomId">> := RoomId,
                    <<"moduleName">> := ModuleName,
                    <<"fwVersion">> := FwVersion,
                    <<"groupId">> := GroupId}) ->
    Bulb#{home_id => HomeId,
          room_id => RoomId,
          module_name => ModuleName,
          fw_version => FwVersion,
          group_id => GroupId};
add_system_config(#{name := Name, mac := Mac} = Bulb, Config) ->
    logger:warning("Bulb ~p (~p): Unexpected system config: ~p", [Name, Mac, Config]),
    Bulb.

-spec add_user_config(bulb(), map()) -> bulb().
add_user_config(Bulb,
                #{<<"dftDim">> := DftDim,
                  <<"fadeIn">> := FadeIn,
                  <<"fadeOut">> := FadeOut,
                  <<"minDimming">> := MinDimming,
                  <<"opMode">> := OpMode,
                  <<"po">> := PO,
                  <<"tapSensor">> := TapSensor}) ->
    Bulb#{dft_dim => DftDim,
          fade_in => FadeIn,
          fade_out => FadeOut,
          min_dimming => MinDimming,
          op_mode => OpMode,
          po => PO,
          tap_sensor => TapSensor};
add_user_config(#{name := Name, mac := Mac} = Bulb, Config) ->
    logger:warning("Bulb ~p (~p): Unexpected user config: ~p", [Name, Mac, Config]),
    Bulb.

-spec add_model_config(bulb(), map()) -> bulb().
add_model_config(Bulb,
                 #{<<"cctRange">> := CctRange,
                   <<"nowc">> := Nowc,
                   <<"ps">> := PS,
                   <<"pwmFreq">> := PwmFrequency,
                   <<"pwmRange">> := PwmRange,
                   <<"renderFactor">> := RenderFactor,
                   <<"wcr">> := Wcr,
                   <<"wizc1">> := #{<<"mode">> := Wizc1Mode},
                   <<"wizc2">> := #{<<"mode">> := Wizc2Mode}}) ->
    Bulb#{cct_range => CctRange,
          nowc => Nowc,
          ps => PS,
          pwm_freq => PwmFrequency,
          pwm_range => PwmRange,
          render_factor => RenderFactor,
          wcr => Wcr,
          wizc1_mode => Wizc1Mode,
          wizc2_mode => Wizc2Mode};
add_model_config(#{name := Name, mac := Mac} = Bulb, Config) ->
    logger:warning("Bulb ~p (~p): Unexpected model config: ~p", [Name, Mac, Config]),
    Bulb.
