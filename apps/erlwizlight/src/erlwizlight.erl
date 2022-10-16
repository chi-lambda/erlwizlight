-module(erlwizlight).

-export([discover/0, list/0, on/1, off/1, dim/2]).

-spec discover() -> ok.
discover() ->
    erlwizlight_discovery:discover().

-spec list() -> [erlwizlight_bulb:bulb()].
list() ->
    Bulbs = erlwizlight_registry:get_all(),
    lists:sort(fun(#{name := Name1}, #{name := Name2}) -> Name1 =< Name2 end, Bulbs).

-spec on(string()) -> ok.
on(Name) ->
    Mac = erlwizlight_storage:get_mac(Name),
    erlwizlight_bulb:on(Mac).

-spec off(string()) -> ok.
off(Name) ->
    Mac = erlwizlight_storage:get_mac(Name),
    erlwizlight_bulb:off(Mac).

-spec dim(string(), integer()) -> ok.
dim(Name, Dimming) ->
    Mac = erlwizlight_storage:get_mac(Name),
    erlwizlight_bulb:set_dimming(Mac, Dimming).
