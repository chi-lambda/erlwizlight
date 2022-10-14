-module(erlwizlight).

-export([discover/0, list/0, on/1, off/1, dim/2]).

discover() ->
    erlwizlight_discovery:discover().

list() ->
    Bulbs = erlwizlight_registry:get_all(),
    lists:sort(fun(#{name := Name1}, #{name := Name2}) -> Name1 =< Name2 end, Bulbs).

on(Name) ->
    Mac = erlwizlight_storage:get_mac(Name),
    erlwizlight_bulb:on(Mac).

off(Name) ->
    Mac = erlwizlight_storage:get_mac(Name),
    erlwizlight_bulb:off(Mac).

dim(Name, Dimming) ->
    Mac = erlwizlight_storage:get_mac(Name),
    erlwizlight_bulb:set_dimming(Mac, Dimming).
