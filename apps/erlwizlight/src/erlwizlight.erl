-module(erlwizlight).
-export([discover/0, list/0]).

discover() ->
    erlwizlight_discovery:discover().

list() ->
    erlwizlight_registry:get_all().
