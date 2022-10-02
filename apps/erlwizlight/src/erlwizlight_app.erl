%%%-------------------------------------------------------------------
%% @doc erlwizlight public API
%% @end
%%%-------------------------------------------------------------------

-module(erlwizlight_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlwizlight_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
