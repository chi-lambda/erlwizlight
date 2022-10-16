%%%-------------------------------------------------------------------
%% @doc erlwizlight public API
%% @end
%%%-------------------------------------------------------------------

-module(erlwizlight_app).

-behaviour(application).

-export([start/2, stop/1]).

-spec start(any(), any()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    {ok, Pid} = erlwizlight_sup:start_link(),
    {ok, Pid}.

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

%% internal functions
