-module(erlwizlight_bulb_sup).

-behaviour(supervisor).

-export([init/1, start_link/0]).
-export([start_child/1]).

init(_) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 5,
          period => 60},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(#{mac := Mac} = Bulb) ->
    ChildSpec = #{id => Mac, start => {erlwizlight_bulb, start_link, [Bulb, []]}},
    supervisor:start_child(?MODULE, ChildSpec).
