-module(erlwizlight_bulb_sup).

-behaviour(supervisor).

-export([init/1, start_link/0]).
-export([start_child/1]).

-spec init(any()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(_Args) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 5,
          period => 60},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(erlwizlight_bulb:bulb()) -> supervisor:startchild_ret().
start_child(#{mac := Mac} = Bulb) ->
    ChildSpec = #{id => Mac, start => {erlwizlight_bulb, start_link, [Bulb, []]}},
    supervisor:start_child(?MODULE, ChildSpec).
