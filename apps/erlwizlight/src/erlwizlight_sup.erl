%%%-------------------------------------------------------------------
%% @doc erlwizlight top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlwizlight_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 5,
          period => 60},
    ChildSpecs =
        [#{id => erlwizlight_registry, start => {erlwizlight_registry, start_link, [[]]}},
         #{id => erlwizlight_storage, start => {erlwizlight_storage, start_link, [[]]}},
         #{id => erlwizlight_discovery, start => {erlwizlight_discovery, start_link, [[]]}},
         #{id => erlwizlight_bulb_sup,
           start => {erlwizlight_bulb_sup, start_link, []},
           type => supervisor}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
