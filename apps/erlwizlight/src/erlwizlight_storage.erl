-module(erlwizlight_storage).

-behavior(gen_server).

-export([init/1, handle_cast/2, handle_call/3, start_link/1]).
-export([get_name/1, set_name/2]).

-include_lib("stdlib/include/ms_transform.hrl").

-record(bulb, {mac :: string(), name :: string()}).

%%% gen_server functions

init(ok) ->
    ok = create_schema(),
    application:start(mnesia),
    ok = create_table(),
    {ok, #{}}.

handle_call(Msg, _From, State) ->
    logger:warning("~p: Unhandled call ~p with state ~p", [?MODULE, Msg, State]),
    {noreply, State}.

handle_cast({set_name, Mac, Name}, State) ->
    {atomic, ok} =
        mnesia:transaction(fun() -> mnesia:write(#bulb{mac = Mac, name = Name}) end),
    {noreply, State};
handle_cast(Msg, State) ->
    logger:warning("~p: Unhandled cast ~p with state ~p", [?MODULE, Msg, State]),
    {noreply, State}.

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ok, Opts).

%%% interface functions

get_name(Mac) ->
    {atomic, Result} =
        mnesia:transaction(fun() ->
                              mnesia:select(bulb,
                                            ets:fun2ms(fun(#bulb{mac = Mac2, name = Name})
                                                          when Mac == Mac2 ->
                                                          Name
                                                       end))
                           end),
    maybe(Result).

set_name(Mac, Name) ->
    gen_server:cast(?MODULE, {set_name, Mac, Name}).

%%% private functions

create_schema() ->
    N = node(),
    case mnesia:create_schema([N]) of
        ok ->
            ok;
        {error, {N, {already_exists, N}}} ->
            ok;
        X ->
            X
    end.

create_table() ->
    case mnesia:create_table(bulb,
                             [{type, ordered_set},
                              {disc_copies, [node()]},
                              {attributes, record_info(fields, bulb)}])
    of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, bulb}} ->
            ok
    end,
    mnesia:wait_for_tables([bulb], infinity).

maybe([H|_]) -> H;
maybe(_) -> "".
