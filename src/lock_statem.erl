-module(lock_statem).
-behavior(gen_statem).

-export([start_link/0, callback_mode/0, init/1, stop/0]).


start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

callback_mode() -> handle_event_function.

init([]) -> {ok, locked, state}.

stop() ->
    gen_statem:stop(?MODULE).
