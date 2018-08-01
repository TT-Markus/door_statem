-module(lock_statem).
-behavior(gen_statem).

-export([start_link/0, callback_mode/0, init/1, stop/0, handle_event/4]).

-export([get_state/0, get_count/0, press_digit/1]).


start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

callback_mode() -> handle_event_function.

init([]) -> {ok, locked, #{count => 0}}.

get_state() ->
    gen_statem:call(?MODULE, get_state).

get_count() ->
    gen_statem:call(?MODULE, get_count).

press_digit(Digit) ->
    gen_statem:call(?MODULE, {press_digit, Digit}).

handle_event({call, From}, {press_digit, _Digit}, locked, #{count := Count} = Data) ->
    NewData = Data#{count => Count + 1},
    {next_state, locked, NewData, {reply, From, locked}};
handle_event({call, From}, get_count, State, #{count := Count} = Data) ->
    {next_state, State, Data, {reply, From, Count}};
handle_event({call, From}, get_state, State, Data) ->
    {next_state, State, Data, {reply, From, State}};
handle_event(_EventType, _EventContent, State_Name, State) ->
    {keep_state, State_Name, State}.

stop() ->
    gen_statem:stop(?MODULE).
