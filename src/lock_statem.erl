-module(lock_statem).
-behavior(gen_statem).

-export([start_link/0, callback_mode/0, init/1, stop/0, handle_event/4]).

-export([get_state/0, get_count/0, press_digit/1, lock/0]).

-define(EXPECTED_CODE, "1234").

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

callback_mode() -> handle_event_function.

init([]) -> {ok, locked, #{digits => [], count => 0}}.

lock() ->
    gen_statem:call(?MODULE, lock).

get_state() ->
    gen_statem:call(?MODULE, get_state).

get_count() ->
    gen_statem:call(?MODULE, get_count).

press_digit(Digit) ->
    gen_statem:call(?MODULE, {press_digit, Digit}).

handle_event({call, From}, lock, _State, Data) ->
    {next_state, locked, Data, {reply, From, locked}};
handle_event({call, From}, {press_digit, Digit}, locked, #{digits := Digits, count := Count} = Data) ->
    Entered_Code = Digits ++ Digit,
    case Entered_Code =:= ?EXPECTED_CODE of
        true ->
            {next_state, unlocked, #{digits => [], count => 0},
                [{reply, From, unlocked}, {state_timeout, 1000, lock_after_second}]};
        false ->
            NewData = Data#{digits => Entered_Code, count => Count + 1},
            {next_state, locked, NewData, {reply, From, locked}}
    end;
handle_event({call, From}, get_count, State, #{count := Count} = Data) ->
    {next_state, State, Data, {reply, From, Count}};
handle_event({call, From}, get_state, State, Data) ->
    {next_state, State, Data, {reply, From, State}};
handle_event(state_timeout, lock_after_second, unlocked, _Data) ->
    {next_state, locked, #{digits => [], count => 0}};
handle_event(_EventType, _EventContent, State_Name, State) ->
    {keep_state, State_Name, State}.

stop() ->
    gen_statem:stop(?MODULE).
