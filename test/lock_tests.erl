-module(lock_tests).
-include_lib("eunit/include/eunit.hrl").

start_and_stop_test() ->
  {ok, _Pid} = lock_statem:start_link(),
  lock_statem:stop().

get_state_test() ->
  {ok, _Pid} = lock_statem:start_link(),
  ?assertEqual(locked, lock_statem:get_state()),
  lock_statem:stop().

handle_digit_press_test() ->
  {ok, _Pid} = lock_statem:start_link(),
  ?assertEqual(locked, lock_statem:press_digit("1")),
  ?assertEqual(1, lock_statem:get_count()),
  lock_statem:stop().

unlock_test() ->
  {ok, _Pid} = lock_statem:start_link(),
  ?assertEqual(locked, lock_statem:press_digit("1")),
  ?assertEqual(locked, lock_statem:press_digit("2")),
  ?assertEqual(locked, lock_statem:press_digit("3")),
  ?assertEqual(unlocked, lock_statem:press_digit("4")),
  ?assertEqual(0, lock_statem:get_count()),
  lock_statem:stop().

unlock_timeout_test() ->
  {ok, _Pid} = lock_statem:start_link(),
  ?assertEqual(locked, lock_statem:press_digit("1")),
  ?assertEqual(locked, lock_statem:press_digit("2")),
  ?assertEqual(locked, lock_statem:press_digit("3")),
  ?assertEqual(unlocked, lock_statem:press_digit("4")),
  ?assertEqual(0, lock_statem:get_count()),
  timer:sleep(1001),
  ?assertEqual(locked, lock_statem:get_state()),
  lock_statem:stop().

state_timeout_test() ->
  {ok, _Pid} = lock_statem:start_link(),
  ?assertEqual(locked, lock_statem:press_digit("1")),
  ?assertEqual(locked, lock_statem:press_digit("2")),
  ?assertEqual(locked, lock_statem:press_digit("3")),
  ?assertEqual(unlocked, lock_statem:press_digit("4")),
  ?assertEqual(0, lock_statem:get_count()),
  timer:sleep(500),
  ?assertEqual(locked, lock_statem:lock()),
  ?assertEqual(locked, lock_statem:press_digit("1")),
  ?assertEqual(locked, lock_statem:press_digit("2")),
  ?assertEqual(locked, lock_statem:press_digit("3")),
  ?assertEqual(unlocked, lock_statem:press_digit("4")),
  timer:sleep(500),
  ?assertEqual(unlocked, lock_statem:get_state()),
  timer:sleep(500),
  ?assertEqual(locked, lock_statem:get_state()),
  lock_statem:stop().
