%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(break_shim).
-export([success/0, err/1, ignored_error/1, timeout/0,
         manual_block/0, manual_deblock/0, manual_reset/0]).

-define(SERVICE, test_service).

success() ->
    circuit_breaker:call(
      ?SERVICE,
      fun() -> success end, timer:hours(1),
      fun() -> true end, timer:hours(1),
      options()
    ).

err(Reason) ->
    circuit_breaker:call(
      ?SERVICE,
      fun() -> {error, Reason} end, timer:hours(1),
      fun() -> true end, timer:hours(1),
      options()
    ).

ignored_error(Reason) -> err(Reason). % same call

timeout() ->
    circuit_breaker:call(
      ?SERVICE,
      fun() -> timer:sleep(infinity) end, 0, %(1)
      fun() -> true end, timer:hours(1),
      options()
    ).

manual_block() -> circuit_breaker:block(?SERVICE).
manual_deblock() -> circuit_breaker:deblock(?SERVICE).
manual_reset() -> circuit_breaker:clear(?SERVICE).

options() ->
    [{n_error, 3},
     {time_error, timer:minutes(30)},
     {n_timeout, 3},
     {time_timeout, timer:minutes(30)},
     {n_call_timeout, 3},
     {time_call_timeout, timer:minutes(30)},
     {ignore_errors, [ignore1, ignore2]}].
