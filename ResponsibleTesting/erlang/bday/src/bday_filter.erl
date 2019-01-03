%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(bday_filter).
-export([birthday/2]).

birthday(People, {Year, 2, 28}) ->
    case calendar:is_leap_year(Year) of
        true -> filter_dob(People, 2, 28);
        false -> filter_dob(People, 2, 28) ++ filter_dob(People, 2, 29)
    end;
birthday(People, {_Year, Month, Day}) ->
    filter_dob(People, Month, Day).

filter_dob(People, Month, Day) ->
    lists:filter(
      fun(#{"date_of_birth" := {_,M,D}}) -> {Month,Day} == {M,D} end,
      People
    ).

