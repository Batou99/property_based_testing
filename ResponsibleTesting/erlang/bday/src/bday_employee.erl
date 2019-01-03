%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(bday_employee).

%% CSV exports
-export([from_csv/1]).
%% Regular usage exports
-export([filter_birthday/2, fetch/1,
         last_name/1, first_name/1, date_of_birth/1, email/1]).

-ifdef(TEST).
-export([adapt_csv_result/1]).
-endif.

-opaque employee() :: #{string() := term()}.
-opaque handle() :: {raw, [employee()]}.
-export_type([handle/0, employee/0]).

-spec from_csv(string()) -> handle().
from_csv(String) ->
    {raw, [adapt_csv_result(Map) || Map <- bday_csv:decode(String)]}.

-spec fetch(handle()) -> [employee()].
fetch({raw, Maps}) -> Maps.

-spec last_name(employee()) -> string() | undefined.
last_name(#{"last_name" := Name}) -> Name.

-spec first_name(employee()) -> string() | undefined.
first_name(#{"first_name" := Name}) -> Name.

-spec date_of_birth(employee()) -> calendar:date().
date_of_birth(#{"date_of_birth" := DoB}) -> DoB.

-spec email(employee()) -> string().
email(#{"email" := Email}) -> Email.

-spec filter_birthday(handle(), calendar:date()) -> handle().
filter_birthday({raw, Employees}, Date) ->
    {raw, bday_filter:birthday(Employees, Date)}.

-spec adapt_csv_result(map()) -> employee().
adapt_csv_result(Map) ->
    NewMap = maps:fold(
        fun(K,V,NewMap) -> NewMap#{trim(K) => maybe_null(trim(V))} end,
        #{},
        Map
    ),
    DoB = maps:get("date_of_birth", NewMap), % crash if key missing
    NewMap#{"date_of_birth" => parse_date(DoB)}.

trim(Str) -> string:trim(Str, leading, " ").

maybe_null("") -> undefined;
maybe_null(Str) -> Str.

parse_date(Str) ->
    [Y,M,D] = [list_to_integer(X) || X <- string:lexemes(Str, "/")],
    {Y,M,D}.
