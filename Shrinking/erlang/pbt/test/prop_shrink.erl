%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(prop_shrink).
-include_lib("proper/include/proper.hrl").
-compile([export_all, {no_auto_import,[date/0]}, {no_auto_import,[time/0]}]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_too_much_dairy() ->
    ?FORALL(Food, meal(), dairy_count(Food) =:= 0).

dairy_count(L) ->
    length([X || X <- L, is_dairy(X)]).

is_dairy(cheesesticks) -> true;
is_dairy(lasagna) -> true;
is_dairy(icecream) -> true;
is_dairy(milk) -> true;
is_dairy(_) -> false.

meal() ->
    ?LETSHRINK([Appetizer, Drink, Entree, Dessert],
               [elements([soup, salad, cheesesticks]),
                elements([coffee, tea, milk, water, juice]),
                elements([lasagna, tofu, steak]),
                elements([cake, chocolate, icecream])],
               [Appetizer, Drink, Entree, Dessert]).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
strdatetime() ->
    ?LET(DateTime, datetime(), to_str(DateTime)).

datetime() ->
    {date(), time(), timezone()}.

date() ->
    ?SUCHTHAT({Y,M,D}, {year(), month(), day()},
              calendar:valid_date(Y,M,D)).

year() ->
    ?SHRINK(range(0, 9999), [range(1970, 2000), range(1900, 2100)]). %(1)

month() ->
    range(1, 12).

day() ->
    range(1, 31).

time() ->
    {range(0, 24), range(0, 59), range(0, 60)}.

timezone() ->
    {elements(['+', '-']),
     ?SHRINK(range(0, 99), [range(0, 14), 0]),
     ?SHRINK(range(0, 99), [0, 15, 30, 45])}.

%% Helper to convert the internal format to a string
to_str({{Y,M,D}, {H,Mi,S}, {Sign,Ho,Mo}}) ->
    FormatStr = "~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b~s~2..0b:~2..0b",
    lists:flatten(io_lib:format(FormatStr, [Y,M,D,H,Mi,S,Sign,Ho,Mo])).


tree(N) when N =< 1 ->
    {leaf, number()};
tree(N) ->
    PerBranch = N div 2,
    {branch, tree(PerBranch), tree(PerBranch)}.

tree_shrink(N) when N =< 1 ->
    {leaf, number()};
tree_shrink(N) ->
    PerBranch = N div 2,
    ?LETSHRINK([L, R], [tree_shrink(PerBranch), tree_shrink(PerBranch)],
               {branch, L, R}).


