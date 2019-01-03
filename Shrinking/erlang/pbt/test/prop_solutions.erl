%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(prop_solutions).
-include_lib("proper/include/proper.hrl").
-compile(export_all).

prop_too_much_dairy() ->
    ?FORALL(Food, meal(), dairy_count(Food) == 0).

dairy_count(L) ->
    length([X || X <- L, is_dairy(X)]).

is_dairy(cheesesticks) -> true;
is_dairy(lasagna) -> true;
is_dairy(icecream) -> true;
is_dairy(milk) -> true;
is_dairy(_) -> false.

meal() ->
    ?LETSHRINK([Appetizer, Drink, Entree, Desert],
               [[elements([soup, salad, cheesesticks])],
                [elements([coffee, tea, milk, water, juice])],
                [elements([lasagna, tofu, steak])],
                [elements([cake, chocolate, icecream])]],
               Appetizer ++ Drink ++ Entree ++ Desert).

%%%

item_price_special() ->
    %% first LET: PriceList
    ?LET(PriceList, price_list(),
         %% second LET: SpecialList
         ?LET(SpecialList, special_list(PriceList),
              %% third LET: Regular + Special items and prices
              ?LET({Items, Price},
                   %% Help shrinking by first trying only one of the
                   %% two possible lists in case a specific form causes
                   %% the problems on its own
                   ?LETSHRINK([{RegularItems, RegularExpected},
                               {SpecialItems, SpecialExpected}],
                              [regular_gen(PriceList, SpecialList),
                               special_gen(PriceList, SpecialList)],
                              %% And we merge:
                              {RegularItems ++ SpecialItems,
                               RegularExpected + SpecialExpected}),
                    {Items, Price, PriceList, SpecialList}))).

price_list() ->
    ?LET(PriceList,
         non_empty(list({non_empty(string()), pos_integer()})),
         lists:ukeysort(1, PriceList)). % no dupes

%% Generates specials in a list of the form
%% [{Name, Count, SpecialPrice}]
special_list(PriceList) ->
    Items = [Name || {Name, _} <- PriceList],
    ?LET(Specials, list({elements(Items), choose(2,5), integer()}),
         lists:ukeysort(1, Specials)). % no dupes

%% Generates lists of regular items, at a price below the special value.
regular_gen(PriceList, SpecialList) ->
    regular_gen(PriceList, SpecialList, [], 0).

regular_gen([], _, List, Price) ->
    {List, Price};
regular_gen([{Item, Cost}|PriceList], SpecialList, Items, Price) ->
    CountGen = case lists:keyfind(Item, 1, SpecialList) of
        {_, CountReq, _} ->
            %% The item can be on special. Generate fewer than needed
            choose(0, CountReq-1);
        _ ->
            %% Item not on specials. Generate at will
            non_neg_integer()
    end,
    %% Use the conditional generator to generate items
    ?LET(Count, CountGen,
         regular_gen(PriceList, SpecialList,
                     ?LET(V, vector(Count, Item), V ++ Items),
                     Cost * Count + Price)).

special_gen(_, SpecialList) ->
    %% actually do not need the item list
    special_gen(SpecialList, [], 0).

special_gen([], Items, Price) ->
    {Items, Price};
special_gen([{Item, Count, Cost} | SpecialList], Items, Price) ->
    %% Generate sequences of items equal to the special, based on a
    %% multiplier. If we have a need for 3 items for a special, we can
    %% generate 0, 3, 6, 9, ... of such items at once
    ?LET(Multiplier, non_neg_integer(),
         special_gen(SpecialList,
                     ?LET(V, vector(Count * Multiplier, Item), V ++ Items),
                     Cost * Multiplier + Price)).


