%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(prop_checkout).
-include_lib("proper/include/proper.hrl").
-compile(export_all).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_no_special1() ->
    ?FORALL({ItemList, ExpectedPrice, PriceList}, item_price_list(),
            ExpectedPrice =:= checkout:total(ItemList, PriceList, [])).

prop_no_special2() ->
    ?FORALL({ItemList, ExpectedPrice, PriceList}, item_price_list(),
            collect(
              bucket(length(ItemList), 10),
              ExpectedPrice =:= checkout:total(ItemList, PriceList, [])
            )).

prop_special() ->
    ?FORALL({ItemList, ExpectedPrice, PriceList, SpecialList},
            item_price_special(),
        ExpectedPrice =:= checkout:total(ItemList, PriceList, SpecialList)).

prop_expected_result() ->
    ?FORALL({ItemList, PriceList, SpecialList}, lax_lists(),
        try checkout:total(ItemList, PriceList, SpecialList) of
            N when is_integer(N) -> true
        catch
            error:{unknown_item, _} -> true;
            error:invalid_price_list -> true;
            error:invalid_special_list -> true;
            _:_ -> false
        end).

prop_dupe_list_invalid() ->
    ?FORALL(PriceList, dupe_list(),
        false =:= checkout:valid_price_list(PriceList)).

prop_dupe_specials_invalid() ->
    ?FORALL(SpecialList, dupe_special_list(),
        false =:= checkout:valid_special_list(SpecialList)).


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
bucket(N, Unit) ->
    (N div Unit) * Unit.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
item_price_list() ->
    ?LET(PriceList, price_list(),
         ?LET({ItemList, ExpectedPrice}, item_list(PriceList),
              {ItemList, ExpectedPrice, PriceList})).

%% generate a list of {ItemName, Price} to configure the checkout
price_list() ->
    ?LET(PriceList, non_empty(list({non_empty(string()), integer()})),
         lists:ukeysort(1, PriceList)). % remove duplicates

%% set up recursive generator for the purchased item list along with
%% its expected price, based off the price list.
item_list(PriceList) ->
    ?SIZED(Size, item_list(Size, PriceList, {[], 0})).

item_list(0, _, Acc) -> Acc;
item_list(N, PriceList, {ItemAcc, PriceAcc}) ->
    ?LET({Item, Price}, elements(PriceList),
         item_list(N-1, PriceList, {[Item|ItemAcc], Price+PriceAcc})).

item_price_special() ->
    %% first LET: freeze the PriceList
    ?LET(PriceList, price_list(),
        %% second LET: freeze the SpecialList
        ?LET(SpecialList, special_list(PriceList),
            %% third LET: Regular + Special items and prices
              ?LET({{RegularItems, RegularExpected},
                    {SpecialItems, SpecialExpected}},
                   {regular_gen(PriceList, SpecialList),
                    special_gen(PriceList, SpecialList)},
                   %% And merge + return initial lists:
                   {shuffle(RegularItems ++ SpecialItems),
                    RegularExpected + SpecialExpected,
                    PriceList, SpecialList}))).

shuffle(L) ->
    %% Shuffle the list by adding a random number first,
    %% then sorting on it, and then removing it
    Shuffled = lists:sort([{rand:uniform(), X} || X <- L]),
    [X || {_, X} <- Shuffled].

%% Generates specials in a list of the form
%% [{Name, Count, SpecialPrice}]
special_list(PriceList) ->
    Items = [Name || {Name, _} <- PriceList],
    ?LET(Specials, list({elements(Items), choose(2,5), integer()}),
         lists:ukeysort(1, Specials)). % no dupes

%% Generates lists of regular items, at a price below the special value.
regular_gen(PriceList, SpecialList) -> %(1)
    regular_gen(PriceList, SpecialList, [], 0).

regular_gen([], _, Items, Price) -> %(2)
    {Items, Price};
regular_gen([{Item, Cost}|PriceList], SpecialList, Items, Price) ->
    CountGen = case lists:keyfind(Item, 1, SpecialList) of %(3)
        {_, Limit, _} -> choose(0, Limit-1); % has special; set max amount
        _ -> non_neg_integer()               % no special; generate at will
    end,
    %% Use the conditional generator to generate items
    ?LET(Count, CountGen, %(4)
         regular_gen(PriceList, SpecialList,
                     ?LET(V, vector(Count, Item), V ++ Items), %(5)
                     Cost*Count + Price)). %(6)

special_gen(_, SpecialList) ->
    %% actually do not need the item list
    special_gen(SpecialList, [], 0).

special_gen([], Items, Price) ->
    {Items, Price};
special_gen([{Item, Count, Cost} | SpecialList], Items, Price) ->
    %% Generate sequences of items equal to the special, based on a
    %% multiplier. If we have a need for 3 items for a special, we can
    %% generate 0, 3, 6, 9, ... of such items at once
    ?LET(Multiplier, non_neg_integer(), %(7)
         special_gen(SpecialList,
                     ?LET(V, vector(Count * Multiplier, Item), V ++ Items),
                     Cost * Multiplier + Price)).

lax_lists() ->
    KnownItems = ["A", "B", "C"],
    MaybeKnownItemGen = elements(KnownItems ++ [string()]),
    {list(MaybeKnownItemGen),                          % item list
     list({MaybeKnownItemGen, integer()}),             % price list
     list({MaybeKnownItemGen, integer(), integer()})}. % specials list

dupe_list() ->
    ?LET(Items, non_empty(list(string())),
         vector(length(Items)+1, {elements(Items), integer()})).

dupe_special_list() ->
    ?LET(Items, non_empty(list(string())),
         vector(length(Items)+1, {elements(Items), integer(), integer()})).
