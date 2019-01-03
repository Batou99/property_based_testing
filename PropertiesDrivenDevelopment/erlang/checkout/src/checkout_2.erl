%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(checkout_2).
-export([total/3]).

-type item() :: string().
-type price() :: integer().
-type special() :: {item(), pos_integer(), price()}.

-spec total([item()], [{item(), price()}], [special()]) -> price().
total(ItemList, PriceList, Specials) ->
    Counts = count_seen(ItemList),
    {CountsLeft, Prices} = apply_specials(Counts, Specials),
    Prices + apply_regular(CountsLeft, PriceList).

-spec count_seen([item()]) -> [{item(), pos_integer()}].
count_seen(ItemList) ->
    Count = fun(X) -> X+1 end,
    maps:to_list(
      lists:foldl(fun(Item, M) -> maps:update_with(Item, Count, 1, M) end,
                  maps:new(), ItemList)
    ).

-spec apply_specials([{item(), pos_integer()}], [special()]) ->
    {[{item(), pos_integer()}], price()}.
apply_specials(Items, Specials) ->
    lists:mapfoldl(fun({Name, Count}, Price) ->
        case lists:keyfind(Name, 1, Specials) of
            false -> % not found
                {{Name, Count}, Price}; %(1)
            {_, Needed, Value} ->
                {{Name, Count rem Needed}, %(2)
                 Value * (Count div Needed) + Price} %(3)
        end
    end, 0, Items).

-spec apply_regular([{item(), integer()}], [{item(), price()}]) -> price().
apply_regular(Items, PriceList) ->
    lists:sum([Count * proplists:get_value(Name, PriceList)
               || {Name, Count} <- Items]).
