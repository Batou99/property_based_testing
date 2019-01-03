%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(checkout_1).
-export([total/3]).

-type item() :: string().
-type price() :: integer().

-spec total([item()], [{item(), price()}], any()) -> price().
total(ItemList, PriceList, _Specials) ->
    lists:sum([proplists:get_value(Item, PriceList) || Item <- ItemList]).
