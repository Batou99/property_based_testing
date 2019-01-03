%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(prop_exercises).
-include_lib("proper/include/proper.hrl").

prop_set_union() ->
    ?FORALL({ListA, ListB}, {list(number()), list(number())},
        begin
            SetA = sets:from_list(ListA),
            SetB = sets:from_list(ListB),
            ModelUnion = lists:sort(ListA ++ ListB),
            lists:sort(sets:to_list(sets:union(SetA, SetB))) =:= ModelUnion
        end).

prop_dict_merge() ->
    ?FORALL({ListA, ListB}, {list({term(), term()}), list({term(), term()})},
        begin
            Merged = dict:merge(fun(_Key, V1, _V2) -> V1 end,
                                dict:from_list(ListA),
                                dict:from_list(ListB)),
            extract_keys(lists:sort(dict:to_list(Merged)))
            ==
            lists:usort(extract_keys(ListA ++ ListB))
        end).

extract_keys(List) -> [K || {K,_} <- List].
