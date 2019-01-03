%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(prop_thinking).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_biggest() ->
    ?FORALL(List, non_empty(list(integer())),
        begin
            thinking:biggest(List) =:= model_biggest(List)
        end).

prop_last() ->
    %% pick a list and a last number
    ?FORALL({List, KnownLast}, {list(number()), number()},
        begin
            KnownList = List ++ [KnownLast], % known number appended to list
            KnownLast =:= lists:last(KnownList) % known last number is found
        end).

prop_sort() ->
    ?FORALL(List, list(term()),
            is_ordered(lists:sort(List))).

%% @doc the sorted and unsorted list should both remain the same size
prop_same_size() ->
    ?FORALL(L, list(number()),
            length(L) =:= length(lists:sort(L))).

%% @doc any element in the sorted list has to have its equivalent in
%% the unsorted list
prop_no_added() ->
    ?FORALL(L, list(number()),
        begin
            Sorted = lists:sort(L),
            lists:all(fun(Element) -> lists:member(Element, L) end, Sorted)
        end).

%% @doc any element in the unsorted list has to have its equivalent in
%% the sorted list
prop_no_removed() ->
    ?FORALL(L, list(number()),
        begin
            Sorted = lists:sort(L),
            lists:all(fun(Element) -> lists:member(Element, Sorted) end, L)
        end).

prop_symmetric() ->
    ?FORALL(Data, list({atom(), any()}),
        begin
            Encoded = encode(Data), is_binary(Encoded) andalso
            Data =:= decode(Encoded)
        end).


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
model_biggest(List) ->
    lists:last(lists:sort(List)).

is_ordered([A,B|T]) ->
    A =< B andalso is_ordered([B|T]);
is_ordered(_) -> % lists with fewer than 2 elements
    true.

%% Take a shortcut by using Erlang primitives
encode(T) -> term_to_binary(T).
decode(T) -> binary_to_term(T).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

%% nothing!
