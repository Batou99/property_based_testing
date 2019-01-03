%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(prop_target).
-include_lib("proper/include/proper.hrl").
-compile(export_all).

prop_path(opts) -> [{search_steps, 100}]. % otherwise this runs 1000 times!
prop_path() ->
    ?FORALL_TARGETED(P, path(),
        begin
            {X,Y} = lists:foldl(fun move/2, {0,0}, P),
            io:format("~p",[{X,Y}]),
            ?MAXIMIZE(X-Y),
            true
        end).

path() -> list(oneof([left, right, up, down])).

move(left, {X,Y}) -> {X-1,Y};
move(right, {X,Y}) -> {X+1,Y};
move(up, {X,Y}) -> {X,Y+1};
move(down, {X,Y}) -> {X,Y-1}.

sort([]) -> [];
sort([Pivot|T]) ->
    sort([ X || X <- T, X < Pivot])
    ++ [Pivot] ++
    sort([ X || X <- T, X >= Pivot]).

sort_fixed([]) -> [];
sort_fixed(L) ->
    N = rand:uniform(length(L)),
    Pivot = lists:nth(N, L),
    sort_fixed([X || X <- L, X < Pivot])
    ++ [X || X <- L, X == Pivot] ++
    sort_fixed([X || X <- L, X > Pivot]).

prop_quicksort_time(opts) -> [noshrink].
prop_quicksort_time() ->
    ?FORALL_TARGETED(L, ?SUCHTHAT(L, list(integer()), length(L) < 100000),
        begin
            T0 = erlang:monotonic_time(millisecond),
            sort(L),
            T1 = erlang:monotonic_time(millisecond),
            ?MAXIMIZE(T1-T0),
            T1-T0 < 5000
        end).

prop_quicksort_time_fixed(opts) -> [noshrink].
prop_quicksort_time_fixed() ->
    ?FORALL_TARGETED(L, ?SUCHTHAT(L, list(integer()), length(L) < 100000),
        begin
            T0 = erlang:monotonic_time(millisecond),
            sort_fixed(L),
            T1 = erlang:monotonic_time(millisecond),
            ?MAXIMIZE(T1-T0),
            T1-T0 < 5000
        end).

prop_quicksort_time_regular(opts) -> [{numtests, 1000}].
prop_quicksort_time_regular() ->
    ?FORALL(L, ?SUCHTHAT(L, list(integer()), length(L) < 100000),
        begin
            T0 = erlang:monotonic_time(millisecond),
            sort(L),
            T1 = erlang:monotonic_time(millisecond),
            T1-T0 < 5000
        end).

prop_mergesort_time() ->
    ?FORALL_TARGETED(L, ?SUCHTHAT(L, list(integer()), length(L) < 100000),
        begin
            T0 = erlang:monotonic_time(millisecond),
            lists:sort(L),
            T1 = erlang:monotonic_time(millisecond),
            ?MAXIMIZE(T1-T0),
            T1-T0 < 5000
        end).

prop_tree_regular(opts) -> [{numtests, 1000}].
prop_tree_regular() ->
    ?FORALL(T, tree(),
        begin
            Weight = sides(T),
            io:format(" ~p", [Weight]),
            true
        end).

prop_tree() ->
    ?FORALL_TARGETED(T, tree(),
        begin
            {Left, Right} = Weight = sides(T),
            io:format(" ~p", [Weight]),
            ?MAXIMIZE(Left-Right),
            true
        end).

prop_tree_neighbor() ->
    ?FORALL_TARGETED(T, ?USERNF(tree(), next_tree()),
        begin
            {Left, Right} = Weight = sides(T),
            io:format(" ~p", [Weight]),
            ?MAXIMIZE(Left-Right),
            true
        end).

%% This one takes long because it does 100 rounds for ?FORALL
%% and 1000 rounds for ?NOT_EXISTS; this gives 100,000 executions!
prop_tree_search() ->
    ?FORALL(L, list(integer()),
        ?NOT_EXISTS(T,
            ?USERNF(
                ?LET(X, L, to_tree(X)), % trick to wrap the generator
                next_tree()             % same neighbor function
            ),
            begin
                {Left, Right} = sides(T),
                ?MAXIMIZE(Left-Right),
                false % using `false' for NOT_EXISTS to pass
            end)).

%% Does not work well!
%tree() ->
%    list(oneof([
%        undefined,
%        {leaf, integer()},
%        ?LAZY({node, integer(), tree(), tree()})
%    ])).

tree() ->
    ?LET(L, non_empty(list(integer())), to_tree(L)).

next_tree() ->
    fun(OldTree, {_,T}) ->
        ?LET(N, integer(), insert(trunc(N*T*100), OldTree))
    end.

to_tree(L) ->
    lists:foldl(fun insert/2, undefined, L).

insert(N, {node, N, L, R}) -> {node, N, L, R};
insert(N, {node, M, L, R}) when N < M -> {node, M, insert(N, L), R};
insert(N, {node, M, L, R}) when N > M -> {node, M, L, insert(N, R)};
insert(N, {leaf, N}) -> {leaf, N};
insert(N, {leaf, M}) when N < M -> {node, N, undefined, {leaf, M}};
insert(N, {leaf, M}) when N > M -> {node, N, {leaf, M}, undefined};
insert(N, undefined) -> {leaf, N}.

sides({node, _, Left, Right}) ->
    {LL, LR} = sides(Left),
    {RL, RR} = sides(Right),
    {count_inner(Left)+LL+LR, count_inner(Right)+RL+RR};
sides(_) ->
    {0,0}.

count_inner({node, _, _, _}) -> 1;
count_inner(_) -> 0.
