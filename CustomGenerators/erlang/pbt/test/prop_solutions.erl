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
-compile([export_all, nowarn_export_all]).

%% The tree generates a data type that represents the following types:
-type tree() :: tree(term()).
-type tree(T) :: {node,
                  Value :: T,
                  Left  :: tree(T) | undefined,
                  Right :: tree(T) | undefined}.

tree() ->
    tree(term()).

tree(Type) ->
    frequency([
        {3, {node, Type, undefined, undefined}},
        {2, {node, Type, ?LAZY(tree(Type)), undefined}},
        {2, {node, Type, undefined, ?LAZY(tree(Type))}},
        {3, {node, Type, ?LAZY(tree(Type)), ?LAZY(tree(Type))}}
    ]).

limited_tree(Type) ->
    ?SIZED(Size, limited_tree(Size, Type)).

limited_tree(Size, Type) when Size =< 1 ->
    {node, Type, undefined, undefined};
limited_tree(Size, Type) ->
    frequency([
        {1, {node, Type, ?LAZY(limited_tree(Size-1, Type)), undefined}},
        {1, {node, Type, undefined, ?LAZY(limited_tree(Size-1, Type))}},
        {5, {node, Type,
             %% Divide to avoid exponential growth
            ?LAZY(limited_tree(Size div 2, Type)),
            ?LAZY(limited_tree(Size div 2, Type))}}
    ]).

prop_finishes() ->
    ?FORALL(T, limited_tree(integer()),
            is_tuple(T)). % can't test for the halting program, just see!

file_open(Name, Opts) ->
    {ok, Fd} = file:open(Name, Opts),
    %% ensure the file is refreshed on each test run
    file:truncate(Fd),
    Fd.

file_write(Fd, Data) ->
    ok = file:write(Fd, Data),
    Fd.

file(Name) ->
    ?SIZED(
       Size,
       lines(Size, {'$call', ?MODULE, file_open, [Name, [read,write,raw]]})
    ).

lines(Size, Fd) ->
    if Size =< 1 -> Fd
     ; Size > 1 -> lines(Size-1, {'$call', ?MODULE, file_write, [Fd,bin()]})
    end.

bin() ->
    non_empty(binary()).
