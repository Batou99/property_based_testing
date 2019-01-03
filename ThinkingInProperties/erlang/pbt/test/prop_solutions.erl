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

%% @doc this function tests that any lists of `{Key,Val}' pairs
%% end up being able to be sorted by the key by using `lists:keysort/2'.
prop_keysort1() ->
    ?FORALL(List, list({term(),term()}),
        begin
            %% is_key_ordered checks that all tuples' keys are ordered.
            is_key_ordered(lists:keysort(1, List))
        end).

is_key_ordered([{A,_},{B,_}=BTuple|T]) ->
    A =< B andalso is_key_ordered([BTuple|T]);
is_key_ordered(_) -> % smaller lists
    true.


%% @doc This function instead works by using random tuples with various
%% sizes, and picking a random key to test it.
%% This tests broader usages of lists:keysort, such as
%% `lists:keysort(2, [{a,b},{e,f,g},{t,a,n,e}])' yielding the list
%% `[{t,a,n,e},{a,b},{e,f,g}]', where the comparison takes place
%% on the second element of each tuple.
%%
%% While more complete than the previous one, this function
%% does not accurately portray the need for stability in the
%% function as documented: `[{a,b}, {a,a}]' being sorted will
%% not be tested here!
%% Those can either be added in a regular test case, or would
%% require devising a different property.
prop_keysort2() ->
    ?FORALL(List, non_empty(list(non_empty(list()))),
        begin
            %% Since the default built-in types do not let us easily
            %% create random-sized tuples that do not include `{}',
            %% which is not working with `lists:keysort/2', we
            %% create variable-sized tuples ourselves.
            Tuples = [list_to_tuple(L) || L <- List],
            %% To know what position to use, we're going to use
            %% the smallest, to avoid errors
            Pos = lists:min([tuple_size(T) || T <- Tuples]),
            Sorted = lists:keysort(Pos, Tuples),
            Keys = extract_keys(Pos, Sorted),
            %% The keys returned by keysort have to be in the
            %% same order as returned by `lists:sort/1', which
            %% we now trust.
            Keys == lists:sort(Keys)
        end).

extract_keys(Pos, List) -> [element(Pos,T) || T <- List].

word_count(String) ->
    Stripped = string:trim(dedupe_spaces(String), both, " "),
    Spaces = lists:sum([1 || Char <- Stripped, Char =:= $\s]),
    case Stripped of
        "" -> 0;
        _ -> Spaces + 1
    end.

dedupe_spaces([]) -> [];
dedupe_spaces([$\s,$\s|Rest]) -> dedupe_spaces([$\s|Rest]);
dedupe_spaces([H|T]) -> [H|dedupe_spaces(T)].

prop_word_count() ->
    ?FORALL(String, non_empty(string()),
            word_count(String) =:= alt_word_count(String)
    ).

alt_word_count(String) -> space(String).

space([]) -> 0;
space([$\s|Str]) -> space(Str);
space(Str) -> word(Str).

word([]) -> 1;
word([$\s|Str]) -> 1+space(Str);
word([_|Str]) -> word(Str).
