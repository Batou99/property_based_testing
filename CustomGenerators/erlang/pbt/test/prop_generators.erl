%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(prop_generators).
-include_lib("proper/include/proper.hrl").
-compile(export_all).

prop_dupes() ->
    ?FORALL(KV, list({key(), val()}),
      begin
        M = maps:from_list(KV),
        [maps:get(K, M) || {K, _V} <- KV], % crash if K's not in map
        collect(
            {dupes, to_range(5, length(KV) - length(lists:ukeysort(1,KV)))},
            true
        )
      end).

key() -> oneof([range(1,10), integer()]).
val() -> term().

prop_collect1() ->
    ?FORALL(Bin, binary(), collect(byte_size(Bin), is_binary(Bin))).

prop_collect2() ->
    ?FORALL(Bin, binary(),
            collect(to_range(10, byte_size(Bin)), is_binary(Bin))).

to_range(M, N) ->
    Base = N div M,
    {Base*M, (Base+1)*M}.

prop_aggregate() ->
    Suits = [club, diamond, heart, spade],
    ?FORALL(Hand, vector(5, {oneof(Suits), choose(1,13)}),
            aggregate(Hand, true)). % `true' makes it always pass

prop_escape() ->
    ?FORALL(Str, string(),
            aggregate(classes(Str), escape(Str))).

escape(_) -> true. % we don't care about this for this example

classes(Str) ->
    L = letters(Str),
    N = numbers(Str),
    P = punctuation(Str),
    O = length(Str) - (L+N+P),
    [{letters, to_range(5,L)}, {numbers, to_range(5,N)},
     {punctuation, to_range(5,P)}, {others, to_range(5,O)}].

letters(Str) ->
    length([1 || Char <- Str,
                 (Char >= $A andalso Char =< $Z) orelse
                 (Char >= $a andalso Char =< $z)]).

numbers(Str) ->
    length([1 || Char <- Str, Char >= $0, Char =< $9]).

punctuation(Str) ->
    length([1 || Char <- Str, lists:member(Char, ".,;:'\"-")]).

prop_resize() ->
    ?FORALL(Bin, resize(150, binary()),  % <= resized here
            collect(to_range(10, byte_size(Bin)), is_binary(Bin))).

prop_profile1() ->
    ?FORALL(Profile, [{name, resize(10, string())},
                      {age, pos_integer()},
                      {bio, resize(350, string())}],
        begin
            NameLen = to_range(10,length(proplists:get_value(name, Profile))),
            BioLen = to_range(300,length(proplists:get_value(bio, Profile))),
            aggregate([{name, NameLen}, {bio, BioLen}], true)
        end).

prop_profile2() ->
    ?FORALL(Profile, [{name, string()},
                      {age, pos_integer()},
                      {bio, ?SIZED(Size, resize(Size*35, string()))}],
        begin
            NameLen = to_range(10,length(proplists:get_value(name, Profile))),
            BioLen = to_range(300,length(proplists:get_value(bio, Profile))),
            aggregate([{name, NameLen}, {bio, BioLen}], true)
        end).

prop_queue_naive() ->
    ?FORALL(List, list({term(), term()}),
        begin
            Queue = queue:from_list(List),
            queue:is_queue(Queue)
        end).

prop_queue_nicer() ->
    ?FORALL(Q, queue(),
            queue:is_queue(Q)).

queue() ->
    ?LET(List, list({term(), term()}),
         queue:from_list(List)).

text_like() ->
    list(frequency([{80, range($a, $z)},              % letters
                    {10, $\s},                        % whitespace
                    {1,  $\n},                        % linebreak
                    {1, oneof([$., $-, $!, $?, $,])}, % punctuation
                    {1, range($0, $9)}                % numbers
                   ])).

mostly_sorted() ->
    ?LET(Lists,
         list(frequency([
            {5, sorted_list()},
            {1, list()}
         ])),
         lists:append(Lists)).

sorted_list() ->
    ?LET(L, list(), lists:sort(L)).

path() ->
    ?SIZED(Size,
           %   Max, Current, Acc,  VisitedMap     ToIgnore
           path(Size, {0,0}, [], #{{0,0} => seen}, [])).

path(0, _Current, Acc, _Seen, _Ignore) -> % directions limit
    Acc; % max depth reached
path(_Max, _Current, Acc, _Seen, [_,_,_,_]) -> % all directions tried
    Acc; % we give up
path(Max, Current, Acc, Seen, Ignore) ->
    increase_path(Max, Current, Acc, Seen, Ignore).

increase_path(Max, Current, Acc, Seen, Ignore) ->
    DirectionGen = oneof([left, right, up, down] -- Ignore),
    ?LET(Direction, DirectionGen,
      begin
        NewPos = move(Direction, Current),
        case Seen of
            #{NewPos := _} -> % exists
                path(Max, Current, Acc, Seen, [Direction|Ignore]); % retry
            _ ->
                path(Max-1, NewPos, [Direction|Acc],
                     Seen#{NewPos => seen}, [])
        end
      end).

move(left, {X,Y}) -> {X-1,Y};
move(right, {X,Y}) -> {X+1,Y};
move(up, {X,Y}) -> {X,Y+1};
move(down, {X,Y}) -> {X,Y-1}.

dict_gen() ->
    ?LET(X, list({integer(),integer()}), dict:from_list(X)).

dict_symb() ->
    ?SIZED(Size, dict_symb(Size, {call, dict, new, []})).

dict_symb(0, Dict) ->
    Dict;
dict_symb(N, Dict) ->
    dict_symb(N-1, {call, dict, store, [integer(), integer(), Dict]}).

dict_autosymb() ->
    ?SIZED(Size, dict_autosymb(Size, {'$call', dict, new, []})).

dict_autosymb(0, Dict) ->
    Dict;
dict_autosymb(N, Dict) ->
    dict_autosymb(N-1, {'$call', dict, store, [integer(), integer(), Dict]}).

prop_dict_gen() ->
    ?FORALL(D, dict_gen(), dict:size(D) < 5).

prop_dict_symb() ->
    ?FORALL(DSymb, dict_symb(), dict:size(eval(DSymb)) < 5).

prop_dict_autosymb() ->
    ?FORALL(D, dict_autosymb(), dict:size(D) < 5).
