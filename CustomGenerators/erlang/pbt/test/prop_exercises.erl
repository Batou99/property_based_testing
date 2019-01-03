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
        {1, {node, Type, tree(Type), undefined}},
        {1, {node, Type, undefined, tree(Type)}},
        {5, {node, Type, tree(Type), tree(Type)}}
    ]).

stamp() -> {hour(), min(), sec()}.
hour() -> choose(0,23).
min()  -> choose(0,59).
sec()  -> choose(0,59).

%% Return hours in the morning
am_stamp1() ->
    ?SUCHTHAT({H,_,_}, stamp(), H < 12).
am_stamp2() ->
    ?LET({H,M,S}, stamp(), {H rem 12, M, S}).

%% Return two ordered timestamps
stamps1() ->
    ?SUCHTHAT({S1, S2}, {stamp(), stamp()}, S1 =< S2).
stamps2() ->
    ?LET({S1, S2}, {stamp(), stamp()}, {min(S1,S2), max(S1,S2)}).

%% Return any time that does not overlap standup meetings
no_standup1() ->
    ?SUCHTHAT({H,M,_}, stamp(), H =/= 9 orelse M > 10).
no_standup2() ->
    ?LET({H,M,S}, stamp(),
         case H of
            9 when M =< 10 -> {8, M, S};
            _ -> {H,M,S}
         end).

%% Wrapper for file opening, to let the abstract format
%% work without the `{ok, Fd}' interface and just pass the
%% file descriptor directly.
%% By returning `Fd' for both file_open and file_write, we
%% ensure generators can recursively call them transparently
%% and fully benefit from their side-effects
file_open(Name, Opts) ->
    {ok, Fd} = file:open(Name, Opts),
    %% ensure the file is refreshed on each test run
    file:truncate(Fd),
    Fd.

file_write(Fd, Data) ->
    ok = file:write(Fd, Data),
    Fd.

