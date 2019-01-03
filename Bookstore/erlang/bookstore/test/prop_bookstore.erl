%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(prop_bookstore).
-include_lib("proper/include/proper.hrl").
-compile(export_all).

prop_test() ->
    ?SETUP(fun() ->
        {ok, Apps} = application:ensure_all_started(bookstore),
        fun() -> [application:stop(App) || App <- Apps], ok end
      end,
      ?FORALL(Cmds, commands(?MODULE),
        begin
            bookstore_db:setup(),
            {History, State, Result} = run_commands(?MODULE, Cmds),
            bookstore_db:teardown(),
            ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                [History, State, Result]),
                      aggregate(command_names(Cmds), Result =:= ok))
        end)
    ).

prop_parallel() ->
    ?SETUP(fun() ->
        {ok, Apps} = application:ensure_all_started(bookstore),
        fun() -> [application:stop(App) || App <- Apps], ok end
      end,
      ?FORALL(Cmds, parallel_commands(?MODULE),
        begin
            bookstore_db:setup(),
            {History, State, Result} = run_parallel_commands(?MODULE, Cmds),
            bookstore_db:teardown(),
            ?WHENFAIL(io:format("=======~n"
                                "Failing command sequence:~n~p~n"
                                "At state: ~p~n"
                                "=======~n"
                                "Result: ~p~n"
                                "History: ~p~n",
                                [Cmds, State,Result,History]),
                      aggregate(command_names(Cmds), Result =:= ok))
        end)
    ).


%% Initial model value at system start. Should be deterministic.
initial_state() -> #{}.

command(State) ->
    AlwaysPossible = [
        {call, book_shim, add_book_new, [isbn(), title(), author(), 1, 1]},
        {call, book_shim, add_copy_new, [isbn()]},
        {call, book_shim, borrow_copy_unknown, [isbn()]},
        {call, book_shim, return_copy_unknown, [isbn()]},
        {call, book_shim, find_book_by_isbn_unknown, [isbn()]},
        {call, book_shim, find_book_by_author_unknown, [author()]},
        {call, book_shim, find_book_by_title_unknown, [title()]}
    ],
    ReliesOnState = case maps:size(State) of
        0 -> % no values yet
            [];
        _ -> % values form which to work
            S = State,
            [{call, book_shim, add_book_existing,
              [isbn(S), title(), author(), 1, 1]},
             {call, book_shim, add_copy_existing, [isbn(S)]},
             {call, book_shim, borrow_copy_avail, [isbn(S)]},
             {call, book_shim, borrow_copy_unavail, [isbn(S)]},
             {call, book_shim, return_copy_existing, [isbn(S)]},
             {call, book_shim, return_copy_full, [isbn(S)]},
             {call, book_shim, find_book_by_isbn_exists, [isbn(S)]},
             {call, book_shim, find_book_by_author_matching, [author(S)]},
             {call, book_shim, find_book_by_title_matching, [title(S)]}]
    end,
    oneof(AlwaysPossible ++ ReliesOnState).

%% Picks whether a command should be valid under the current state.
%% -- all the unknown calls
precondition(S, {call, _, add_book_new, [ISBN|_]}) ->
    not has_isbn(S, ISBN);
precondition(S, {call, _, add_copy_new, [ISBN]}) ->
    not has_isbn(S, ISBN);
precondition(S, {call, _, borrow_copy_unknown, [ISBN]}) ->
    not has_isbn(S, ISBN);
precondition(S, {call, _, return_copy_unknown, [ISBN]}) ->
    not has_isbn(S, ISBN);
precondition(S, {call, _, find_book_by_isbn_unknown, [ISBN]}) ->
    not has_isbn(S, ISBN);
precondition(S, {call, _, find_book_by_author_unknown, [Author]}) ->
    not like_author(S, Author);
precondition(S, {call, _, find_book_by_title_unknown, [Title]}) ->
    not like_title(S, Title);
precondition(S, {call, _, find_book_by_author_matching, [Author]}) ->
    like_author(S, Author);
precondition(S, {call, _, find_book_by_title_matching, [Title]}) ->
    like_title(S, Title);
precondition(S, {call, _, borrow_copy_avail, [ISBN]}) ->
    0 < element(5, maps:get(ISBN, S, {fake,fake,fake,fake,0}));
precondition(S, {call, _, borrow_copy_unavail, [ISBN]}) ->
    0 =:= element(5, maps:get(ISBN, S, {fake,fake,fake,fake,1}));
precondition(S, {call, _, return_copy_full, [ISBN]}) ->
    {_, _, _, Owned, Avail} = maps:get(ISBN, S, {fake,fake,fake,0,0}),
    Owned =:= Avail andalso Owned =/= 0;
precondition(S, {call, _, return_copy_existing, [ISBN]}) ->
    {_, _, _, Owned, Avail} = maps:get(ISBN, S, {fake,fake,fake,0,0}),
    Owned =/= Avail andalso Owned =/= 0;
%% -- all the calls with known ISBNs
precondition(S, {call, _Mod, _Fun, [ISBN|_]}) ->
    %% to hell with it, we blank match the rest since they're all
    %% constraints on existing ISBNs, no matter the number of arguments
    has_isbn(S, ISBN).

%% Given the state `State' *prior* to the call `{call, Mod, Fun, Args}',
%% determine whether the result `Res' (coming from the actual system)
%% makes sense.
postcondition(_, {_, _, add_book_new, _}, ok) ->
    true;
postcondition(_, {_, _, add_book_existing, _}, {error, _}) ->
    true;
postcondition(_, {_, _, add_copy_existing, _}, ok) ->
    true;
postcondition(_, {_, _, add_copy_new, _}, {error, not_found}) ->
    true;
postcondition(_, {_, _, borrow_copy_avail, _}, ok) ->
    true;
postcondition(_, {_, _, borrow_copy_unavail, _}, {error, unavailable}) ->
    true;
postcondition(_, {_, _, borrow_copy_unknown, _}, {error, not_found}) ->
    true;
postcondition(_, {_, _, return_copy_full, _}, {error, _}) ->
    true;
postcondition(_, {_, _, return_copy_existing, _}, ok) ->
    true;
postcondition(_, {_, _, return_copy_unknown, _}, {error, not_found}) ->
    true;
postcondition(S, {_, _, find_book_by_isbn_exists, [ISBN]}, {ok, [Res]}) ->
    book_equal(Res, maps:get(ISBN, S, undefined));
postcondition(_, {_, _, find_book_by_isbn_unknown, _}, {ok, []}) ->
    true;
postcondition(S, {_, _, find_book_by_author_matching, [Auth]}, {ok,Res}) ->
    Map = maps:filter(fun(_, {_,_,A,_,_}) ->
                              nomatch =/= string:find(A, Auth)
                      end, S),
    books_equal(lists:sort(Res), lists:sort(maps:values(Map)));
postcondition(_, {_, _, find_book_by_author_unknown, _}, {ok, []}) ->
    true;
postcondition(S, {_, _, find_book_by_title_matching, [Title]}, {ok,Res}) ->
    Map = maps:filter(fun(_, {_,T,_,_,_}) ->
                             nomatch =/= string:find(T, Title)
                      end, S),
    books_equal(lists:sort(Res), lists:sort(maps:values(Map)));
postcondition(_, {_, _, find_book_by_title_unknown, _}, {ok, []}) ->
    true;
postcondition(_State, {call, _Mod, _Fun, _Args}, _Res) ->
    io:format("~nnon-matching postcondition: {~p,~p,~p} -> ~p~n",
              [_Mod, _Fun, _Args, _Res]),
    false.

%% Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.
next_state(State, _, {call, _, add_book_new,
                      [ISBN, Title, Author, Owned, Avail]}) ->
    State#{ISBN => {ISBN, Title, Author, Owned, Avail}};
next_state(State, _, {call, _, add_copy_existing, [ISBN]}) ->
    #{ISBN := {ISBN, Title, Author, Owned, Avail}} = State,
    State#{ISBN => {ISBN, Title, Author, Owned+1, Avail+1}};
next_state(State, _, {call, _, borrow_copy_avail, [ISBN]}) ->
    #{ISBN := {ISBN, Title, Author, Owned, Avail}} = State,
    State#{ISBN => {ISBN, Title, Author, Owned, Avail-1}};
next_state(State, _, {call, _, return_copy_existing, [ISBN]}) ->
    #{ISBN := {ISBN, Title, Author, Owned, Avail}} = State,
    State#{ISBN => {ISBN, Title, Author, Owned, Avail+1}};
next_state(State, _Res, {call, _Mod, _Fun, _Args}) ->
    NewState = State,
    NewState.

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
has_isbn(Map, ISBN) ->
    maps:is_key(ISBN, Map).

like_author(Map, Auth) ->
    lists:any(fun({_,_,A,_,_}) -> nomatch =/= string:find(A, Auth) end,
              maps:values(Map)).

like_title(Map, Title) ->
    lists:any(fun({_,T,_,_,_}) -> nomatch =/= string:find(T, Title) end,
              maps:values(Map)).

books_equal([], []) ->
    true;
books_equal([A|As], [B|Bs]) ->
    book_equal(A, B) andalso books_equal(As, Bs);
books_equal(_, _) ->
    false.

book_equal({ISBNA, TitleA, AuthorA, OwnedA, AvailA},
           {ISBNB, TitleB, AuthorB, OwnedB, AvailB}) ->
    {ISBNA, OwnedA, AvailA} =:= {ISBNB, OwnedB, AvailB}
    andalso
    string:equal(TitleA, TitleB) andalso string:equal(AuthorA, AuthorB).


%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
title() -> friendly_unicode().

author() -> friendly_unicode().

friendly_unicode() ->
    ?LET(X, ?SUCHTHAT(S, string(),
                      not lists:member(0, S) andalso
                      nomatch =:= string:find(S, "\\") andalso
                      nomatch =:= string:find(S, "_") andalso
                      nomatch =:= string:find(S, "%") andalso
                      string:length(S) < 256),
         elements([X, unicode:characters_to_binary(X)])).

isbn() ->
    ?LET(ISBN,
         [oneof(["978","979"]),
          ?LET(X, range(0,9999), integer_to_list(X)),
          ?LET(X, range(0,9999), integer_to_list(X)),
          ?LET(X, range(0,999), integer_to_list(X)),
          frequency([{10, range($0,$9)}, {1, "X"}])],
         iolist_to_binary(lists:join("-", ISBN))).

isbn(State) ->
    elements(maps:keys(State)).

author(State) ->
    elements([partial(Author) || {_,_,Author,_,_} <- maps:values(State)]).

title(State) ->
    elements([partial(Title) || {_,Title,_,_,_} <- maps:values(State)]).

%% Create a partial string, built from a portion of a complete one.
partial(String) ->
    L = string:length(String),
    ?LET({Start, Len}, {range(0, L), non_neg_integer()},
         string:slice(String, Start, Len)).
