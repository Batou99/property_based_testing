%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(bookstore_db).
-export([load_queries/0, setup/0, teardown/0,
         add_book/3, add_book/5, add_copy/1, borrow_copy/1, return_copy/1,
         find_book_by_author/1, find_book_by_isbn/1, find_book_by_title/1]).

%% @doc Create the database table required for the bookstore
setup() ->
    run_query(setup_table_books, []).

%% @doc Delete the database table required for the bookstore
teardown() ->
    run_query(teardown_table_books, []).

%% @doc Add a new book to the inventory, with no copies of it.
add_book(ISBN, Title, Author) ->
    add_book(ISBN, Title, Author, 0, 0).

%% @doc Add a new book to the inventory, with a pre-set number of owned
%% and available copies.
add_book(ISBN, Title, Author, Owned, Avail) ->
    BinTitle = unicode:characters_to_binary(Title),
    BinAuthor = unicode:characters_to_binary(Author),
    case run_query(add_book, [ISBN, BinTitle, BinAuthor, Owned, Avail]) of
        {{insert,0,1},[]} -> ok;
        {error, Reason} -> {error, Reason};
        Other -> {error, Other}
    end.

%% @doc Add a copy of a book to the bookstore's inventory
add_copy(ISBN) ->
    handle_single_update(run_query(add_copy, [ISBN])).

%% @doc Borrow a copy of a book; reduces the count of available
%% copies by one. Who borrowed the book is not tracked at this
%% moment and is left as an exercise to the reader.
borrow_copy(ISBN) ->
    case find_book_by_isbn(ISBN) of
        {error, Reason} -> {error, Reason};
        {ok, []} -> {error, not_found};
        {ok, _} ->
            case handle_single_update(run_query(borrow_copy, [ISBN])) of
                {error, not_found} -> {error, unavailable}; % rewrite error
                Other -> Other
            end
    end.

%% @doc Return a book copy, making it available again.
return_copy(ISBN) ->
    handle_single_update(run_query(return_copy, [ISBN])).

%% @doc Search all books written by a given author. The matching is loose
%% and so searching for `Hawk' will return copies of books written
%% by `Stephen Hawking' (if such copies are in the system)
find_book_by_author(Author) ->
    handle_select(
        run_query(find_by_author,
                  [unicode:characters_to_binary(["%",Author,"%"])])
    ).

%% @doc Find books under a given ISBN.
find_book_by_isbn(ISBN) ->
    handle_select(run_query(find_by_isbn, [ISBN])).

%% @doc Find books with a given title. The matching us loose and searching
%% for `Test' may return `PropEr Testing'.
find_book_by_title(Title) ->
    handle_select(
        run_query(find_by_title,
                  [unicode:characters_to_binary(["%",Title,"%"])])
    ).

handle_select({{select, _}, List}) -> {ok, List};
handle_select(Error) -> Error.

handle_single_update({{update,1}, _}) -> ok;
handle_single_update({{update,0}, _}) -> {error, not_found};
handle_single_update({error, Reason}) -> {error, Reason};
handle_single_update(Other) -> {error, Other}.

%% @doc Run a query with a given set of arguments. This function
%% automatically wraps the whole operation with a connection to
%% a database.
run_query(Name, Args) ->
    with_connection(fun(Conn) -> run_query(Name, Args, Conn) end).

%% @doc Run a query with a given set of arguments, within the scope
%% of a specific PostgreSQL connection. For example, this allows to run
%% multiple queries within a single connection, or within larger
%% transactions.
run_query(Name, Args, Conn) ->
    pgsql_connection:extended_query(query(Name), Args, Conn).

%% @doc Takes a function, and runs it with a connection to a PostgreSQL
%% database connection as an argument. Closes the connection after
%% the call, and returns its result.
with_connection(Fun) ->
    %% A pool call could be hidden and wrapped here, rather than
    %% always grabbing a new connection
    {ok, Conn} = connect(),
    Res = Fun(Conn),
    close(Conn),
    Res.

%% @doc open up a new connection to a PostgreSQL database from
%% the application configuration
connect() -> connect(application:get_env(bookstore, pg, [])).

%% @doc open up a new connection to a PostgreSQL database with
%% explicit configuration parameters.
connect(Args) ->
    try pgsql_connection:open(Args) of
        {pgsql_connection, _} = Conn -> {ok, Conn}
    catch
        throw:Error -> {error, Error}
    end.

%% @doc end a connection
close(Conn) -> pgsql_connection:close(Conn).


load_queries() ->
    ets:new(bookstore_sql, [named_table, public, {read_concurrency, true}]),
    SQLFile = filename:join(code:priv_dir(bookstore), "queries.sql"),
    {ok, Queries} = eql:compile(SQLFile),
    ets:insert(bookstore_sql, Queries),
    ok.

query(Name) ->
    case ets:lookup(bookstore_sql, Name) of
        [] -> {query_not_found, Name};
        [{_, Query}] -> Query
    end.

