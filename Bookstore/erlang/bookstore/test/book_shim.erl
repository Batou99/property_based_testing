%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(book_shim).
-compile(export_all).

add_book_existing(ISBN, Title, Author, Owned, Avail) ->
    bookstore_db:add_book(ISBN, Title, Author, Owned, Avail).
add_book_new(ISBN, Title, Author, Owned, Avail) ->
    bookstore_db:add_book(ISBN, Title, Author, Owned, Avail).

add_copy_existing(ISBN) -> bookstore_db:add_copy(ISBN).
add_copy_new(ISBN) -> bookstore_db:add_copy(ISBN).

borrow_copy_avail(ISBN) -> bookstore_db:borrow_copy(ISBN).
borrow_copy_unavail(ISBN) -> bookstore_db:borrow_copy(ISBN).
borrow_copy_unknown(ISBN) -> bookstore_db:borrow_copy(ISBN).

return_copy_full(ISBN) -> bookstore_db:return_copy(ISBN).
return_copy_existing(ISBN) -> bookstore_db:return_copy(ISBN).
return_copy_unknown(ISBN) -> bookstore_db:return_copy(ISBN).

find_book_by_isbn_exists(ISBN) -> bookstore_db:find_book_by_isbn(ISBN).
find_book_by_isbn_unknown(ISBN) -> bookstore_db:find_book_by_isbn(ISBN).

find_book_by_author_matching(Author) ->
    bookstore_db:find_book_by_author(Author).
find_book_by_author_unknown(Author) ->
    bookstore_db:find_book_by_author(Author).

find_book_by_title_matching(Title) ->
    bookstore_db:find_book_by_title(Title).
find_book_by_title_unknown(Title) ->
    bookstore_db:find_book_by_title(Title).
