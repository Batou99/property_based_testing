%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(bookstore_init).
-export([main/1]).

main(_) ->
    %% See: https://www.postgresql.org/docs/9.6/static/server-start.html
    ok = filelib:ensure_dir("postgres/data/.init-here"),
    io:format("initializing database structure...~n"),
    cmd("initdb -D postgres/data"),
    io:format("starting postgres instance...~n"),

    %% On windows this is synchronous and never returns until done
    StartCmd = "pg_ctl -D postgres/data -l logfile start",
    case os:type() of
        {win32, _} -> spawn(fun() -> cmd(StartCmd) end);
        {unix, _} -> cmd(StartCmd)
    end,
    timer:sleep(5000), % wait and pray!

    io:format("setting up 'bookstore_db' database...~n"),
    cmd("psql -h localhost -d template1 -c "
        "\"CREATE DATABASE bookstore_db;\""),
    io:format("OK.~n"),
    init:stop().

cmd(Str) -> io:format("~s~n", [os:cmd(Str)]).
