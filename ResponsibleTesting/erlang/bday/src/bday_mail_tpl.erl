%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(bday_mail_tpl).
-export([body/1]).
-export([full/1]). % add this export near the top of the file

-spec body(bday_employee:employee()) -> string().
body(Employee) ->
    lists:flatten(io_lib:format("Happy birthday, dear ~s!",
                                [bday_employee:first_name(Employee)])).
-spec full(bday_employee:employee()) -> {[string()], string(), string()}.
full(Employee) ->
    {[bday_employee:email(Employee)],
     "Happy birthday!",
     body(Employee)}.
