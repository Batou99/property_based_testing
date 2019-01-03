%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(bday).
-export([main/1]).

main([Path]) ->
    {ok, Data} = file:read_file(Path),
    Handle = bday_employee:from_csv(binary_to_list(Data)),
    Query = bday_employee:filter_birthday(Handle, date()), % date = local time
    BdaySet = bday_employee:fetch(Query),
    Mails = [bday_mail_tpl:full(Employee) || Employee <- BdaySet],
    [send_email(To, Topic, Body) || {To, Topic, Body} <- Mails].

send_email(To, _, _) ->
    io:format("sent birthday email to ~p~n", [To]).

