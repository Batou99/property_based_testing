%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(prop_bday_mail_tpl).
-include_lib("proper/include/proper.hrl").

prop_template_email() ->
    ?FORALL(Employee, employee_map(),
            nomatch =/= string:find(bday_mail_tpl:body(Employee),
                                    maps:get("first_name", Employee))
    ).

employee_map() ->
    %% Convert from a proplist to have specific keys in a map
    ?LET(PropList,
         [{"last_name", non_empty(prop_csv:field())},
          {"first_name", non_empty(prop_csv:field())},
          {"date_of_birth", {choose(1900,2020),choose(1,12),choose(1,31)}},
          {"email", non_empty(prop_csv:field())}],
         maps:from_list(PropList)).
