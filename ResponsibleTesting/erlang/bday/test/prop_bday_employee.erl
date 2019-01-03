%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(prop_bday_employee).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_fix_csv_leading_space() ->
    ?FORALL(Map, raw_employee_map(),
        begin
            Emp = bday_employee:adapt_csv_result(Map),
            Strs = [X || X <- maps:keys(Emp) ++ maps:values(Emp), is_list(X)],
            lists:all(fun(String) -> hd(String) =/= $\s end, Strs)
        end).

prop_fix_csv_date_of_birth() ->
    ?FORALL(Map, raw_employee_map(),
        case bday_employee:adapt_csv_result(Map) of
            #{"date_of_birth" := {Y,M,D}} ->
                is_integer(Y) and is_integer(M) and is_integer(D);
            _ ->
                false
        end).

prop_handle_access() ->
    ?FORALL(Maps, non_empty(list(raw_employee_map())),
        begin
            CSV = bday_csv:encode(Maps),
            Handle = bday_employee:from_csv(CSV),
            Partial = bday_employee:filter_birthday(Handle, date()),
            ListFull = bday_employee:fetch(Handle),
            true = is_list(bday_employee:fetch(Partial)),
            %% Check for no crash
            _ = [{bday_employee:first_name(X),
                  bday_employee:last_name(X),
                  bday_employee:email(X),
                  bday_employee:date_of_birth(X)} || X <- ListFull],
            true
        end).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
raw_employee_map() ->
    %% PropEr's native map type does not allow pre-defined static
    %% keys (just `map(T1, T2)') so we convert from a proplist
    ?LET(PropList,
         [{"last_name", prop_csv:field()}, % 1st col has no leading space
          {" first_name", whitespaced_text()},
          {" date_of_birth", text_date()},
          {" email", whitespaced_text()}],
         maps:from_list(PropList)).

whitespaced_text() ->
    ?LET(Txt, prop_csv:field(), " " ++ Txt).

text_date() ->
    %% leading space and leading 0s for months and days; since we're
    %% checking string formats, it doesn't matter if dates are invalid
    ?LET({Y,M,D}, {choose(1900,2020), choose(1,12), choose(1,31)},
         lists:flatten(io_lib:format(" ~w/~2..0w/~2..0w", [Y,M,D]))).

