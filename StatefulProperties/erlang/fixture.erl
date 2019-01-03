%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
prop_example() ->
    ?SETUP(fun() ->
        %% setup phase as any code running within the macro
        OptionalData = do_setup(),
        %% teardown phase as a no-argument function returned
        %% by the setup function
        fun() -> do_teardown(OptionalData) end
    end,
    ?FORALL(property)
).

