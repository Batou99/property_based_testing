%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(prop_demo).
-include_lib("proper/include/proper.hrl").
%% NOT auto-exported by PropEr, we must do it ourselves.
%% Alternatively use -compile(export_all).
-export([prop_demo/1]).

prop_demo(doc) ->
    %% Docs are shown when the test property fails
    "only properties that return `true' are seen as passing";
prop_demo(opts) ->
    %% Override CLI and rebar.config option for `numtests' only
    [{numtests, 500}].

prop_demo() -> % auto-exported by Proper
    ?FORALL(_N, integer(), false). % always fail
