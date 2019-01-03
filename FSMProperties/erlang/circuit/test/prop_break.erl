%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(prop_break).
-include_lib("proper/include/proper.hrl").
-export([initial_state/0, initial_state_data/0, weight/3,
         unregistered/1, ok/1, tripped/1, blocked/1, % State generators
         precondition/4, postcondition/5, next_state_data/5]).
prop_test() ->
    ?FORALL(Cmds, proper_fsm:commands(?MODULE),
        begin
            {ok, Pid} = circuit_breaker:start_link(),
            {History,State,Result} = proper_fsm:run_commands(?MODULE, Cmds),
            gen_server:stop(Pid, normal, 5000), %(1)
            ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                [History,State,Result]),
                      aggregate(zip(proper_fsm:state_names(History),
                                    command_names(Cmds)), 
                                Result =:= ok))
        end).

-record(data, {
          limit = 3 :: pos_integer(),
          errors = 0 :: pos_integer(),
          timeouts = 0 :: pos_integer()
        }).

%% Initial state for the state machine
initial_state() -> unregistered.
%% Initial model data at the start. Should be deterministic.
initial_state_data() -> #data{}.

%% State commands generation
unregistered(_Data) ->
    [{ok, {call, break_shim, success, []}}].

ok(_Data) ->
    [{history, {call, break_shim, success, []}},
     {history, {call, break_shim, err, [valid_error()]}},
     {tripped, {call, break_shim, err, [valid_error()]}},
     {history, {call, break_shim, ignored_error, [ignored_error()]}},
     {history, {call, break_shim, timeout, []}},
     {tripped, {call, break_shim, timeout, []}},
     {blocked, {call, break_shim, manual_block, []}},
     {ok,      {call, break_shim, manual_deblock, []}},
     {ok,      {call, break_shim, manual_reset, []}}].

tripped(_Data) ->
    [{history, {call, break_shim, success, []}},
     {history, {call, break_shim, err, [valid_error()]}},
     {history, {call, break_shim, ignored_error, [ignored_error()]}},
     {history, {call, break_shim, timeout, []}},
     {ok,      {call, break_shim, manual_deblock, []}},
     {ok,      {call, break_shim, manual_reset, []}},
     {blocked, {call, break_shim, manual_block, []}}].

blocked(_Data) ->
    [{history, {call, break_shim, success, []}},
     {history, {call, break_shim, err, [valid_error()]}},
     {history, {call, break_shim, ignored_error, [ignored_error()]}},
     {history, {call, break_shim, timeout, []}},
     {history, {call, break_shim, manual_block, []}},
     {history, {call, break_shim, manual_reset, []}},
     {ok,      {call, break_shim, manual_deblock, []}}].

weight(ok, tripped, _) ->
    5;
weight(ok, ok, {call, _, F, _}) ->
    case F of
        error -> 4;
        timeout -> 4;
        _ -> 1
    end;
weight(_, _, _) ->
    1.

%% Picks whether a command should be valid under the current state.
precondition(unregistered, ok, _, {call, _, Call, _}) ->
    Call =:= success;
precondition(ok, To, #data{errors=N, limit=L}, {call,_,err,_}) ->
    (To =:= tripped andalso N+1 =:= L) orelse (To =:= ok andalso N+1 =/= L);
precondition(ok, To, #data{timeouts=N, limit=L}, {call,_,timeout,_}) ->
    (To =:= tripped andalso N+1 =:= L) orelse (To =:= ok andalso N+1 =/= L);
precondition(_From, _To, _Data, _Call) ->
    true.

%% Given the state `State' *prior* to the call `{call, Mod, Fun, Args}',
%% determine whether the result `Res' (coming from the actual system)
%% makes sense.
postcondition(tripped, tripped, _Data, _, {error, {circuit_breaker, _}}) ->
    true;
postcondition(_, blocked, _Data, {call, _, manual_block, _}, ok) -> %(2)
    true;
postcondition(_, blocked, _Data, _Call, {error, {circuit_breaker, _}}) -> %(3)
    true;
postcondition(_, ok, _Data, {call, _, success, _}, success) ->
    true;
postcondition(_, ok, _Data, {call, _, manual_deblock, _}, ok) ->
    true;
postcondition(_, _, _Data, {call, _, manual_reset, _}, ok) ->
    true;
postcondition(ok, _, _Data, {call, _, timeout, _}, {error, timeout}) -> %(4)
    true;
postcondition(ok, _, _Data, {call, _, err, _}, {error, Err}) ->
    not lists:member(Err, [ignore1, ignore2]);
postcondition(ok, _, _Data, {call, _, ignored_error, _}, {error, Err}) ->
    lists:member(Err, [ignore1, ignore2]);
postcondition(_From, _To, _Data, {call, _Mod, _Fun, _Args}, _Res) ->
    false.

%% Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.
next_state_data(ok, _To, Data=#data{errors=N}, _Res, {call,_,err,_}) ->
    Data#data{errors=N+1};
next_state_data(ok, _To, Data=#data{timeouts=N}, _Res, {call,_,timeout,_}) ->
    Data#data{timeouts=N+1};
next_state_data(_From, _To, Data, _Res, {call,_,manual_deblock,_}) ->
    Data#data{errors=0, timeouts=0};
next_state_data(_From, _To, Data, _Res, {call,_,manual_reset,_}) ->
    Data#data{errors=0, timeouts=0};
next_state_data(ok, _To, Data=#data{errors=E, timeouts=T}, _Res, %(5)
                {call, _, F, _}) when F =:= success; F =:= ignored_error ->
    if E > 0 -> Data#data{errors = E-1};
       T > 0 -> Data#data{timeouts = T-1};
       E =:= 0, T =:= 0 -> Data
    end;
next_state_data(_From, _To, Data, _Res, {call, _Mod, _Fun, _Args}) ->
    Data.

%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%
valid_error() -> elements([badarg, badmatch, badarith, whatever]).
ignored_error() -> elements([ignore1, ignore2]).
