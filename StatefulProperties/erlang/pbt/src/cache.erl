%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(cache).
-export([start_link/1, stop/0, cache/2, find/1, flush/0]).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link(N) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, N, []).

stop() ->
    gen_server:stop(?MODULE).

find(Key) ->
    case ets:match(cache, {'_', {Key, '$1'}}) of
        [[Val]] -> {ok, Val};
        [] -> {error, not_found}
    end.

cache(Key, Val) ->
    case ets:match(cache, {'$1', {Key, '_'}}) of % find dupes
        [[N]] -> %(1)
            ets:insert(cache, {N,{Key,Val}}); % overwrite dupe
        [] ->
            case ets:lookup(cache, count) of % insert new
                [{count,Max,Max}] -> %(2)
                    ets:insert(cache, [{1,{Key,Val}}, {count,1,Max}]);
                [{count,Current,Max}] -> %(3)
                    ets:insert(cache, [{Current+1,{Key,Val}},
                                       {count,Current+1,Max}])
            end
    end.

flush() ->
    [{count,_,Max}] = ets:lookup(cache, count),
    ets:delete_all_objects(cache),
    ets:insert(cache, {count, 0, Max}).

init(N) ->
    ets:new(cache, [public, named_table]),
    ets:insert(cache, {count, 0, N}),
    {ok, nostate}.

handle_call(_Call, _From, State) -> {noreply, State}.

handle_cast(_Cast, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.
