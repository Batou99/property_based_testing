#---
# Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
#---
defmodule CacheTest do
  use ExUnit.Case
  use PropCheck
  use PropCheck.StateM
  doctest Cache
  @moduletag timeout: :infinity

  @cache_size 10

  property "stateful property", [:verbose] do
    forall cmds <- commands(__MODULE__) do
      Cache.start_link(@cache_size)
      {history, state, result} = run_commands(__MODULE__, cmds)
      Cache.stop()

      (result == :ok)
      |> aggregate(command_names(cmds))
      |> when_fail(
        IO.puts("""
        History: #{inspect(history}
        State: #{inspect(state)}
        Result: #{inspect(result)}
        """)
      )
    end
  end


  property "parallel stateful property", numtests: 10000 do
    forall cmds <- parallel_commands(__MODULE__) do
      Cache.start_link(@cache_size)
      {history, state, result} = run_parallel_commands(__MODULE__, cmds)
      Cache.stop()

      (result == :ok)
      |> aggregate(command_names(cmds))
      |> when_fail(
        IO.puts("""
        =======
        Failing command sequence
        #{inspect(cmds)}
        At state: #{inspect(state)}
        =======
        Result: #{inspect(result)}
        History: #{inspect(history)}
        """)
      )
    end
  end

  defmodule State do
    @cache_size 10
    defstruct max: @cache_size, count: 0, entries: []
  end

  def initial_state(), do: %State{}

  def command(_state) do
    frequency([
      {1, {:call, Cache, :find, [key()]}},
      {3, {:call, Cache, :cache, [key(), val()]}},
      {1, {:call, Cache, :flush, []}}
    ])
  end


  def precondition(%State{count: 0}, {:call, Cache, :flush, []}) do
    false
  end

  def precondition(%State{}, {:call, _mod, _fun, _args}) do
    true
  end


  # Given that state prior to the call `{:call, mod, fun, args}`,
  # determine whether the result (res) coming from the actual system
  # makes sense according to the model
  def postcondition(%State{entries: l}, {:call, _, :find, [key]}, res) do
    case List.keyfind(l, key, 0) do
      nil ->
        res == {:error, :not_found}

      {^key, val} ->
        res == {:ok, val}
    end
  end

  def postcondition(_state, {:call, _mod, _fun, _args}, _res) do
    true
  end


  # Assuming the postcondition for a call was true, update the model
  # accordingly for the test to proceed
  def next_state(state, _res, {:call, Cache, :flush, _}) do
    %{state | count: 0, entries: []}
  end

  def next_state(
        s = %State{entries: l, count: n, max: m},
        _res,
        {:call, Cache, :cache, [k, v]}
      ) do
    case List.keyfind(l, k, 0) do
      nil when n == m -> #(1)
        %{s | entries: tl(l) ++ [{k, v}]}

      nil when n < m -> #(2)
        %{s | entries: l ++ [{k, v}], count: n + 1}

      {^k, _} -> #(3)
        %{s | entries: List.keyreplace(l, k, 0, {k, v})}
    end
  end

  def next_state(state, _res, {:call, _mod, _fun, _args}) do
    state
  end


  ## Generators ##
  def key() do
    oneof([range(1, @cache_size), integer()])
  end

  def val() do
    integer()
  end
end
