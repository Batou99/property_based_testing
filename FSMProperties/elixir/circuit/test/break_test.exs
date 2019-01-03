#---
# Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
#---
defmodule BreakTest do
  use ExUnit.Case
  use PropCheck
  use PropCheck.FSM

  property "FSM property for circuit breakers", [:verbose] do
    Application.stop(:circuit_breaker) # we take over that
    forall cmds <- commands(__MODULE__) do
      {:ok, pid} = :circuit_breaker.start_link()
      {history, state, result} = run_commands(__MODULE__, cmds)
      GenServer.stop(pid, :normal, 5000) #(1)

      (result == :ok)
      |> aggregate(
        :proper_statem.zip(state_names(history), command_names(cmds))
      )
      |> when_fail(
        IO.puts("""
        History: #{inspect(history)}
        State: #{inspect(state)}
        Result: #{inspect(result)}
        """)
      )
    end
  end

  # Initial state for the state machine
  def initial_state(), do: :unregistered
  # Initial model at the start. Should be deterministic
  def initial_state_data() do
    %{limit: 3, errors: 0, timeouts: 0}
  end

  # State command generation
  def unregistered(_data) do
    [{:ok, {:call, BreakShim, :success, []}}]
  end

  def ok(_data) do
    [
      {:history, {:call, BreakShim, :success, []}},
      {:history, {:call, BreakShim, :err, [valid_error()]}},
      {:tripped, {:call, BreakShim, :err, [valid_error()]}},
      {:history, {:call, BreakShim, :ignored_error, [ignored_error()]}},
      {:history, {:call, BreakShim, :timeout, []}},
      {:tripped, {:call, BreakShim, :timeout, []}},
      {:blocked, {:call, BreakShim, :manual_block, []}},
      {:ok, {:call, BreakShim, :manual_deblock, []}},
      {:ok, {:call, BreakShim, :manual_reset, []}}
    ]
  end

  def tripped(_data) do
    [
      {:history, {:call, BreakShim, :success, []}},
      {:history, {:call, BreakShim, :err, [valid_error()]}},
      {:history, {:call, BreakShim, :ignored_error, [ignored_error()]}},
      {:history, {:call, BreakShim, :timeout, []}},
      {:ok, {:call, BreakShim, :manual_deblock, []}},
      {:ok, {:call, BreakShim, :manual_reset, []}},
      {:blocked, {:call, BreakShim, :manual_block, []}}
    ]
  end

  def blocked(_data) do
    [
      {:history, {:call, BreakShim, :success, []}},
      {:history, {:call, BreakShim, :err, [valid_error()]}},
      {:history, {:call, BreakShim, :ignored_error, [ignored_error()]}},
      {:history, {:call, BreakShim, :timeout, []}},
      {:history, {:call, BreakShim, :manual_block, []}},
      {:history, {:call, BreakShim, :manual_reset, []}},
      {:ok, {:call, BreakShim, :manual_deblock, []}}
    ]
  end

  # Optional callback, weight modification of transitions
  def weight(:ok, :tripped, _) do
    5
  end
  def weight(:ok, :ok, {:call, _, f, _}) do
    case f do
      :error -> 4
      :timeout -> 4
      _ -> 1
    end
  end
  def weight(_, _, _) do
    1
  end

  # Picks whether a command should be valid
  def precondition(:unregistered, :ok, _, {:call, _, call, _}) do
    call == :success
  end
  def precondition(:ok, to, %{errors: n, limit: l}, {:call, _, :err, _}) do
    (to == :tripped and n + 1 == l) or (to == :ok and n + 1 != l)
  end
  def precondition(
        :ok,
        to,
        %{timeouts: n, limit: l},
        {:call, _, :timeout, _}
      ) do
    (to == :tripped and n + 1 == l) or (to == :ok and n + 1 != l)
  end
  def precondition(_from, _to, _data, _call) do
    true
  end

  # Given that state prior to the call `{:call, mod, fun, args}`,
  # determine whether the result (res) coming from the actual system
  # makes sense according to the model
  def postcondition(
        :tripped,
        :tripped,
        _data,
        _call,
        {:error, {:circuit_breaker, _}}
      ) do
    true
  end
  def postcondition(_, :blocked, _data, {_, _, :manual_block, _}, :ok) do #(2)
    true
  end
  def postcondition(
        _from,
        :blocked,
        _data,
        _call,
        {:error, {:circuit_breaker, _}} #(3)
      ) do
    true
  end
  def postcondition(_, :ok, _data, {_, _, :success, _}, :success) do
    true
  end
  def postcondition(_, :ok, _data, {_, _, :manual_deblock, _}, :ok) do
    true
  end
  def postcondition(_, _, _data, {_, _, :manual_reset, _}, :ok) do
    true
  end
  def postcondition( #(4)
        :ok,
        _to,
        _data,
        {_, _, :timeout, _},
        {:error, :timeout}
      ) do
    true
  end
  def postcondition(:ok, _to, _data, {_, _, :err, _}, {:error, err}) do
    not Enum.member?([:ignore1, :ignore2], err)
  end
  def postcondition(
        :ok,
        _to,
        _data,
        {_, _, :ignored_error, _},
        {:error, err}
      ) do
    Enum.member?([:ignore1, :ignore2], err)
  end
  def postcondition(_from, _to, _data, {:call, _m, _f, _args}, _res) do
    false
  end

  # Assuming the postcondition for a call was true, update the model
  # accordingly for the test to proceed
  def next_state_data(:ok, _, data = %{errors: n}, _, {_, _, :err, _}) do
    %{data | errors: n + 1}
  end
  def next_state_data(:ok, _, d = %{timeouts: n}, _, {_, _, :timeout, _}) do
    %{d | timeouts: n + 1}
  end
  def next_state_data(_from, _to, data, _, {_, _, :manual_deblock, _}) do
    %{data | errors: 0, timeouts: 0}
  end
  def next_state_data(_from, _to, data, _, {_, _, :manual_reset, _}) do
    %{data | errors: 0, timeouts: 0}
  end
  def next_state_data( #(5)
        :ok,
        _to,
        data = %{errors: e, timeouts: t},
        _res,
        {:call, _, f, _}
      )
      when f == :success or f == :ignored_error do
    cond do
      e > 0 ->
        %{data | errors: e - 1}
      t > 0 ->
        %{data | timeouts: t - 1}
      e == 0 and t == 0 ->
        data
    end
  end
  def next_state_data(_from, _to, data, _res, {:call, _m, _f, _args}) do
    data
  end

  def valid_error() do
    elements([:badarg, :badmatch, :badarith, :whatever])
  end
  def ignored_error() do
    elements([:ignore1, :ignore2])
  end
end
