#---
# Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
#---
defmodule BreakShim do
  @service :test_service

  def success() do
    :circuit_breaker.call(
      @service,
      fn -> :success end,
      :timer.hours(1),
      fn -> true end,
      :timer.hours(1),
      options()
    )
  end

  def err(reason) do
    :circuit_breaker.call(
      @service,
      fn -> {:error, reason} end,
      :timer.hours(1),
      fn -> true end,
      :timer.hours(1),
      options()
    )
  end

  def ignored_error(reason), do: err(reason)

  def timeout() do
    :circuit_breaker.call(
      @service,
      fn -> :timer.sleep(:infinity) end, #(1)
      0,
      fn -> true end,
      :timer.hours(1),
      options()
    )
  end

  def manual_block(), do: :circuit_breaker.block(@service)
  def manual_deblock(), do: :circuit_breaker.deblock(@service)
  def manual_reset(), do: :circuit_breaker.clear(@service)

  defp options() do
    [
      n_error: 3,
      time_error: :timer.minutes(30),
      n_timeout: 3,
      time_timeout: :timer.minutes(30),
      n_call_timeout: 3,
      time_call_timeout: :timer.minutes(30),
      ignore_errors: [:ignore1, :ignore2]
    ]
  end
end
