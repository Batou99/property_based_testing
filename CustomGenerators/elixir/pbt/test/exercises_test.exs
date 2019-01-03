#---
# Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
#---
defmodule PbtExercises do
  use ExUnit.Case
  use PropCheck

  def tree(), do: tree(term())

  def tree(type) do
    frequency([
      {1, {:node, type, tree(type), nil}},
      {1, {:node, type, nil, tree(Type)}},
      {5, {:node, type, tree(type), tree(type)}}
    ])
  end


  def stamp(), do: {hour(), min(), sec()}
  def hour(), do: choose(0, 23)
  def min(), do: choose(0, 59)
  def sec(), do: choose(0, 59)

  # returning hours in the morning
  def am_stamp1() do
    such_that({h, _, _} <- stamp(), when: h < 12)
  end

  def am_stamp2() do
    let({h, m, s} <- stamp(), do: {rem(h, 12), m, s})
  end


  # Return two ordered timestamps
  def stamps1() do
    such_that({s1, s2} <- {stamp(), stamp()}, when: s1 <= s2)
  end

  def stamps2() do
    let({s1, s2} <- {stamp(), stamp()}, do: {min(s1, s2), max(s1, s2)})
  end


  # Return any time that does not overlap standup meetings
  def no_standup1() do
    such_that({h, m, _} <- stamp(), when: h != 9 or m > 10)
  end

  def no_standup2() do
    let {h, m, s} <- stamp() do
      case h do
        9 when m <= 10 -> {8, m, s}
        _ -> {h, m, s}
      end
    end
  end


  def file_open(name, opts) do
    {:ok, fd} = File.open(name, opts)
    # ensure the file is refreshed on each run
    :file.truncate(fd)
    fd
  end

  def file_write(fd, data) do
    IO.write(fd, data)
    fd
  end

end
