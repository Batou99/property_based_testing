#---
# Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
#---
defmodule PbtSolutions do
  use ExUnit.Case
  use PropCheck

  def tree(), do: tree(term())

  def tree(type) do
    frequency([
      {3, {:node, type, nil, nil}},
      {2, {:node, type, lazy(tree(type)), nil}},
      {2, {:node, type, nil, lazy(tree(Type))}},
      {3, {:node, type, lazy(tree(type)), lazy(tree(type))}}
    ])
  end


  def limited_tree(type) do
    sized(size, limited_tree(size, type))
  end

  def limited_tree(size, type) when size <= 1, do: {:node, type, nil, nil}

  def limited_tree(size, type) do
    frequency([
      {1, {:node, type, lazy(limited_tree(size - 1, type)), nil}},
      {1, {:node, type, nil, lazy(limited_tree(size - 1, type))}},
      {5,
       {
         :node,
         type,
         # divide to avoid exponential growth
         lazy(limited_tree(div(size, 2), type)),
         lazy(limited_tree(div(size, 2), type))
       }}
    ])
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

  def file(name) do
    sized(
      size,
      lines(
        size,
        {:"$call", __MODULE__, :file_open, [name, [:read, :write, :utf8]]}
      )
    )
  end

  def lines(size, fd) when size <= 1, do: fd

  def lines(size, fd) do
    lines(
      size - 1,
      {:"$call", __MODULE__, :file_write, [fd, non_empty(utf8())]}
    )
  end

end
