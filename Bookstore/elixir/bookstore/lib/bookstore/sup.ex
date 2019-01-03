#---
# Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
#---
defmodule Bookstore.Sup do
  use Supervisor

  def start_link() do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    Bookstore.DB.load_queries()
    Supervisor.init([], strategy: :one_for_one)
  end
end
