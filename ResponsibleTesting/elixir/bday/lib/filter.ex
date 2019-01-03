#---
# Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
#---
defmodule Bday.Filter do
  def birthday(people, date = %Date{month: 2, day: 28}) do
    case Date.leap_year?(date) do
      true -> filter_dob(people, 2, 28)
      false -> filter_dob(people, 2, 28) ++ filter_dob(people, 2, 29)
    end
  end

  def birthday(people, %Date{month: m, day: d}) do
    filter_dob(people, m, d)
  end

  defp filter_dob(people, month, day) do
    Enum.filter(
      people,
      fn %{"date_of_birth" => %Date{month: m, day: d}} ->
        {month, day} == {m, d}
      end
    )
  end
end
