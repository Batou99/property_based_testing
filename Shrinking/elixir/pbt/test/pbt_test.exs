#---
# Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
#---
defmodule PbtTest do
  use ExUnit.Case
  use PropCheck

  property "dairy" do
    forall food <- meal() do
      dairy_count(food) == 0
    end
  end

  defp dairy_count(list) do
    Enum.count(list, &is_dairy/1)
  end

  defp is_dairy(:cheesesticks), do: true
  defp is_dairy(:lasagna), do: true
  defp is_dairy(:icecream), do: true
  defp is_dairy(:milk), do: true
  defp is_dairy(_), do: false

  def meal() do
    let_shrink([
      appetizer <- elements([:soup, :salad, :cheesesticks]),
      drink <- elements([:coffee, :tea, :milk, :water, :juice]),
      entree <- elements([:lasagna, :tofu, :steak]),
      dessert <- elements([:cake, :chocolate, :icecream])
    ]) do
      [appetizer, drink, entree, dessert]
    end
  end


  def strdatetime() do
    let(date_time <- datetime(), do: to_str(date_time))
  end

  def datetime() do
    {date(), time(), timezone()}
  end

  def date() do
    such_that(
      {y, m, d} <- {year(), month(), day()},
      when: :calendar.valid_date(y, m, d)
    )
  end

  def year() do
    shrink(range(0, 9999), [range(1970, 2000), range(1900, 2100)]) #(1)
  end

  def month(), do: range(1, 12)

  def day(), do: range(1, 31)

  def time(), do: {range(0, 24), range(0, 59), range(0, 60)}

  def timezone() do
    {elements([:+, :-]), shrink(range(0, 99), [range(0, 14), 0]),
     shrink(range(0, 99), [0, 15, 30, 45])}
  end

  def to_str({{y, m, d}, {h, mi, s}, {sign, ho, mo}}) do
    format_str = "~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b~s~2..0b:~2..0b"

    :io_lib.format(format_str, [y, m, d, h, mi, s, sign, ho, mo])
    |> to_string()
  end

  def tree(n) when n <= 1 do
    {:leaf, number()}
  end

  def tree(n) do
    per_branch = div(n, 2)
    {:branch, tree(per_branch), tree(per_branch)}
  end


  def tree_shrink(n) when n <= 1 do
    {:leaf, number()}
  end

  def tree_shrink(n) do
    per_branch = div(n, 2)

    let_shrink([
      left <- tree_shrink(per_branch),
      right <- tree_shrink(per_branch)
    ]) do
      {:branch, left, right}
    end
  end

end
