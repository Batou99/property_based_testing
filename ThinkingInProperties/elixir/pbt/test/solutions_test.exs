#---
# Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
#---
defmodule SolutionsTest do
  use ExUnit.Case
  use PropCheck

  property "pair keysort approach" do
    # This function tests that any list of {key,val} pairs
    # end up being able to be sorted by the key by using List.keysort
    forall list <- list({term(), term()}) do
      # is_key_ordered checks that all tuples' keys are ordered.
      is_key_ordered(List.keysort(list, 0))
    end
  end

  def is_key_ordered([{a, _}, {b, _} = btuple | t]) do
    a <= b and is_key_ordered([btuple | t])
  end

  def is_key_ordered(_) do
    true
  end

  # This function instead works by using random tuples with various sizes,
  # and picking a random key to test it.
  # This tests broader usages of List.keysort/2, such as
  # List.keysort([{:a,:b},{:e,:f,:g},{:t,:a,:n,:e}], 2) yielding the list
  # [{:t,:a,:n,:e},{:a,:b},{:e,:f,:g}], where the comparison takes place
  # on the second element of each tuple.
  #
  # While more complete than the previous one, this function does not
  # accurately portray the need for stability in the function:
  # [{:a,:b}, {:a,:a}] being sorted in the same order will not be tested
  # here!
  #
  # Those can either be added in a regular test case, or would require
  # devising a different property.
  property "multi-sized tuple keysort approach" do
    forall list <- non_empty(list(non_empty(list()))) do
      # Since the default built-in types do not let us easily create
      # random-sized tuples that do not include {}, which wouldn't work
      # with List.keysort/2, we create variable-sized tuples ourselves
      tuples = for l <- list, do: List.to_tuple(l)
      # To know what position to use, we're going to use the smallest,
      # to avoid errors
      pos = Enum.min(for t <- tuples, do: tuple_size(t)) - 1
      sorted = List.keysort(tuples, pos)
      keys = extract_keys(sorted, pos)
      # The keys returned by keysort have to be in the same order as
      # returned by Enum.sort/1, which we now trust.
      keys == Enum.sort(keys)
    end
  end

  def extract_keys(list, pos) do
    for t <- list, do: :erlang.element(pos + 1, t)
  end


  def word_count(chars) do
    stripped = :string.trim(dedupe_spaces(chars), :both, ' ')
    spaces = Enum.sum(for char <- stripped, char == ?\s, do: 1)

    case stripped do
      '' -> 0
      _ -> spaces + 1
    end
  end

  defp dedupe_spaces([]), do: []
  defp dedupe_spaces([?\s, ?\s | rest]), do: dedupe_spaces([?\s | rest])
  defp dedupe_spaces([h | t]), do: [h | dedupe_spaces(t)]
  property "word counting" do
    forall chars <- non_empty(char_list()) do
      word_count(chars) == alt_word_count(chars)
    end
  end

  defp alt_word_count(string), do: space(to_charlist(string))

  defp space([]), do: 0
  defp space([?\s | str]), do: space(str)
  defp space(str), do: word(str)

  defp word([]), do: 1
  defp word([?\s | str]), do: 1 + space(str)
  defp word([_ | str]), do: word(str)
end
