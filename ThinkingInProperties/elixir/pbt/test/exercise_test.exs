#---
# Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
#---
defmodule ExerciseTest do
  use ExUnit.Case
  use PropCheck

  property "set union" do
    forall {list_a, list_b} <- {list(number()), list(number())} do
      set_a = MapSet.new(list_a)
      set_b = MapSet.new(list_b)
      model_union = Enum.sort(list_a ++ list_b)

      res =
        MapSet.union(set_a, set_b)
        |> MapSet.to_list()
        |> Enum.sort()

      res == model_union
    end
  end


  property "merge dictionaries" do
    forall {list_a, list_b} <-
             {list({term(), term()}), list({term(), term()})} do
      merged =
        Map.merge(Map.new(list_a), Map.new(list_b), fn _k,v1,_v2 -> v1 end)

      extract_keys(Enum.sort(Map.to_list(merged))) ==
        Enum.sort(Enum.uniq(extract_keys(list_a ++ list_b)))
    end
  end

  def extract_keys(list), do: for({k, _} <- list, do: k)
end
