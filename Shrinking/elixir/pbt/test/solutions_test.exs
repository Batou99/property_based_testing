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
      appetizer <- [elements([:soup, :salad, :cheesesticks])],
      drink <- [elements([:coffee, :tea, :milk, :water, :juice])],
      entree <- [elements([:lasagna, :tofu, :steak])],
      dessert <- [elements([:cake, :chocolate, :icecream])]
    ]) do
      appetizer ++ drink ++ entree ++ dessert
    end
  end


  def item_price_special() do
    # first let: freeze the price list
    let price_list <- price_list() do
      # second let: freeze the list of specials
      let special_list <- special_list(price_list) do
        # third let: regular + special items and prices
        # help shrinking by first trying only one of the
        # two possible lists in case a specific form causes
        # the problems on its own
        # and we merge
        let {items, price} <-
              (let_shrink([
                 {regular_items, regular_expected} <-
                   regular_gen(price_list, special_list),
                 {special_items, special_expected} <-
                   special_gen(price_list, special_list)
               ]) do
                 {regular_items ++ special_items,
                  regular_expected + special_expected}
               end) do
          {items, price, price_list, special_list}
        end
      end
    end
  end


  defp price_list() do
    let price_list <- non_empty(list({non_empty(utf8()), integer()})) do
      sorted = Enum.sort(price_list)
      Enum.dedup_by(sorted, fn {x, _} -> x end)
    end
  end

  defp special_list(price_list) do
    items = for {name, _} <- price_list, do: name

    let specials <- list({elements(items), choose(2, 5), integer()}) do
      sorted = Enum.sort(specials)
      Enum.dedup_by(sorted, fn {x, _, _} -> x end)
    end
  end

  defp regular_gen(price_list, special_list) do
    regular_gen(price_list, special_list, [], 0)
  end

  defp regular_gen([], _, list, price), do: {list, price}

  defp regular_gen([{item, cost} | prices], specials, items, price) do
    count_gen =
      case List.keyfind(specials, item, 0) do
        {_, limit, _} -> choose(0, limit - 1)
        _ -> non_neg_integer()
      end

    let count <- count_gen do
      regular_gen(
        prices,
        specials,
        let(v <- vector(count, item), do: v ++ items),
        cost * count + price
      )
    end
  end

  defp special_gen(_, special_list) do
    special_gen(special_list, [], 0)
  end

  defp special_gen([], items, price), do: {items, price}

  defp special_gen([{item, count, cost} | specials], items, price) do
    let multiplier <- non_neg_integer() do
      special_gen(
        specials,
        let(v <- vector(count * multiplier, item), do: v ++ items),
        cost * multiplier + price
      )
    end
  end
end
