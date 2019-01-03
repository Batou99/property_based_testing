#---
# Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
#---
defmodule Checkout do
  @moduledoc """
  A Checkout counter implementation
  """

  def valid_price_list(list) do
    sorted = Enum.sort(list)
    length(list) == length(Enum.dedup_by(sorted, fn {x, _} -> x end))
  end

  def valid_special_list(list) do
    sorted = Enum.sort(list)

    Enum.all?(list, fn {_, x, _} -> x != 0 end) &&
      length(list) == length(Enum.dedup_by(sorted, fn {x, _, _} -> x end))
  end


  def total(item_list, price_list, specials) do
    if not valid_price_list(price_list) do #(1)
      raise RuntimeError, message: "invalid list of prices"
    end

    if not valid_special_list(specials) do
      raise RuntimeError, message: "invalid list of specials"
    end

    counts = count_seen(item_list)
    {counts_left, prices} = apply_specials(counts, specials)
    prices + apply_regular(counts_left, price_list)
  end


  defp count_seen(item_list) do
    count = fn x -> x + 1 end

    Map.to_list(
      Enum.reduce(item_list, Map.new(), fn item, m ->
        Map.update(m, item, 1, count)
      end)
    )
  end

  defp apply_specials(items, specials) do
    Enum.map_reduce(items, 0, fn {name, count}, price ->
      case List.keyfind(specials, name, 0) do
        nil ->
          {{name, count}, price}

        {_, needed, value} ->
          {{name, rem(count, needed)}, value * div(count, needed) + price}
      end
    end)
  end

  defp apply_regular(items, price_list) do
    Enum.sum(
      for {name, count} <- items do
        count * cost_of_item(price_list, name)
      end
    )
  end

  defp cost_of_item(price_list, name) do
    case List.keyfind(price_list, name, 0) do
      nil -> raise RuntimeError, message: "unknown item: #{name}"
      {_, price} -> price
    end
  end

  ## Old work-in-progress implementations
  defmodule V1 do
    def total(item_list, price_list, _specials) do
      Enum.sum(
        for item <- item_list do
          elem(List.keyfind(price_list, item, 0), 1)
        end
      )
    end

  end

  defmodule V2 do
    def total(item_list, price_list, specials) do
      counts = count_seen(item_list)
      {counts_left, prices} = apply_specials(counts, specials)
      prices + apply_regular(counts_left, price_list)
    end


    defp count_seen(item_list) do
      count = fn x -> x + 1 end

      Map.to_list(
        Enum.reduce(item_list, Map.new(), fn item, m ->
          Map.update(m, item, 1, count)
        end)
      )
    end


    defp apply_specials(items, specials) do
      Enum.map_reduce(items, 0, fn {name, count}, price ->
        case List.keyfind(specials, name, 0) do
          nil ->
            {{name, count}, price} #(2)

          {_, needed, value} ->
            {{name, rem(count, needed)}, #(3)
              value * div(count, needed) + price} #(4)
        end
      end)
    end

    defp apply_regular(items, price_list) do
      Enum.sum(
        for {name, count} <- items do
          {_, price} = List.keyfind(price_list, name, 0)
          count * price
        end
      )
    end

  end

  defmodule V3 do
    def valid_special_list(list) do
      Enum.all?(list, fn {_, x, _} -> x != 0 end)
    end

    def total(item_list, price_list, specials) do
      if not valid_special_list(specials) do #(5)
        raise RuntimeError, message: "invalid list of specials"
      end

      counts = count_seen(item_list)
      {counts_left, prices} = apply_specials(counts, specials)
      prices + apply_regular(counts_left, price_list)
    end


    defp count_seen(item_list) do
      count = fn x -> x + 1 end

      Map.to_list(
        Enum.reduce(item_list, Map.new(), fn item, m ->
          Map.update(m, item, 1, count)
        end)
      )
    end


    defp apply_specials(items, specials) do
      Enum.map_reduce(items, 0, fn {name, count}, price ->
        case List.keyfind(specials, name, 0) do
          nil ->
            {{name, count}, price}

          {_, needed, value} ->
            {{name, rem(count, needed)}, value * div(count, needed) + price}
        end
      end)
    end


    defp apply_regular(items, price_list) do
      Enum.sum(
        for {name, count} <- items do
          count * cost_of_item(price_list, name)
        end
      )
    end

    defp cost_of_item(price_list, name) do
      case List.keyfind(price_list, name, 0) do
        nil -> raise RuntimeError, message: "unknown item: #{name}"
        {_, price} -> price
      end
    end

  end
end
