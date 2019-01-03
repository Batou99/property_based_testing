#---
# Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
#---
defmodule Bday.Employee do
  @opaque employee() :: %{required(String.t()) => term()}
  @opaque handle() :: {:raw, [employee()]}

  def from_csv(string) do
    {:raw,
     for map <- Bday.Csv.decode(string) do
       adapt_csv_result(map)
     end}
  end


  @spec fetch(handle()) :: [employee()]
  def fetch({:raw, maps}), do: maps

  @spec last_name(employee()) :: String.t() | nil
  def last_name(%{"last_name" => name}), do: name

  @spec first_name(employee()) :: String.t() | nil
  def first_name(%{"first_name" => name}), do: name

  @spec date_of_birth(employee()) :: Date.t()
  def date_of_birth(%{"date_of_birth" => dob}), do: dob

  @spec email(employee()) :: String.t()
  def email(%{"email" => email}), do: email

  @spec filter_birthday(handle(), Date.t()) :: handle()
  def filter_birthday({:raw, employees}, date) do
    {:raw, Bday.Filter.birthday(employees, date)}
  end


  if Mix.env() == :test do
    def adapt_csv_result_shim(map), do: adapt_csv_result(map)
  end

  defp adapt_csv_result(map) do
    map =
      for {k, v} <- map, into: %{} do
        {trim(k), maybe_null(trim(v))}
      end

    dob = Map.fetch!(map, "date_of_birth")
    %{map | "date_of_birth" => parse_date(dob)}
  end

  defp trim(str), do: String.trim_leading(str, " ")

  defp maybe_null(""), do: nil
  defp maybe_null(str), do: str

  defp parse_date(str) do
    [y, m, d] = Enum.map(String.split(str, "/"), &String.to_integer(&1))
    {:ok, date} = Date.new(y, m, d)
    date
  end

end
