#---
# Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
#---
defmodule Bday.MailTpl do
  def body(employee) do
    name = Bday.Employee.first_name(employee)
    "Happy birthday, dear #{name}!"
  end

  def full(employee) do
    {[Bday.Employee.email(employee)], "Happy birthday!", body(employee)}
  end
end
