#---
# Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
#---
defmodule Bookstore.Init do
  def main(_) do
    # See: https://www.postgresql.org/docs/9.6/static/server-start.html
    File.mkdir_p!("postgres/data/")
    stdout = IO.stream(:stdio, :line)

    IO.puts("initializing database structure...")
    System.cmd("initdb", ["-D", "postgres/data"], into: stdout)
    IO.puts("starting postgres instance...")

    args = ["-D", "postgres/data", "-l", "logfile", "start"]
    case :os.type() do
      {:win32, _} ->
        spawn(fn -> System.cmd("pg_ctl", args, into: stdout) end)
      {:unix, _} ->
        System.cmd("pg_ctl", args, into: stdout)
    end

    # wait and pray
    Process.sleep(5000)
    IO.puts("setting up 'bookstore_db' database...")
    System.cmd(
      "psql",
      [
        "-h", "localhost",
        "-d", "template1",
        "-c", "CREATE DATABASE bookstore_db;"
      ],
      into: stdout
    )
    IO.puts("OK.")
    :init.stop()
  end
end
