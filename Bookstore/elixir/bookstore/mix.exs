#---
# Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
#---
defmodule Bookstore.MixProject do
  use Mix.Project

  def project do
    [
      app: :bookstore,
      version: "0.1.0",
      elixir: "~> 1.6",
      elixirc_paths: elixirc_paths(Mix.env),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      escript: escript_config()
    ]
  end

  defp elixirc_paths(:test), do: ["lib","test/"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Bookstore.App, []},
      env: [
        pg: [
          # Single quotes are important
          user: 'ferd', # replace with your own $USER
          password: '',
          database: 'bookstore_db', # as specified by bookstore_init.ex
          host: '127.0.0.1',
          port: 5432,
          ssl: false # not for tests!
        ]
      ]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:eql, "~> 0.1.2", manager: :rebar3},
      {:pgsql, "~> 26.0"},
      {:propcheck, "~> 1.1", only: [:test, :dev]}
    ]
  end

  defp escript_config do
    [main_module: Bookstore.Init, app: nil]
  end
end
