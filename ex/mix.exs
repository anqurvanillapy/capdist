defmodule Capdist.Mixfile do
  use Mix.Project

  def project do
    [ app: :capdist,
      version: "0.1.0",
      elixir: "~> 1.3.1",
      deps: deps ]
  end

  def application do
    []
  end

  defp deps do
    []
  end
end
