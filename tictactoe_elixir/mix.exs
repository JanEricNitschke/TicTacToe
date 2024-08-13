defmodule TictactoeElixir.MixProject do
  use Mix.Project

  def project do
    [
      app: :tictactoe_elixir,
      version: "0.1.0",
      elixir: "~> 1.16",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      dialyzer: [flags: [:unmatched_returns, :error_handling, :underspecs, :extra_return, :missing_return]]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      mod: {TictactoeApp, []},
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:dialyxir, "~> 1.4", only: [:dev], runtime: false}
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end
end
