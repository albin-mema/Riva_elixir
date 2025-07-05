defmodule RivaAsh.MixProject do
  use Mix.Project

  def project do
    [
      app: :riva_ash,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {RivaAsh.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # Ash core
      {:ash, "~> 3.5"},
      {:ash_postgres, "~> 2.6"},
      {:ash_json_api, "~> 1.4"},
      {:ash_graphql, "~> 1.7"},

      # Ash extensions
      {:ash_paper_trail, "~> 0.5.6"},
      {:ash_archival, "~> 1.0"},
      {:ash_admin, "~> 0.13"},
      {:reactor, "~> 0.15", override: true},

      # SAT solver for Ash policies (pure Elixir implementation)
      {:simple_sat, "~> 0.1.3"},

      # Phoenix
      {:phoenix, "~> 1.7"},
      {:phoenix_ecto, "~> 4.5"},

      # Database
      {:ecto_sql, "~> 3.13"},
      {:postgrex, "~> 0.18"},

      # Telemetry
      {:telemetry_metrics, "~> 1.0"},
      {:telemetry_poller, "~> 1.0"},

      # HTTP/API
      {:bandit, "~> 1.7"},
      {:finch, "~> 0.18"},
      {:open_api_spex, "~> 3.21"},
      {:cors_plug, "~> 3.0"},

      # GraphQL
      {:absinthe, "~> 1.7"},
      {:absinthe_plug, "~> 1.5"},

      # Utilities
      {:gettext, "~> 0.24"},
      {:jason, "~> 1.4"},
      {:dns_cluster, "~> 0.1.1"},

      # Test
      {:mox, "~> 1.1", only: :test}
    ]
  end

  defp aliases do
    [
      setup: ["deps.get", "ecto.setup"],
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      test: ["ecto.create --quiet", "ecto.migrate --quiet", "test"]
    ]
  end
end
