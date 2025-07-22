defmodule RivaAsh.MixProject do
  use Mix.Project

  def project do
    [
      app: :riva_ash,
      version: "0.1.0",
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases(),
      test_coverage: [tool: ExCoveralls]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [
        :logger,
        :runtime_tools,
        :os_mon,
        :ssl,
        :crypto,
        :public_key,
        :inets
      ],
      mod: {RivaAsh.Application, []},
      start_phases: [migrate: []]
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

      # Authentication
      {:ash_authentication, "~> 4.9"},
      {:ash_authentication_phoenix, "~> 2.6"},
      {:bcrypt_elixir, "~> 3.1"},

      # Phoenix
      {:phoenix, "~> 1.7"},
      {:phoenix_ecto, "~> 4.5"},
      {:phoenix_live_view, "~> 1.0"},
      {:phoenix_html, "~> 4.0"},
      {:live_react, "~> 1.1"},

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
      {:timex, "~> 3.0"},   # Added Timex dependency

      # UI Components
      {:salad_ui, "~> 1.0.0-beta.3"},
      {:heroicons, "~> 0.5.0"},
      {:flop, "~> 0.26.0"},
      {:flop_phoenix, "~> 0.25.2"},

      # Development
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      {:live_debugger, "~> 0.3.0", only: :dev},
      {:phoenix_storybook, "~> 0.6.0", only: :dev},
      {:tailwind, "~> 0.2", runtime: Mix.env() == :dev},
      {:esbuild, "~> 0.8", runtime: Mix.env() == :dev},

      # Test
      {:stream_data, "~> 1.0"},
      {:mox, "~> 1.1", only: :test},
      {:phoenix_test, "~> 0.7.0", only: :test, runtime: false},
      {:excoveralls, "~> 0.18", only: :test}
    ]
  end

  defp aliases do
    [
      setup: ["deps.get", "ecto.setup"],
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      test: fn _ ->
        if System.get_env("SKIP_DB") == "true" do
          ["test"]
        else
          ["ecto.create --quiet", "ecto.migrate --quiet", "test"]
        end
      end,
      "assets.deploy": [
        "tailwind default --minify",
        "tailwind storybook --minify",
        "esbuild default --minify",
        "phx.digest"
      ]
    ]
  end
end
