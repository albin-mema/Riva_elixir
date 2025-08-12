defmodule RivaAsh.MixProject do
  use Mix.Project

  @spec project :: keyword()
  def project do
    [
      app: :riva_ash,
      version: "0.2.0",
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases(),
      test_coverage: [tool: ExCoveralls],
      dialyzer: [
        plt_file: {:no_warn, "priv/plts/dialyzer.plt"},
        flags: [:error_handling, :underspecs, :unknown],
        ignore_warnings: ".dialyzer_ignore.exs",
        plt_add_apps: [:mix, :eex, :iex, :ex_unit, :ash, :phoenix],
        plt_core_path: "priv/plts",
        check_plt: true
      ],
      # Source code configuration
      consolidate_protocols: Mix.env() != :test,
      erlc_options: [:warnings_as_errors, :warn_unused_vars],
      elixirc_paths: elixirc_paths(Mix.env()),
      # Compilation configuration
      compilers: Mix.compilers(),
      # Package configuration
      package: package(),
      # Documentation configuration
      name: "Riva Ash",
      source_url: "https://github.com/your-org/riva_ash",
      homepage_url: "https://github.com/your-org/riva_ash",
      docs: [
        main: "readme",
        extras: ["README.md", "docs/README.md", "docs/configuration.md", "docs/api.md"],
        groups_for_modules: [
          Ash: ~r/^Ash/,
          RivaAsh: ~r/^RivaAsh/,
          Web: ~r/^RivaAshWeb/,
          Auth: ~r/^RivaAsh.*Auth/,
          Components: ~r/^RivaAshWeb.*Component/,
          Live: ~r/^RivaAshWeb.*Live/,
          Resources: ~r/^RivaAsh\.Resources\./,
          Accounts: ~r/^RivaAsh\.Accounts\./
        ],
        nest_modules_by_prefix: [
          RivaAshWeb,
          RivaAsh.Resources,
          RivaAsh.Accounts
        ]
      ],
      # Build configuration
      build_embedded: Mix.env() == :prod,
      lockfile: "mix.lock",
      # Type safety: Configure Dialyzer for comprehensive type checking
      dialyzer: [
        plt_file: {:no_warn, "priv/plts/dialyzer.plt"},
        flags: [:error_handling, :underspecs, :unknown],
        ignore_warnings: ".dialyzer_ignore.exs",
        plt_add_apps: [:mix, :eex, :iex, :ex_unit, :ash, :phoenix],
        plt_core_path: "priv/plts",
        check_plt: true
      ]
    ]
  end

  @spec application :: keyword()
  def application do
    start_phases = if Mix.env() == :prod, do: [migrate: [], setup_extensions: [], load_extensions: []], else: []

    [
      extra_applications: [
        :logger,
        :runtime_tools,
        :os_mon,
        :ssl,
        :crypto,
        :public_key,
        :inets,
        :ash,
        :ash_postgres,
        :phoenix,
        :ecto_sql,
        :telemetry_metrics,
        :telemetry_poller
      ],
      mod: {RivaAsh.Application, []},
      start_phases: start_phases,
      env: [
        # Application-specific configuration
        ash_ecto_repo: RivaAsh.Repo,
        ash_json_api: %{
          json_api_url: "/api/v1",
          json_api_key: "riva_ash_api"
        }
      ]
    ]
  end

  @spec deps :: [{atom(), binary()} | {atom(), binary(), keyword()}]
  defp deps do
    [
      # === Core Dependencies ===
      {:ash, "~> 3.5"},
      {:ash_postgres, "~> 2.6"},
      {:ash_json_api, "~> 1.4"},
      {:ash_graphql, "~> 1.7"},
      {:reactor, "~> 0.15", override: true},

      # === Ash Extensions ===
      {:ash_paper_trail, "~> 0.5.6"},
      {:ash_archival, "~> 1.0"},
      {:ash_admin, "~> 0.13"},
      {:simple_sat, "~> 0.1.3"},

      # === Authentication ===
      {:ash_authentication, "~> 4.9"},
      {:ash_authentication_phoenix, "~> 2.6"},
      {:bcrypt_elixir, "~> 3.1"},

      # === Phoenix Framework ===
      {:phoenix, "~> 1.7"},
      {:phoenix_ecto, "~> 4.5"},
      {:phoenix_live_view, "~> 1.0"},
      {:phoenix_html, "~> 4.0"},
      {:live_react, "~> 1.1"},

      # === Database & Persistence ===
      {:ecto_sql, "~> 3.13"},
      {:postgrex, "~> 0.18"},

      # === Telemetry & Monitoring ===
      {:telemetry_metrics, "~> 1.0"},
      {:telemetry_poller, "~> 1.0"},

      # === HTTP & API ===
      {:bandit, "~> 1.7"},
      {:finch, "~> 0.18"},
      {:open_api_spex, "~> 3.21"},
      {:cors_plug, "~> 3.0"},

      # === GraphQL ===
      {:absinthe, "~> 1.7"},
      {:absinthe_plug, "~> 1.5"},

      # === Utilities & Core Libraries ===
      {:gettext, "~> 0.24"},
      {:jason, "~> 1.4"},
      {:dns_cluster, "~> 0.1.1"},
      {:timex, "~> 3.0"},
      {:norm, "~> 0.13"},

      # === UI Components ===
      {:salad_ui, "~> 1.0.0-beta.3"},
      {:heroicons, "~> 0.5.0"},
      {:flop, "~> 0.26.0"},
      {:flop_phoenix, "~> 0.25.2"},

      # === Development Dependencies ===
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      {:live_debugger, "~> 0.3.0", only: :dev},
      {:phoenix_storybook, "~> 0.6.0", only: :dev},
      {:tailwind, "~> 0.2", runtime: Mix.env() == :dev},
      {:esbuild, "~> 0.8", runtime: Mix.env() == :dev},

      # === Test Dependencies ===
      {:stream_data, "~> 1.0"},
      {:mox, "~> 1.1", only: :test},
      {:phoenix_test, "~> 0.7.0", only: :test, runtime: false},
      {:excoveralls, "~> 0.18", only: :test},
      {:faker, "~> 0.18", only: [:test, :dev]},
      {:phoenix_test_playwright, "~> 0.7.0", only: :test, runtime: false},
      {:floki, ">= 0.36.0", only: :test},

      # === Code Quality & Analysis ===
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false},
      {:ex_doc, "~> 0.34", only: :dev, runtime: false},
      {:sobelow, "~> 0.13", only: [:dev, :test], runtime: false}
    ]
  end

  @spec aliases :: keyword()
  defp aliases do
    [
      # === I18n ===
      "i18n.extract": ["gettext.extract --merge"],
      "i18n.check": ["gettext.extract --merge --check-up-to-date"],
      # === Setup & Database ===
      setup: ["deps.get", "ecto.setup"],
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      "ecto.migrate": ["ecto.migrate", "ecto.dump"],
      "ecto.rollback": ["ecto.rollback"],

      # === Testing ===
      test: fn args ->
        env = System.get_env("SKIP_DB")
        IO.puts("[mix alias test] SKIP_DB=#{inspect(env)} args=#{inspect(args)}")

        truthy? =
          case env do
            "true" -> true
            "1" -> true
            "yes" -> true
            "on" -> true
            _ -> false
          end

        if truthy? do
          IO.puts("[mix alias test] DB setup skipped due to SKIP_DB")
          # Run tests without DB. Load a unit-only test helper if present to avoid Repo/Sandbox.
          unit_helper = Path.join(["test", "unit_test_helper.exs"])

          if File.exists?(unit_helper) do
            IO.puts("[mix alias test] loading #{unit_helper}")
            Code.require_file(unit_helper)
          end

          Mix.shell(Mix.Shell.IO)
          Mix.Task.reenable("test")
          Mix.Tasks.Test.run(args)
        else
          # Prepare DB quietly, then run the built-in test task (avoid alias recursion).
          Mix.Task.run("ecto.create", ["--quiet"])
          Mix.Task.run("ecto.migrate", ["--quiet"])
          Mix.shell(Mix.Shell.IO)
          Mix.Task.reenable("test")
          Mix.Tasks.Test.run(args)
        end
      end,

      # === Code Quality ===
      "credo.check": "credo --strict",
      "credo.suggest": "credo --strict --only",
      "credo.fix": "mix format && mix credo --strict --fix",
      "quality.check": ["format", "credo.check", "dialyzer.check"],
      "quality.quick": ["credo.check", "dialyzer.check"],
      "quality.full": ["format", "credo.check", "dialyzer.check", "sobelow.check", "test"],
      "dialyzer.check": "dialyzer",
      "sobelow.check": "sobelow --config",

      # === Assets ===
      "assets.deploy": [
        "tailwind default --minify",
        "tailwind storybook --minify",
        "esbuild default --minify",
        "phx.digest"
      ],
      "assets.setup": ["tailwind.install --if-missing", "esbuild.install --if-missing"],
      "assets.build": ["tailwind default", "esbuild default"],

      # === Documentation ===
      "docs.build": "mix docs",
      "docs.open": "mix docs",

      # === Release ===
      "release.clean": ["clean", "clean deps"],
      "release.test": ["ecto.create --quiet", "ecto.migrate --quiet", "test"],
      "release.prepare": ["ecto.create --quiet", "ecto.migrate --quiet", "assets.deploy"]
    ]
  end

  # Helper function to determine elixirc_paths based on environment
  # Single level of abstraction: Keep path determination simple and focused
  @spec elixirc_paths(atom()) :: [binary()]
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Package configuration for publishing
  # Type safety: Ensure proper package metadata
  @spec package :: keyword()
  defp package do
    [
      name: :riva_ash,
      description: "A comprehensive reservation management system built with Elixir, Phoenix, and Ash Framework",
      licenses: ["MIT"],
      links: %{
        "GitHub" => "https://github.com/your-org/riva_ash",
        "Documentation" => "https://hexdocs.pm/riva_ash",
        "Changelog" => "https://github.com/your-org/riva_ash/blob/main/CHANGELOG.md",
        "Issues" => "https://github.com/your-org/riva_ash/issues"
      },
      files: ~w(lib priv mix.exs README.md LICENSE CHANGELOG.md docs),
      maintainers: [
        "Your Name <your.email@example.com>",
        "Team Member <team.member@example.com>"
      ],
      build_embedded: Mix.env() == :prod,
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      source_url: "https://github.com/your-org/riva_ash",
      homepage_url: "https://github.com/your-org/riva_ash",
      docs: [
        main: "readme",
        extras: ["README.md", "docs/README.md", "docs/configuration.md", "docs/api.md"],
        groups_for_modules: [
          Ash: ~r/^Ash/,
          RivaAsh: ~r/^RivaAsh/,
          Web: ~r/^RivaAshWeb/,
          Auth: ~r/^RivaAsh.*Auth/,
          Components: ~r/^RivaAshWeb.*Component/,
          Live: ~r/^RivaAshWeb.*Live/,
          Resources: ~r/^RivaAsh\.Resources\./,
          Accounts: ~r/^RivaAsh\.Accounts\./
        ],
        nest_modules_by_prefix: [
          RivaAshWeb,
          RivaAsh.Resources,
          RivaAsh.Accounts
        ],
        source_ref: "main",
        output: "docs"
      ],
      # Hex.pm specific configuration
      source_url_pattern: "https://github.com/your-org/riva_ash/blob/main/%{path}#L%{line}",
      # Build tools
      compilers: Mix.compilers(),
      # Test coverage
      test_coverage: [tool: ExCoveralls],
      # Dialyzer configuration
      dialyzer: [
        plt_file: {:no_warn, "priv/plts/dialyzer.plt"},
        flags: [:error_handling, :underspecs, :unknown],
        ignore_warnings: ".dialyzer_ignore.exs",
        plt_add_apps: [:mix, :eex, :iex, :ex_unit, :ash, :phoenix],
        plt_core_path: "priv/plts",
        check_plt: true
      ],
      # Additional metadata
      keywords: [
        "elixir",
        "phoenix",
        "ash",
        "reservation",
        "booking",
        "api",
        "graphql",
        "json-api",
        "admin"
      ],
      # Requirements
      requirements: [
        {"elixir", "~> 1.18", ">= 1.18.0"},
        {"phoenix", "~> 1.7", ">= 1.7.0"},
        {"ash", "~> 3.5", ">= 3.5.0"}
      ]
    ]
  end
end
