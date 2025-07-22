#!/usr/bin/env elixir

# Test runner script for RivaAsh
# Usage: mix run scripts/test_runner.exs [options]

Mix.install([
  {:ex_unit, "~> 1.15"},
  {:excoveralls, "~> 0.18"},
  {:stream_data, "~> 1.0"},
  {:credo, "~> 1.7"},
  {:dialyzer, "~> 1.4"}
])

defmodule RivaAsh.TestRunner do
  @moduledoc """
  Comprehensive test runner for RivaAsh application.
  """

  @test_modules [
    "test/riva_ash/authorization_test.exs",
    "test/riva_ash/availability_test.exs",
    "test/riva_ash/booking_test.exs",
    "test/riva_ash/changes_test.exs",
    "test/riva_ash/database_health_test.exs",
    "test/riva_ash/error_helpers_test.exs",
    "test/riva_ash/mermaid_test.exs",
    "test/riva_ash/permissions_test.exs",
    "test/riva_ash/queries_test.exs",
    "test/riva_ash/recurring_reservations_test.exs",
    "test/riva_ash/release_test.exs",
    "test/riva_ash/resource_helpers_test.exs",
    "test/riva_ash/validations_test.exs"
  ]

  @integration_tests [
    "test/integration/booking_flow_test.exs",
    "test/integration/availability_flow_test.exs",
    "test/integration/recurring_reservations_flow_test.exs",
    "test/integration/authorization_flow_test.exs"
  ]

  @property_tests [
    "test/property/validations_property_test.exs",
    "test/property/booking_property_test.exs",
    "test/property/availability_property_test.exs"
  ]

  def main(args) do
    {opts, _, _} = OptionParser.parse(args,
      switches: [
        coverage: :boolean,
        integration: :boolean,
        property: :boolean,
        all: :boolean,
        fast: :boolean,
        slow: :boolean,
        watch: :boolean,
        parallel: :integer,
        seed: :integer,
        trace: :boolean,
        verbose: :boolean,
        help: :boolean
      ],
      aliases: [
        c: :coverage,
        i: :integration,
        p: :property,
        a: :all,
        f: :fast,
        s: :slow,
        w: :watch,
        h: :help
      ]
    )

    if opts[:help] do
      print_help()
      System.halt(0)
    end

    setup_environment(opts)
    run_tests(opts)
  end

  defp print_help do
    IO.puts("""
    RivaAsh Test Runner

    Usage: mix run scripts/test_runner.exs [options]

    Options:
      -c, --coverage      Run tests with coverage report
      -i, --integration   Run integration tests
      -p, --property      Run property-based tests
      -a, --all           Run all tests (unit + integration + property)
      -f, --fast          Run only fast tests
      -s, --slow          Run only slow tests
      -w, --watch         Run tests in watch mode
      --parallel N        Run tests with N parallel processes
      --seed N            Set random seed for reproducible tests
      --trace             Show detailed test execution
      --verbose           Verbose output
      -h, --help          Show this help message

    Examples:
      mix run scripts/test_runner.exs --coverage
      mix run scripts/test_runner.exs --all --parallel 4
      mix run scripts/test_runner.exs --integration --verbose
    """)
  end

  defp setup_environment(opts) do
    # Set up test environment
    System.put_env("MIX_ENV", "test")

    if opts[:coverage] do
      System.put_env("COVERAGE", "true")
    end

    if opts[:trace] do
      System.put_env("TRACE", "true")
    end

    if opts[:verbose] do
      System.put_env("VERBOSE", "true")
    end

    # Configure ExUnit
    ExUnit.configure(
      formatters: [ExUnit.CLIFormatter],
      exclude: [],
      include: []
    )

    # Set parallel execution
    if parallel = opts[:parallel] do
      ExUnit.configure(max_cases: parallel)
    end

    # Set random seed
    if seed = opts[:seed] do
      ExUnit.configure(seed: seed)
    end

    # Start required applications
    {:ok, _} = Application.ensure_all_started(:riva_ash)
  end

  defp run_tests(opts) do
    IO.puts("Starting RivaAsh test suite...")
    IO.puts("Options: #{inspect(opts)}")

    test_files = determine_test_files(opts)

    if Enum.empty?(test_files) do
      IO.puts("No tests to run based on provided options")
      System.halt(0)
    end

    # Run tests
    case ExUnit.run() do
      %{failures: 0} ->
        IO.puts("All tests passed!")
        if opts[:coverage], do: generate_coverage_report()
        System.halt(0)

      %{failures: failures} ->
        IO.puts("Tests failed: #{failures} failures")
        System.halt(1)
    end
  end

  defp determine_test_files(opts) do
    cond do
      opts[:all] -> @test_modules ++ @integration_tests ++ @property_tests
      opts[:integration] -> @integration_tests
      opts[:property] -> @property_tests
      opts[:fast] -> @test_modules
      opts[:slow] -> @integration_tests ++ @property_tests
      true -> @test_modules
    end
  end

  defp generate_coverage_report do
    IO.puts("Generating coverage report...")

    # Run coverage analysis
    System.cmd("mix", ["coveralls.html"], into: IO.stream(:stdio, :line))

    # Open coverage report
    if File.exists?("cover/excoveralls.html") do
      IO.puts("Coverage report generated at cover/excoveralls.html")

      case System.cmd("which", ["open"]) do
        {_, 0} -> System.cmd("open", ["cover/excoveralls.html"])
        _ -> :ok
      end
    end
  end

  defp run_watch_mode do
    IO.puts("Starting test watch mode...")

    # Use file system watcher to re-run tests on file changes
    case System.cmd("which", ["fswatch"]) do
      {_, 0} ->
        System.cmd("fswatch", ["-o", "lib", "test"], into: IO.stream(:stdio, :line))
        run_tests([])

      _ ->
        IO.puts("fswatch not found. Install with: brew install fswatch")
        System.halt(1)
    end
  end
end

# Run the test runner
RivaAsh.TestRunner.main(System.argv())
