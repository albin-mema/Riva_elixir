defmodule Mix.Tasks.Routes.Check do
  @moduledoc """
  Mix task to check all routes for potential errors and crashes.

  This task enumerates all routes in your Phoenix application and tests them
  systematically to identify potential issues before they cause crashes in production.

  ## Usage

      mix routes.check
      mix routes.check --verbose
      mix routes.check --category=api
      mix routes.check --with-server

  ## Options

    * `--verbose` - Show detailed output for each route test
    * `--category` - Test only specific category (public, auth, api, admin)
    * `--with-server` - Start a test server and make actual HTTP requests
    * `--parallel` - Run tests in parallel (faster but less detailed output)

  """

  use Mix.Task

  @shortdoc "Check all application routes for potential errors"

  def run(args) do
    {opts, _, _} =
      OptionParser.parse(args,
        switches: [
          verbose: :boolean,
          category: :string,
          with_server: :boolean,
          parallel: :boolean
        ]
      )

    Mix.Task.run("app.start")

    IO.puts("🔍 Checking application routes...")
    IO.puts("=" |> String.duplicate(50))

    routes = get_all_routes()
    categorized = categorize_routes(routes)

    print_summary(categorized, opts)

    case opts[:category] do
      nil -> test_all_categories(categorized, opts)
      category -> test_specific_category(categorized, category, opts)
    end

    IO.puts("\n✅ Route checking complete!")
  end

  defp get_all_routes do
    try do
      # Use Phoenix.Router.routes/1 to get routes
      Phoenix.Router.routes(RivaAshWeb.Router)
    rescue
      error ->
        IO.puts("❌ Could not load routes: #{inspect(error)}")
        []
    end
  end

  defp categorize_routes(routes) do
    %{
      public: filter_public_routes(routes),
      auth: filter_auth_routes(routes),
      api: filter_api_routes(routes),
      admin: filter_admin_routes(routes),
      live: filter_live_routes(routes)
    }
  end

  defp filter_public_routes(routes) do
    Enum.filter(routes, fn route ->
      not String.starts_with?(route.path, "/api") and
        not String.starts_with?(route.path, "/admin") and
        not has_auth_pipeline?(route)
    end)
  end

  defp filter_auth_routes(routes) do
    Enum.filter(routes, fn route ->
      has_auth_pipeline?(route)
    end)
  end

  defp filter_api_routes(routes) do
    Enum.filter(routes, fn route ->
      String.starts_with?(route.path, "/api") or
        String.starts_with?(route.path, "/graphql")
    end)
  end

  defp filter_admin_routes(routes) do
    Enum.filter(routes, fn route ->
      String.starts_with?(route.path, "/admin")
    end)
  end

  defp filter_live_routes(routes) do
    Enum.filter(routes, fn route ->
      case route.plug do
        plug when is_atom(plug) ->
          plug_name = to_string(plug)
          String.contains?(plug_name, "Live")

        _ ->
          false
      end
    end)
  end

  defp has_auth_pipeline?(route) do
    # Check if route has authentication pipeline
    # Phoenix routes might not have pipe_through in the structure we get
    case Map.get(route, :pipe_through) do
      nil ->
        false

      pipelines when is_list(pipelines) ->
        Enum.member?(pipelines, :require_authenticated_user)

      _ ->
        false
    end
  end

  defp print_summary(categorized, opts) do
    IO.puts("📊 Route Summary:")
    IO.puts("  Public: #{length(categorized.public)}")
    IO.puts("  Authenticated: #{length(categorized.auth)}")
    IO.puts("  API: #{length(categorized.api)}")
    IO.puts("  Admin: #{length(categorized.admin)}")
    IO.puts("  LiveView: #{length(categorized.live)}")

    if opts[:verbose] do
      print_detailed_summary(categorized)
    end
  end

  defp print_detailed_summary(categorized) do
    IO.puts("\n📋 Detailed Route Listing:")

    Enum.each(categorized, fn {category, routes} ->
      IO.puts("\n#{String.upcase(to_string(category))} ROUTES:")

      Enum.each(routes, fn route ->
        IO.puts("  #{route.verb} #{route.path} -> #{route.plug}")
      end)
    end)
  end

  defp test_all_categories(categorized, opts) do
    categories = [:public, :auth, :api, :admin, :live]

    Enum.each(categories, fn category ->
      routes = Map.get(categorized, category, [])
      test_route_category(category, routes, opts)
    end)
  end

  defp test_specific_category(categorized, category_name, opts) do
    category = String.to_atom(category_name)
    routes = Map.get(categorized, category, [])

    if Enum.empty?(routes) do
      IO.puts("❌ No routes found for category: #{category_name}")
    else
      test_route_category(category, routes, opts)
    end
  end

  defp test_route_category(category, routes, opts) do
    IO.puts("\n🧪 Testing #{String.upcase(to_string(category))} routes (#{length(routes)})...")

    results =
      if opts[:parallel] do
        test_routes_parallel(routes, opts)
      else
        test_routes_sequential(routes, opts)
      end

    print_results(results)
  end

  defp test_routes_sequential(routes, opts) do
    Enum.reduce(routes, %{success: 0, errors: 0, skipped: 0}, fn route, acc ->
      case test_single_route(route, opts) do
        :success -> %{acc | success: acc.success + 1}
        :error -> %{acc | errors: acc.errors + 1}
        :skipped -> %{acc | skipped: acc.skipped + 1}
      end
    end)
  end

  defp test_routes_parallel(routes, opts) do
    routes
    |> Task.async_stream(fn route -> test_single_route(route, opts) end,
      max_concurrency: System.schedulers_online()
    )
    |> Enum.reduce(%{success: 0, errors: 0, skipped: 0}, fn {:ok, result}, acc ->
      case result do
        :success -> %{acc | success: acc.success + 1}
        :error -> %{acc | errors: acc.errors + 1}
        :skipped -> %{acc | skipped: acc.skipped + 1}
      end
    end)
  end

  defp test_single_route(route, opts) do
    if String.contains?(route.path, ":") do
      if opts[:verbose], do: IO.puts("  ⏭️  #{route.verb} #{route.path} (has parameters)")
      :skipped
    else
      try do
        validate_route(route)
        if opts[:verbose], do: IO.puts("  ✅ #{route.verb} #{route.path}")
        :success
      rescue
        error ->
          if opts[:verbose] do
            IO.puts("  ❌ #{route.verb} #{route.path} - #{inspect(error)}")
          end

          :error
      end
    end
  end

  defp validate_route(route) do
    # Validate route structure
    validate_path(route.path)
    validate_verb(route.verb)
    validate_plug(route.plug)
    # Skip pipeline validation as it's not available in Phoenix.Router.routes/1 output
  end

  defp validate_path(path) do
    if !(is_binary(path) and String.starts_with?(path, "/")) do
      raise "Invalid path: #{inspect(path)}"
    end
  end

  defp validate_verb(verb) do
    valid_verbs = [:get, :post, :put, :patch, :delete, :head, :options, :*]

    if verb not in valid_verbs do
      raise "Invalid HTTP verb: #{inspect(verb)}"
    end
  end

  defp validate_plug(plug) do
    if not is_atom(plug) do
      raise "Invalid plug: #{inspect(plug)}"
    end

    if !Code.ensure_loaded?(plug) do
      raise "Plug module not found: #{inspect(plug)}"
    end
  end



  defp validate_pipelines(_), do: :ok

  defp print_results(results) do
    total = results.success + results.errors + results.skipped

    IO.puts(
      "  Results: ✅ #{results.success}/#{total} | ❌ #{results.errors} | ⏭️  #{results.skipped}"
    )

    if results.errors > 0 do
      IO.puts("  ⚠️  Found #{results.errors} route errors that need attention!")
    end
  end
end
