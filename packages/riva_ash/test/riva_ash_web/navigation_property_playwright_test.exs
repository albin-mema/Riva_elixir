defmodule RivaAshWeb.NavigationPropertyPlaywrightTest do
  @moduledoc """
  Real-browser route smoke testing with PhoenixTest + Playwright.

  Improvements:
  - Cover public, authenticated, and admin parameterless GET routes
  - Property test samples across all categories
  - Deterministic enumerator test reports all crashing pages with their paths
  """

  use RivaAshWeb.FeatureCase, async: false
  # Start Playwright-backed PhoenixTest session
  use PhoenixTest.Playwright.Case
  use ExUnitProperties
  import PhoenixTest, except: [check: 2]

  import StreamData
  alias RivaAsh.PropertyTesting.RouteEnumerator

  @moduletag browser: :chromium

  # --- Helpers

  # Collect all parameterless GET routes across public/authenticated/admin
  defp parameterless_paths do
    routes = RouteEnumerator.enumerate_routes()

    [:public, :authenticated, :admin]
    |> Enum.flat_map(fn cat -> Map.get(routes, cat, []) end)
    |> Enum.filter(&(not &1.requires_params and &1.verb == :get))
    |> Enum.map(& &1.path)
    |> Enum.uniq()
  end

  # Log in as an admin using real browser interactions so cookies are set in Playwright
  defp ensure_admin_logged_in(session) do
    admin = create_user!(%{role: :admin, password: "password123"})

    session
    |> visit("/sign-in")
    # Use exact labels from the sign-in form
    |> fill_in("Email address", with: to_string(admin.email))
    |> fill_in("Password", with: "password123")
    |> click_button("Sign In")
  end

  defp visit_and_check(session, path) do
    try do
      session = visit(session, path)
      # Negative assertions for obvious server errors
      refute_has(session, ~s(text="Internal Server Error"))
      refute_has(session, ~s(text="Server Error"))
      refute_has(session, ~s(text="Exception"))
      {:ok, session}
    rescue
      e -> {:error, Exception.message(e)}
    end
  end

  # --- Deterministic enumerator: reports ALL crashing pages in one run
  @tag :browser
  test "enumerates all parameterless GET routes and reports crashing pages", %{conn: conn} do
    # Start from unauthenticated session, then promote to admin once to unlock authed/admin pages
    session = conn |> ensure_admin_logged_in()

    # Build set of paths and tack on Storybook examples if present
    storybook = ["/storybook", "/storybook/ui/button"]
    paths =
      parameterless_paths()
      |> Enum.concat(storybook)
      |> Enum.uniq()

    {_, failures} =
      paths
      |> Enum.sort() # stable order
      |> Enum.reduce({session, []}, fn path, {sess, fails} ->
        case visit_and_check(sess, path) do
          {:ok, new_sess} -> {new_sess, fails}
          {:error, reason} -> {sess, [{path, reason} | fails]}
        end
      end)

    if failures != [] do
      IO.puts("\n❌ Crashing pages detected:")
      failures
      |> Enum.reverse()
      |> Enum.each(fn {path, reason} ->
        IO.puts("  #{path} -> #{reason}")
      end)
    end

    assert failures == [],
           "Some routes crashed in the browser. See failures above. Count: #{length(failures)}"
  end

  # --- Property: sample random sequences
  property "random parameterless GET pages render without server errors", %{conn: conn} do
    # Log in as admin once so both public and protected routes are accessible
    session = conn |> ensure_admin_logged_in()

    paths =
      parameterless_paths()
      |> then(fn ps -> if "/" in ps, do: ps, else: ["/" | ps] end)

    runs =
      case System.get_env("NAV_PROP_MAX_RUNS") do
        nil -> min(30, max(length(paths), 1))
        str -> case Integer.parse(str) do
          {n, _} when n > 0 -> n
          _ -> min(10, max(length(paths), 1))
        end
      end

    check all(
            sample <- list_of(member_of(paths), length: 3..6),
            max_runs: runs
          ) do
      Enum.reduce(sample, session, fn path, sess ->
        case visit_and_check(sess, path) do
          {:ok, new_sess} -> new_sess
          {:error, reason} -> flunk("Crash on #{path}: #{reason}")
        end
      end)
    end
  end
end
