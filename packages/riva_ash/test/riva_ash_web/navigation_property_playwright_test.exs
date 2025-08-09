defmodule RivaAshWeb.NavigationPropertyPlaywrightTest do
  @moduledoc """
  Real-browser property test using Phoenix Test + Playwright.

  It randomly visits several public, parameterless routes to ensure pages render
  without server errors. This opens a visible browser when PLAYWRIGHT_HEADLESS=false.
  """

  use RivaAshWeb.FeatureCase, async: false
  # Start Playwright-backed PhoenixTest session
  use PhoenixTest.Playwright.Case
  use ExUnitProperties
  import PhoenixTest, except: [check: 2]

  import StreamData
  alias RivaAsh.PropertyTesting.RouteEnumerator

  @moduletag browser: :chromium

  defp public_start_paths do
    RouteEnumerator.public_routes()
    |> Enum.reject(& &1.requires_params)
    |> Enum.map(& &1.path)
    |> Enum.uniq()
  end

  property "public pages render without server errors in a real browser", %{conn: conn} do
    start_paths = public_start_paths()

    # Ensure we have at least the root path
    start_paths = if "/" in start_paths, do: start_paths, else: ["/" | start_paths]

    check all(
            # Pick 3..5 routes to visit in sequence
            paths <- list_of(member_of(start_paths), length: 3..5),
            max_runs: min(5, max(length(start_paths), 1))
          ) do
      # Visit each route in the real browser
      Enum.reduce(paths, conn, fn path, session ->
        session = visit(session, path)
        # Lightweight smoke assertions
        refute_has(session, ~s(text="Internal Server Error"))
        refute_has(session, ~s(text="Server Error"))
        refute_has(session, ~s(text="Exception"))
        session
      end)
    end
  end
end
