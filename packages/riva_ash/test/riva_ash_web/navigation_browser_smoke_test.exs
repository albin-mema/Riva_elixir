defmodule RivaAshWeb.NavigationBrowserSmokeTest do
  @moduledoc """
  Real-browser smoke test using Phoenix Test + Playwright.
  Visits a handful of public routes and asserts no obvious server error text.
  Set PLAYWRIGHT_HEADLESS=false to see the browser window.
  """

  use RivaAshWeb.FeatureCase, async: false
  # Start Playwright-backed PhoenixTest session (real browser)
  use PhoenixTest.Playwright.Case
  import PhoenixTest

  alias RivaAsh.PropertyTesting.RouteEnumerator

  @moduletag browser: :chromium

  test "visit several public pages without server errors", %{conn: conn} do
    start_paths =
      RouteEnumerator.public_routes()
      |> Enum.reject(& &1.requires_params)
      |> Enum.map(& &1.path)
      |> Enum.uniq()

    # Make sure we include root if available
    start_paths = if "/" in start_paths, do: start_paths, else: ["/" | start_paths]

    # Pick 4 unique routes to visit (or fewer if not available)
    paths =
      start_paths
      |> Enum.shuffle()
      |> Enum.take(min(4, length(start_paths)))

    Enum.reduce(paths, conn, fn path, session ->
      session = visit(session, path)

      # minimal negative assertions
      refute_has(session, "text=Internal Server Error")
      refute_has(session, "text=Server Error")
      refute_has(session, "text=Exception")
      session
    end)
  end
end
