defmodule RivaAshWeb.FeatureCase do
  @moduledoc """
  This module defines the test case to be used by
  feature tests that use PhoenixTest (with or without Playwright).

  Such tests rely on `PhoenixTest` for in-memory testing or with Playwright
  driver for real browser testing.

  For browser tests, the database sandbox is configured to allow concurrent
  access since the browser runs in a separate process.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      # Import conveniences for testing with connections
      use RivaAshWeb, :verified_routes

      import RivaAshWeb.FeatureCase
      import RivaAsh.TestHelpers

      # Standard Phoenix testing
      import Phoenix.ConnTest
      import Phoenix.LiveViewTest

      # Phoenix Test imports
      import PhoenixTest

      # Endpoint for Phoenix testing
      @endpoint RivaAshWeb.Endpoint
    end
  end

  setup tags do
    # Configure database sandbox
    pid = Ecto.Adapters.SQL.Sandbox.start_owner!(RivaAsh.Repo, shared: not tags[:async])
    on_exit(fn -> Ecto.Adapters.SQL.Sandbox.stop_owner(pid) end)

    # For Phoenix Test with Playwright browser tests, allow database access
    if tags[:playwright] do
      Ecto.Adapters.SQL.Sandbox.mode(RivaAsh.Repo, {:shared, self()})
    end

    # Return a conn for all tests (Phoenix Test handles browser automation internally)
    conn = Phoenix.ConnTest.build_conn()
    %{conn: conn}
  end
end
