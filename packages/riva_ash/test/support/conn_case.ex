defmodule RivaAshWeb.ConnCase do
  @moduledoc """
  This module defines the test case to be used by
  tests that require setting up a connection.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      # Import conveniences for testing with connections
      import Plug.Conn
      import Phoenix.ConnTest
      import RivaAshWeb.ConnCase
      import RivaAsh.TestHelpers

      # PhoenixTest disabled - not available in current setup
      # import PhoenixTest

      alias RivaAshWeb.Router.Helpers, as: Routes

      # The default endpoint for testing
      @endpoint RivaAshWeb.Endpoint
    end
  end

  setup tags do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(RivaAsh.Repo)

    unless tags[:async] do
      Ecto.Adapters.SQL.Sandbox.mode(RivaAsh.Repo, {:shared, self()})
    end

    # Skip mock setup for now - using real repo in sandbox mode

    conn = Phoenix.ConnTest.build_conn()
  end
end
