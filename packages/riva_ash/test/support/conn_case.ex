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

      # Helper imports for convenience
      import RivaAsh.Test.TimeHelpers
      import RivaAsh.Test.JsonApiHelpers

      alias RivaAshWeb.Router.Helpers, as: Routes

      # The default endpoint for testing
      @endpoint RivaAshWeb.Endpoint
    end
  end

  setup tags do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(RivaAsh.Repo)

    if tags[:shared_db] do
      Ecto.Adapters.SQL.Sandbox.mode(RivaAsh.Repo, {:shared, self()})
    else
      unless tags[:async] do
        Ecto.Adapters.SQL.Sandbox.mode(RivaAsh.Repo, {:shared, self()})
      end
    end

    {:ok, conn: Phoenix.ConnTest.build_conn()}
  end
end
