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

      # Import PhoenixTest for unified testing
      import PhoenixTest

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

    # Start any mocks needed for the tests
    Mox.stub(RivaAsh.Repo.Mock, :query, fn _, _, _ -> {:ok, %{rows: []}} end)

    # Set up default mocks for common Repo operations
    Mox.stub(RivaAsh.Repo.Mock, :all, fn _ -> [] end)
    Mox.stub(RivaAsh.Repo.Mock, :get, fn _, _ -> nil end)
    Mox.stub(RivaAsh.Repo.Mock, :get_by, fn _, _ -> nil end)
    Mox.stub(RivaAsh.Repo.Mock, :insert, fn _ -> {:ok, %{id: 1}} end)
    Mox.stub(RivaAsh.Repo.Mock, :update, fn _ -> {:ok, %{id: 1}} end)
    Mox.stub(RivaAsh.Repo.Mock, :delete, fn _ -> {:ok, %{id: 1}} end)

    {:ok, conn: Phoenix.ConnTest.build_conn()}
  end
end
