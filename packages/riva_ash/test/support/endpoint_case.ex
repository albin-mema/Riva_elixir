defmodule RivaAshWeb.EndpointCase do
  @moduledoc """
  This module defines the test case to be used by
  tests that require setting up a connection.
  """

  use ExUnit.CaseTemplate
  import Phoenix.ConnTest

  using do
    quote do
      # Import conveniences for testing with connections
      import Phoenix.ConnTest
      import RivaAshWeb.ConnCase
      import RivaAshWeb.EndpointCase

      # The default endpoint for testing
      @endpoint RivaAshWeb.Endpoint
    end
  end

  setup tags do
    # Setup sandbox for database access
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(RivaAsh.Repo)

    unless tags[:async] do
      Ecto.Adapters.SQL.Sandbox.mode(RivaAsh.Repo, {:shared, self()})
    end

    # Build a connection for testing
    conn = Phoenix.ConnTest.build_conn()

    # Add any default headers or configurations
    conn =
      conn
      |> Plug.Conn.put_req_header("accept", "application/vnd.api+json")
      |> Plug.Conn.put_req_header("content-type", "application/vnd.api+json")
      |> Plug.Conn.put_private(:phoenix_recycled, true)

    {:ok, conn: conn}
  end
end
