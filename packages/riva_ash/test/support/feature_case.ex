alias Ecto.Adapters.SQL.Sandbox, as: Sandbox
alias Phoenix.ConnTest, as: ConnTest

defmodule RivaAshWeb.FeatureCase do
  @moduledoc """
  This module defines the test case to be used by
  feature tests that use PhoenixTest.

  Such tests rely on `PhoenixTest` and also
  import other functionality to make it easier
  to build common data structures and query the data layer.

  Finally, if the test case interacts with the database,
  we enable the SQL sandbox, so changes done to the database
  are reverted at the end of every test. If you are using
  PostgreSQL, you can even run database tests asynchronously
  by setting `use RivaAshWeb.FeatureCase, async: true`, although
  this option is not recommended for other databases.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      # Import conveniences for testing with connections
      use RivaAshWeb, :verified_routes

      import RivaAshWeb.FeatureCase
      import RivaAsh.TestHelpers

      # Temporarily using standard Phoenix LiveView testing
      import Phoenix.ConnTest
      import Phoenix.LiveViewTest
    end
  end

  setup tags do
    pid = Ecto.Adapters.SQL.Sandbox.start_owner!(RivaAsh.Repo, shared: not tags[:async])
    on_exit(fn -> Ecto.Adapters.SQL.Sandbox.stop_owner(pid) end)

    %{conn: Phoenix.ConnTest.build_conn()}
  end
end
