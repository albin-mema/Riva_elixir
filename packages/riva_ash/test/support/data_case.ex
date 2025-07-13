defmodule RivaAsh.DataCase do
  @moduledoc """
  This module defines the setup for tests requiring
  access to the application's data layer.

  You may define functions here to be used as helpers in
  your tests.

  Finally, if the test case interacts with the database,
  we enable the SQL sandbox, so changes done to the database
  during the test are rolled back at the end of the test.
  If you are using PostgreSQL, you can even run database
  tests asynchronously by setting `use RivaAsh.DataCase,
  async: true`, although this option is not recommended for
  other databases.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      alias RivaAsh.Repo

      import Ecto
      import Ecto.Changeset
      import Ecto.Query
      import RivaAsh.DataCase
      import RivaAsh.Factory
      import RivaAsh.PropertyHelpers
      import RivaAsh.TestHelpers

      # Enable property-based testing
      use ExUnitProperties
    end
  end

  setup tags do
    RivaAsh.DataCase.setup_sandbox(tags)
    :ok
  end

  @doc """
  Sets up the sandbox based on the test tags.
  """
  def setup_sandbox(tags) do
    # Check if repo is using SQL Sandbox
    repo_config = RivaAsh.Repo.config()
    case repo_config[:pool] do
      Ecto.Adapters.SQL.Sandbox ->
        pid = Ecto.Adapters.SQL.Sandbox.start_owner!(RivaAsh.Repo, shared: not tags[:async])
        on_exit(fn -> Ecto.Adapters.SQL.Sandbox.stop_owner(pid) end)
      _ ->
        # Fallback for non-sandbox environments
        :ok = Ecto.Adapters.SQL.Sandbox.checkout(RivaAsh.Repo)
        on_exit(fn -> Ecto.Adapters.SQL.Sandbox.checkin(RivaAsh.Repo) end)
    end
  end

  @doc """
  A helper that transforms changeset errors into a map of messages.

      assert {:error, changeset} = Accounts.create_user(%{password: "short"})
      assert "password is too short" in errors_on(changeset).password
      assert %{password: ["password is too short"]} = errors_on(changeset)

  """
  def errors_on(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {message, opts} ->
      Regex.replace(~r"%{(\w+)}", message, fn _, key ->
        opts |> Keyword.get(String.to_existing_atom(key), key) |> to_string()
      end)
    end)
  end
end
