defmodule RivaAsh.TestHelpers do
  @moduledoc """
  Helper functions for testing.

  This module provides common test helpers for setting up test data,
  making requests, and asserting responses in a consistent way.

  ## Usage

  Add this to your test module:

      use RivaAsh.TestHelpers

  Or import specific functions:

      import RivaAsh.TestHelpers, only: [create_item: 1]
  """

  import ExUnit.Assertions
  import Phoenix.ConnTest
  import Plug.Conn
  defmacro __using__(_opts) do
    quote do
      import ExUnit.Assertions
      import Plug.Conn
      import Phoenix.ConnTest

      alias RivaAsh.Resources.Item
      alias RivaAsh.Accounts.User
      alias RivaAsh.Domain
      alias RivaAsh.Accounts
      alias Plug.Conn

      # Setup the SQL sandbox for tests.
      setup tags do
        # The sandbox is already started in test_helper.exs, so we just need to set the mode.
        unless tags[:async] do
          Ecto.Adapters.SQL.Sandbox.mode(RivaAsh.Repo, {:shared, self()})
        end

        :ok
      end
    end
  end


  @doc """
  Run a function in a sandbox transaction.

  This is useful for isolating test data within a test case.
  """
  @spec sandboxed((-> any())) :: any()
  def sandboxed(fun) when is_function(fun, 0) do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(RivaAsh.Repo)
    fun.()
  end

  @doc """
  Create a test item with the given attributes.

  ## Examples

      # Create with default name
      {:ok, item} = create_item()

      # Create with custom attributes
      {:ok, item} = create_item(%{name: "Custom Name"})

  """
  @spec create_item(map()) :: {:ok, Item.t()} | {:error, any()}
  def create_item(attrs) when is_map(attrs) do
    defaults = %{
      name: "Test Item #{System.unique_integer([:positive, :monotonic])}"
    }

    attrs = Map.merge(defaults, attrs)
    Ash.create(Item, attrs, domain: Domain)
  end

  @doc """
  Create a test item with default attributes.
  """
  @spec create_item() :: {:ok, Item.t()} | {:error, any()}
  def create_item do
    create_item(%{})
  end

  @doc """
  Create a test item and return it, raising on error.

  ## Examples

      # Create with default name
      item = create_item!()

      # Create with custom attributes
      item = create_item!(%{name: "Custom Name"})

  """
  @spec create_item!(map()) :: Item.t()
  def create_item!(attrs) when is_map(attrs) do
    case create_item(attrs) do
      {:ok, item} -> item
      {:error, error} -> raise "Failed to create test item: #{inspect(error)}"
    end
  end

  @doc """
  Create a test item with default attributes and return it, raising on error.
  """
  @spec create_item!() :: Item.t()
  def create_item! do
    create_item!(%{})
  end

  @doc """
  Recursively assert that all key-value pairs in expected exist in actual.

  This is more flexible than a direct comparison as it allows for partial matches.
  """
  @spec assert_maps_match(any(), any(), [String.t()]) :: :ok | no_return()
  def assert_maps_match(expected, actual, path \\ []) do
    cond do
      is_map(expected) and is_map(actual) ->
        Enum.each(expected, fn {key, expected_value} ->
          assert Map.has_key?(actual, key),
                 "Expected key `#{key}` not found in #{inspect(actual)} at path #{inspect(path)}"

          assert_maps_match(
            expected_value,
            actual[key] || actual[to_string(key)],
            path ++ [to_string(key)]
          )
        end)

      is_list(expected) and is_list(actual) ->
        assert length(expected) == length(actual),
               "Expected list of length #{length(expected)} but got #{length(actual)} at path #{inspect(path)}"

        Enum.zip(expected, actual)
        |> Enum.with_index()
        |> Enum.each(fn {{expected_item, actual_item}, index} ->
          assert_maps_match(expected_item, actual_item, path ++ ["[#{index}]"])
        end)

      true ->
        assert expected == actual,
               "Expected #{inspect(expected)} but got #{inspect(actual)} at path #{inspect(path)}"
    end

    :ok
  end

  @doc """
  Create a test user with the given attributes.

  ## Examples

      # Create with default attributes
      {:ok, user} = create_user()

      # Create with custom attributes
      {:ok, user} = create_user(%{email: "test@example.com", role: :admin})

  """
  @spec create_user(map()) :: {:ok, User.t()} | {:error, any()}
  def create_user(attrs \\ %{}) do
    defaults = %{
      email: "test_#{System.unique_integer([:positive, :monotonic])}@example.com",
      password: "password123",
      name: "Test User",
      role: :admin
    }

    attrs = Map.merge(defaults, attrs)

    User
    |> Ash.Changeset.for_create(:register_with_password, attrs)
    |> Ash.create(domain: RivaAsh.Accounts)
  end

  @doc """
  Create a test user and return it, raising on error.
  """
  @spec create_user!(map()) :: User.t()
  def create_user!(attrs \\ %{}) do
    case create_user(attrs) do
      {:ok, user} -> user
      {:error, error} -> raise "Failed to create test user: #{inspect(error)}"
    end
  end

  @doc """
  Sign in a user for testing by setting up the session and assigns.

  ## Examples

      conn = build_conn() |> sign_in_user(user)

  """
  @spec sign_in_user(Conn.t(), User.t()) :: Conn.t()
  def sign_in_user(conn, user) do
    token = Phoenix.Token.sign(RivaAshWeb.Endpoint, "user_auth", user.id)

    conn
    |> Plug.Test.init_test_session(%{})
    |> put_session(:user_token, token)
    |> assign(:current_user, user)
  end

  @doc """
  Create a user and sign them in for testing.

  ## Examples

      {conn, user} = build_conn() |> create_and_sign_in_user()
      {conn, user} = build_conn() |> create_and_sign_in_user(%{role: :admin})

  """
  @spec create_and_sign_in_user(Conn.t(), map()) :: {Conn.t(), User.t()}
  def create_and_sign_in_user(conn, attrs \\ %{}) do
    user = create_user!(attrs)
    conn = sign_in_user(conn, user)
    {conn, user}
  end

  @doc """
  Create a test business with the given attributes and actor.

  ## Examples

      # Create with default attributes and admin user
      user = create_user!(%{role: :admin})
      {:ok, business} = create_business(%{name: "Test Business"}, user)

  """
  @spec create_business(map(), User.t()) :: {:ok, any()} | {:error, any()}
  def create_business(attrs \\ %{}, actor) do
    defaults = %{
      name: "Test Business #{System.unique_integer([:positive, :monotonic])}",
      description: "A test business",
      owner_id: actor.id
    }

    attrs = Map.merge(defaults, attrs)

    RivaAsh.Resources.Business
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.create(actor: actor, domain: RivaAsh.Domain)
  end

  @doc """
  Create a test business and return it, raising on error.
  """
  @spec create_business!(map(), User.t()) :: any()
  def create_business!(attrs \\ %{}, actor) do
    case create_business(attrs, actor) do
      {:ok, business} -> business
      {:error, error} -> raise "Failed to create test business: #{inspect(error)}"
    end
  end

  @doc """
  Generate a unique string for testing.

  This is useful for creating unique values in tests to avoid conflicts.

  ## Examples

      # Generate a unique string
      unique = unique_string()

      # With a prefix
      email = unique_string("user_") <> "@example.com"

  """
  @spec unique_string(String.t()) :: String.t()
  def unique_string(prefix) when is_binary(prefix) do
    "#{prefix}#{System.unique_integer([:positive, :monotonic])}"
  end

  @doc """
  Generate a unique string with no prefix.
  """
  @spec unique_string() :: String.t()
  def unique_string do
    unique_string("")
  end
end
