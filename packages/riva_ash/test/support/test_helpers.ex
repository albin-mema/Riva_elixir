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
  require Ash.Query
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
  @spec create_user(map()) :: {:ok, RivaAsh.Accounts.User.t()} | {:error, any()}
  def create_user(attrs \\ %{}) do
    defaults = %{
      email: "test_#{System.unique_integer([:positive, :monotonic])}@example.com",
      password: "password123",
      name: "Test User",
      role: "admin"
    }

    attrs = Map.merge(defaults, attrs)

    RivaAsh.Accounts.User
    |> Ash.Changeset.for_create(:register_with_password, attrs)
    |> Ash.create(domain: RivaAsh.Accounts)
  end

  @doc """
  Create a test user and return it, raising on error.
  """
  @spec create_user!(map()) :: RivaAsh.Accounts.User.t()
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
  @spec sign_in_user(Conn.t(), RivaAsh.Accounts.User.t()) :: Conn.t()
  def sign_in_user(conn, user) do
    token = Phoenix.Token.sign(RivaAshWeb.Endpoint, "user_auth", user.id)

    conn
    |> Plug.Test.init_test_session(%{"user_token" => token})
    |> assign(:current_user, user)
  end

  @doc """
  Create a user and sign them in for testing.

  ## Examples

      {conn, user} = build_conn() |> create_and_sign_in_user()
      {conn, user} = build_conn() |> create_and_sign_in_user(%{role: :admin})

  """
  @spec create_and_sign_in_user(Conn.t(), map()) :: {Conn.t(), RivaAsh.Accounts.User.t()}
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
  @spec create_business(map(), RivaAsh.Accounts.User.t()) :: {:ok, any()} | {:error, any()}
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
  @spec create_business!(map(), RivaAsh.Accounts.User.t()) :: any()
  def create_business!(attrs \\ %{}, actor) do
    case create_business(attrs, actor) do
      {:ok, business} -> business
      {:error, error} -> raise "Failed to create test business: #{inspect(error)}"
    end
  end

  @doc """
  Assign a user token to a connection for authentication in tests.

  This is equivalent to sign_in_user but with a different name for compatibility.
  """
  @spec assign_user_token(Conn.t(), RivaAsh.Accounts.User.t()) :: Conn.t()
  def assign_user_token(conn, user) do
    sign_in_user(conn, user)
  end

  @doc """
  Create a test section with the given attributes and business context.
  """
  @spec create_section(map(), any()) :: {:ok, any()} | {:error, any()}
  def create_section(attrs \\ %{}, business) do
    create_section(attrs, business, nil)
  end

  @doc """
  Create a test section with the given attributes, business context, and actor.
  """
  @spec create_section(map(), any(), any()) :: {:ok, any()} | {:error, any()}
  def create_section(attrs, business, actor) do

    # First create a plot for the business if not provided
    plot = case Map.get(attrs, :plot_id) do
      nil ->
        plot_attrs = %{
          name: "Test Plot #{System.unique_integer([:positive, :monotonic])}",
          description: "A test plot",
          business_id: business.id
        }
        case RivaAsh.Resources.Plot
             |> Ash.Changeset.for_create(:create, plot_attrs)
             |> Ash.create(actor: actor, domain: RivaAsh.Domain) do
          {:ok, plot} -> plot
          {:error, error} -> raise "Failed to create test plot: #{inspect(error)}"
        end
      _plot_id -> nil
    end

    defaults = %{
      name: "Test Section #{System.unique_integer([:positive, :monotonic])}",
      description: "A test section",
      plot_id: plot && plot.id || attrs[:plot_id]
    }

    attrs = Map.merge(defaults, attrs)

    RivaAsh.Resources.Section
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.create(actor: actor, domain: RivaAsh.Domain)
  end

  @doc """
  Create a test section and return it, raising on error.
  """
  @spec create_section!(any()) :: any()
  def create_section!(business) do
    create_section!(business, nil)
  end

  @doc """
  Create a test section with actor and return it, raising on error.
  """
  @spec create_section!(any(), any()) :: any()
  def create_section!(business, actor) do
    case create_section(%{}, business, actor) do
      {:ok, section} -> section
      {:error, error} -> raise "Failed to create test section: #{inspect(error)}"
    end
  end

  @doc """
  Create a test item with the given section context.
  This overrides the previous create_item! function to work with sections.
  """
  @spec create_item!(any()) :: any()
  def create_item!(section) when is_map(section) do
    # Load the section with its plot to get business_id
    import Ash.Expr

    section_with_plot = RivaAsh.Resources.Section
    |> Ash.Query.load(:plot)
    |> Ash.Query.filter(expr(id == ^section.id))
    |> Ash.read_one!(domain: RivaAsh.Domain)

    business_id = section_with_plot.plot.business_id

    item_type_attrs = %{
      name: "Test Item Type #{System.unique_integer([:positive, :monotonic])}",
      description: "A test item type",
      business_id: business_id,
      default_capacity: 1
    }

    {:ok, item_type} = RivaAsh.Resources.ItemType
    |> Ash.Changeset.for_create(:create, item_type_attrs)
    |> Ash.create(domain: RivaAsh.Domain)

    item_attrs = %{
      name: "Test Item #{System.unique_integer([:positive, :monotonic])}",
      description: "A test item",
      section_id: section.id,
      item_type_id: item_type.id,
      capacity: 1
    }

    case RivaAsh.Resources.Item.create(item_attrs) do
      {:ok, item} -> item
      {:error, error} -> raise "Failed to create test item: #{inspect(error)}"
    end
  end

  # Fallback for the original create_item! function with attributes
  def create_item!(attrs) when is_map(attrs) do
    case create_item(attrs) do
      {:ok, item} -> item
      {:error, error} -> raise "Failed to create test item: #{inspect(error)}"
    end
  end

  @doc """
  Create a test recurring reservation with the given item and user.
  """
  @spec create_recurring_reservation!(any(), RivaAsh.Accounts.User.t()) :: any()
  def create_recurring_reservation!(item, user) do
    import Ash.Expr

    # Load the item with its relationships to get business_id
    item_with_relations = RivaAsh.Resources.Item
    |> Ash.Query.load(section: :plot)
    |> Ash.Query.filter(expr(id == ^item.id))
    |> Ash.read_one!(domain: RivaAsh.Domain)

    business_id = item_with_relations.section.plot.business_id

    # Create a client first
    client_attrs = %{
      name: "Test Client #{System.unique_integer([:positive, :monotonic])}",
      email: "client#{System.unique_integer([:positive, :monotonic])}@example.com",
      phone: "555-0123",
      business_id: business_id
    }

    {:ok, client} = RivaAsh.Resources.Client
    |> Ash.Changeset.for_create(:create, client_attrs)
    |> Ash.create(actor: user, domain: RivaAsh.Domain)

    recurring_reservation_attrs = %{
      client_id: client.id,
      item_id: item.id,
      start_date: Date.utc_today(),
      start_time: ~T[09:00:00],
      end_time: ~T[10:00:00],
      consecutive_days: 5,
      pattern_type: :daily,
      title: "Test Recurring Reservation",
      notes: "Test notes"
    }

    case RivaAsh.Resources.RecurringReservation.create(recurring_reservation_attrs) do
      {:ok, recurring_reservation} -> recurring_reservation
      {:error, error} -> raise "Failed to create test recurring reservation: #{inspect(error)}"
    end
  end

  @doc """
  Create a test recurring reservation instance with the given recurring reservation and attributes.
  """
  @spec create_recurring_reservation_instance!(any(), map()) :: any()
  def create_recurring_reservation_instance!(recurring_reservation, attrs \\ %{}) do
    defaults = %{
      recurring_reservation_id: recurring_reservation.id,
      scheduled_date: Date.utc_today(),
      sequence_number: 1,
      status: :pending,
      notes: "Test instance notes"
    }

    attrs = Map.merge(defaults, attrs)

    case RivaAsh.Resources.RecurringReservationInstance.create(attrs) do
      {:ok, instance} -> instance
      {:error, error} -> raise "Failed to create test recurring reservation instance: #{inspect(error)}"
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
