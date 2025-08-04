defmodule RivaAsh.PropertyTesting.DataManager do
  @moduledoc """
  Manages test data creation and cleanup for property-based browser testing.

  This module handles the creation of realistic test data and ensures proper
  cleanup to maintain test isolation between property-based test runs.
  """

  import Ash.Expr
  alias RivaAsh.Factory
  alias RivaAsh.Accounts.User
  alias RivaAsh.Resources.{Business, Client, Item, Employee}

  @type resource_type :: :user | :business | :client | :item | :employee
  @type resource_id :: integer() | String.t()
  @type cleanup_strategy :: :immediate | :after_test | :after_suite

  @doc """
  Initialize test data for a property-based test session.
  """
  def initialize_test_data(opts \\ []) do
    strategy = Keyword.get(opts, :cleanup_strategy, :after_test)

    # Create base test users
    users = create_base_users()

    # Create some base business data
    businesses = create_base_businesses(users)

    # Create some base clients and items
    clients = create_base_clients(businesses)
    items = create_base_items(businesses)

    test_data = %{
      users: users,
      businesses: businesses,
      clients: clients,
      items: items,
      cleanup_strategy: strategy,
      created_at: DateTime.utc_now()
    }

    # Store test data reference for cleanup
    store_test_data_reference(test_data)

    test_data
  end

  @doc """
  Create a resource during test execution.
  """
  def create_resource(resource_type, attrs \\ %{}) do
    case resource_type do
      :user -> create_user(attrs)
      :business -> create_business(attrs)
      :client -> create_client(attrs)
      :item -> create_item(attrs)
      :employee -> create_employee(attrs)
    end
  end

  @doc """
  Clean up a specific resource.
  """
  def cleanup_resource({resource_type, resource_id}) do
    case resource_type do
      :user -> delete_user(resource_id)
      :business -> delete_business(resource_id)
      :client -> delete_client(resource_id)
      :item -> delete_item(resource_id)
      :employee -> delete_employee(resource_id)
    end
  end

  @doc """
  Clean up all test data created during the session.
  """
  def cleanup_all_test_data do
    test_data_references = get_test_data_references()

    Enum.each(test_data_references, fn test_data ->
      cleanup_test_data_set(test_data)
    end)

    clear_test_data_references()
  end

  @doc """
  Get existing test users for login flows.
  """
  def get_test_users do
    [
      %{email: "test@example.com", password: "password123", role: :user},
      %{email: "admin@example.com", password: "admin123", role: :admin},
      %{email: "manager@example.com", password: "manager123", role: :manager}
    ]
  end

  @doc """
  Generate realistic test data for a specific resource type.
  """
  def generate_realistic_data(resource_type, context \\ %{}) do
    case resource_type do
      :user -> generate_user_data(context)
      :business -> generate_business_data(context)
      :client -> generate_client_data(context)
      :item -> generate_item_data(context)
      :employee -> generate_employee_data(context)
    end
  end

  @doc """
  Ensure test database is in a clean state.
  """
  def ensure_clean_state do
    # Clean up any leftover test data
    cleanup_all_test_data()

    # Reset sequences if needed
    reset_database_sequences()

    # Verify clean state
    verify_clean_state()
  end

  # Private functions

  defp create_base_users do
    test_users = [
      %{
        name: "Test User",
        email: "test@example.com",
        password: "password123"
      },
      %{
        name: "Admin User",
        email: "admin@example.com",
        password: "admin123"
      },
      %{
        name: "Manager User",
        email: "manager@example.com",
        password: "manager123"
      }
    ]

    Enum.map(test_users, fn user_attrs ->
      case User
           |> Ash.Changeset.for_create(:register_with_password, user_attrs)
           |> Ash.create(domain: RivaAsh.Accounts) do
        {:ok, user} ->
          user

        {:error, _} ->
          # User might already exist, try to find it
          case User
               |> Ash.Query.for_read(:read)
               |> Ash.Query.filter(expr(email == ^user_attrs.email))
               |> Ash.read_one(domain: RivaAsh.Accounts) do
            {:ok, user} -> user
            _ -> nil
          end
      end
    end)
    |> Enum.filter(& &1)
  end

  defp create_base_businesses(users) do
    return_if_empty(users, [])

    business_data = [
      %{name: "Test Business 1", description: "A test business for property testing"},
      %{name: "Test Business 2", description: "Another test business"},
      %{name: "Admin Business", description: "Business for admin testing"}
    ]

    Enum.zip(business_data, users)
    |> Enum.map(fn {business_attrs, user} ->
      attrs = Map.put(business_attrs, :owner_id, user.id)
      Factory.create!(:business, attrs)
    end)
  end

  defp create_base_clients(businesses) do
    return_if_empty(businesses, [])

    client_data = [
      %{name: "Test Client 1", email: "client1@example.com"},
      %{name: "Test Client 2", email: "client2@example.com"},
      %{name: "VIP Client", email: "vip@example.com"}
    ]

    business = List.first(businesses)

    Enum.map(client_data, fn client_attrs ->
      attrs = Map.put(client_attrs, :business_id, business.id)
      Factory.create!(:client, attrs)
    end)
  end

  defp create_base_items(businesses) do
    return_if_empty(businesses, [])

    item_data = [
      %{name: "Test Item 1", price: 100.00},
      %{name: "Test Item 2", price: 150.00},
      %{name: "Premium Item", price: 500.00}
    ]

    business = List.first(businesses)

    Enum.map(item_data, fn item_attrs ->
      attrs = Map.put(item_attrs, :business_id, business.id)
      Factory.create!(:item, attrs)
    end)
  end

  defp create_user(attrs) do
    default_attrs = %{
      name: "Generated User #{:rand.uniform(1000)}",
      email: "user#{:rand.uniform(1000)}@example.com",
      password: "password123"
    }

    merged_attrs = Map.merge(default_attrs, attrs)

    User
    |> Ash.Changeset.for_create(:register_with_password, merged_attrs)
    |> Ash.create(domain: RivaAsh.Accounts)
  end

  defp create_business(attrs) do
    default_attrs = %{
      name: "Generated Business #{:rand.uniform(1000)}",
      description: "A generated business for testing"
    }

    merged_attrs = Map.merge(default_attrs, attrs)
    Factory.create(:business, merged_attrs)
  end

  defp create_client(attrs) do
    default_attrs = %{
      name: "Generated Client #{:rand.uniform(1000)}",
      email: "client#{:rand.uniform(1000)}@example.com"
    }

    merged_attrs = Map.merge(default_attrs, attrs)
    Factory.create(:client, merged_attrs)
  end

  defp create_item(attrs) do
    default_attrs = %{
      name: "Generated Item #{:rand.uniform(1000)}",
      price: :rand.uniform(500) + 50.0
    }

    merged_attrs = Map.merge(default_attrs, attrs)
    Factory.create(:item, merged_attrs)
  end

  defp create_employee(attrs) do
    default_attrs = %{
      name: "Generated Employee #{:rand.uniform(1000)}",
      email: "employee#{:rand.uniform(1000)}@example.com"
    }

    merged_attrs = Map.merge(default_attrs, attrs)
    Factory.create(:employee, merged_attrs)
  end

  defp delete_user(user_id) do
    case Ash.get(User, user_id, domain: RivaAsh.Accounts) do
      {:ok, user} -> Ash.destroy(user, domain: RivaAsh.Accounts)
      _ -> :ok
    end
  end

  defp delete_business(business_id) do
    case Ash.get(Business, business_id) do
      {:ok, business} -> Ash.destroy(business)
      _ -> :ok
    end
  end

  defp delete_client(client_id) do
    case Ash.get(Client, client_id) do
      {:ok, client} -> Ash.destroy(client)
      _ -> :ok
    end
  end

  defp delete_item(item_id) do
    case Ash.get(Item, item_id) do
      {:ok, item} -> Ash.destroy(item)
      _ -> :ok
    end
  end

  defp delete_employee(employee_id) do
    case Ash.get(Employee, employee_id) do
      {:ok, employee} -> Ash.destroy(employee)
      _ -> :ok
    end
  end

  defp generate_user_data(_context) do
    %{
      name: Faker.Person.name(),
      email: Faker.Internet.email(),
      password: "password123"
    }
  end

  defp generate_business_data(_context) do
    %{
      name: Faker.Company.name(),
      description: Faker.Company.catch_phrase()
    }
  end

  defp generate_client_data(_context) do
    %{
      name: Faker.Person.name(),
      email: Faker.Internet.email(),
      phone: Faker.Phone.number()
    }
  end

  defp generate_item_data(_context) do
    %{
      name: Faker.Commerce.product_name(),
      price: Faker.Commerce.price() |> String.to_float(),
      description: Faker.Lorem.sentence()
    }
  end

  defp generate_employee_data(_context) do
    %{
      name: Faker.Person.name(),
      email: Faker.Internet.email(),
      role: Enum.random(["manager", "staff", "admin"])
    }
  end

  defp store_test_data_reference(test_data) do
    # Store in process dictionary or ETS table for cleanup
    current_refs = Process.get(:test_data_refs, [])
    Process.put(:test_data_refs, [test_data | current_refs])
  end

  defp get_test_data_references do
    Process.get(:test_data_refs, [])
  end

  defp clear_test_data_references do
    Process.put(:test_data_refs, [])
  end

  defp cleanup_test_data_set(test_data) do
    # Clean up in reverse order to handle dependencies
    cleanup_resources(test_data.items)
    cleanup_resources(test_data.clients)
    cleanup_resources(test_data.businesses)
    cleanup_resources(test_data.users)
  end

  defp cleanup_resources(resources) when is_list(resources) do
    Enum.each(resources, fn resource ->
      if resource && resource.id do
        resource_type = get_resource_type(resource)
        cleanup_resource({resource_type, resource.id})
      end
    end)
  end

  defp get_resource_type(%User{}), do: :user
  defp get_resource_type(%Business{}), do: :business
  defp get_resource_type(%Client{}), do: :client
  defp get_resource_type(%Item{}), do: :item
  defp get_resource_type(%Employee{}), do: :employee
  defp get_resource_type(_), do: :unknown

  defp reset_database_sequences do
    # Reset auto-increment sequences if needed
    # This is database-specific and might not be necessary
    :ok
  end

  defp verify_clean_state do
    # Verify that test data has been properly cleaned up
    # Could check for specific test data patterns
    :ok
  end

  defp return_if_empty([], default), do: default
  defp return_if_empty(list, _default), do: list
end
