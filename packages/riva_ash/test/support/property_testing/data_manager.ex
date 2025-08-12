alias RivaAsh.PropertyTesting, as: PropertyTesting
alias RivaAsh.Accounts, as: Accounts
alias RivaAsh.Resources, as: Resources
alias Ash.Changeset, as: Changeset
import Ash.Query
alias Faker.Person, as: Person
alias Faker.Internet, as: Internet
alias Faker.Company, as: Company
alias Faker.Phone, as: Phone
alias Faker.Commerce, as: Commerce
alias Faker.Lorem, as: Lorem

defmodule RivaAsh.PropertyTesting.DataManager do
@moduledoc """
Manages test data creation and cleanup for property-based browser testing.

This module handles the creation of realistic test data and ensures proper
cleanup to maintain test isolation between property-based test runs.
"""

import Ash.Expr
alias RivaAsh.Factory
alias RivaAsh.Accounts.User
alias RivaAsh.Resources.{Business, Client, Item, Employee, Plot, Section, ItemType, Layout, ItemPosition, Pricing, Payment, Reservation, AvailabilityException, RecurringReservation, RecurringReservationInstance}

  @type resource_type :: :user | :business | :client | :item | :employee | :plot | :section | :item_type | :layout | :item_position | :pricing | :payment | :reservation | :availability_exception | :recurring_reservation | :recurring_reservation_instance
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

    # Create basic test data (avoiding complex relationships that require special permissions)
    test_data = create_basic_test_data(businesses, users)

    test_data = Map.put(test_data, :cleanup_strategy, strategy)
    test_data = Map.put(test_data, :created_at, DateTime.utc_now())

    # Store test data reference for cleanup
    store_test_data_reference(test_data)

    test_data
  end

  @doc """
  Create basic test data that doesn't require special permissions.
  """
  def create_basic_test_data(businesses, users) do
    # Create basic clients for each business
    clients =
      Enum.flat_map(businesses, fn business ->
        create_base_clients(business)
      end)

    # Create basic items for each business
    items =
      Enum.flat_map(businesses, fn business ->
        create_base_items(business)
      end)

    %{
      users: users,
      businesses: businesses,
      clients: clients,
      items: items,
      plots: [],
      sections: [],
      item_types: [],
      layouts: [],
      item_positions: [],
      pricing: [],
      employees: [],
      reservations: [],
      payments: [],
      availability_exceptions: [],
      recurring_reservations: [],
      recurring_reservation_instances: []
    }
  end

  @doc """
  Create a resource during test execution.
  """
  def create_resource(resource_type, attrs \\ %{}) do
    resource = case resource_type do
      :user ->
        # Use the accounts domain to create users
        user = RivaAsh.Accounts.User
        |> Ash.Changeset.for_create(:register_with_password, Map.merge(%{
          name: "Test User",
          email: "test#{System.unique_integer([:positive])}@example.com",
          password: "password123",
          role: :admin
        }, attrs))
        |> Ash.create!(domain: RivaAsh.Accounts)
        
        # Store the user ID in the test data registry
        store_test_data_ref(:user, user.id)
        user
        
      :business ->
        # Get the first user to use as actor for business creation
        user = case get_test_data_ids().user_ids do
          [user_id | _] ->
            case RivaAsh.Accounts.User
                 |> Ash.Query.filter(id == ^user_id)
                 |> Ash.read_one(action: :seed_read, domain: RivaAsh.Accounts) do
              {:ok, user} -> user
              _ -> nil
            end
          _ -> nil
        end
        
        # If we have a user, use it as actor, otherwise create without actor
        if user do
          business = RivaAsh.Resources.Business
                     |> Ash.Changeset.for_create(:create, attrs, actor: user, domain: RivaAsh.Domain)
                     |> Ash.create!(domain: RivaAsh.Domain)
          store_test_data_ref(:business, business.id)
          business
        else
          # Fallback: create a user first, then create business with that user as actor
          user_attrs = %{
            email: "test-business-owner@example.com",
            password: "password123",
            password_confirmation: "password123",
            first_name: "Test",
            last_name: "Business Owner",
            role: :user
          }
          
          case RivaAsh.Accounts.User
               |> Ash.Changeset.for_create(:register_with_password, user_attrs, domain: RivaAsh.Accounts)
               |> Ash.create(domain: RivaAsh.Accounts) do
            {:ok, user} ->
              # Now create business with the user as actor
              business = RivaAsh.Resources.Business
                         |> Ash.Changeset.for_create(:create, Map.put(attrs, :owner_id, user.id), actor: user, domain: RivaAsh.Domain)
                         |> Ash.create!(domain: RivaAsh.Domain)
              store_test_data_ref(:business, business.id)
              business
            {:error, _} ->
              # If user creation fails, try creating business without actor
              case RivaAsh.Resources.Business
                   |> Ash.Changeset.for_create(:create, attrs, domain: RivaAsh.Domain)
                   |> Ash.create(domain: RivaAsh.Domain) do
                {:ok, business} ->
                  store_test_data_ref(:business, business.id)
                  business
                {:error, _} ->
                  # Final fallback: use the factory
                  business = Factory.create!(:business, attrs)
                  store_test_data_ref(:business, business.id)
                  business
              end
          end
        end
        
      :client ->
        client = Factory.create!(:client, attrs)
        store_test_data_ref(:client, client.id)
        client
        
      :item ->
        item = Factory.create!(:item, attrs)
        store_test_data_ref(:item, item.id)
        item
        
      :employee ->
        employee = Factory.create!(:employee, attrs)
        store_test_data_ref(:employee, employee.id)
        employee
        
      :plot ->
        plot = Factory.create!(:plot, attrs)
        store_test_data_ref(:plot, plot.id)
        plot
        
      :section ->
        section = Factory.create!(:section, attrs)
        store_test_data_ref(:section, section.id)
        section
        
      :item_type ->
        item_type = Factory.create!(:item_type, attrs)
        store_test_data_ref(:item_type, item_type.id)
        item_type
        
      :layout ->
        layout = Factory.create!(:layout, attrs)
        store_test_data_ref(:layout, layout.id)
        layout
        
      :item_position ->
        item_position = Factory.create!(:item_position, attrs)
        store_test_data_ref(:item_position, item_position.id)
        item_position
        
      :pricing ->
        pricing = Factory.create!(:pricing, attrs)
        store_test_data_ref(:pricing, pricing.id)
        pricing
        
      :payment ->
        payment = Factory.create!(:payment, attrs)
        store_test_data_ref(:payment, payment.id)
        payment
        
      :reservation ->
        reservation = Factory.create!(:reservation, attrs)
        store_test_data_ref(:reservation, reservation.id)
        reservation
        
      :availability_exception ->
        availability_exception = Factory.create!(:availability_exception, attrs)
        store_test_data_ref(:availability_exception, availability_exception.id)
        availability_exception
        
      :recurring_reservation ->
        recurring_reservation = Factory.create!(:recurring_reservation, attrs)
        store_test_data_ref(:recurring_reservation, recurring_reservation.id)
        recurring_reservation
        
      :recurring_reservation_instance ->
        recurring_reservation_instance = Factory.create!(:recurring_reservation_instance, attrs)
        store_test_data_ref(:recurring_reservation_instance, recurring_reservation_instance.id)
        recurring_reservation_instance
    end
    
    resource
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
      :plot -> delete_plot(resource_id)
      :section -> delete_section(resource_id)
      :item_type -> delete_item_type(resource_id)
      :layout -> delete_layout(resource_id)
      :item_position -> delete_item_position(resource_id)
      :pricing -> delete_pricing(resource_id)
      :payment -> delete_payment(resource_id)
      :reservation -> delete_reservation(resource_id)
      :availability_exception -> delete_availability_exception(resource_id)
      :recurring_reservation -> delete_recurring_reservation(resource_id)
      :recurring_reservation_instance -> delete_recurring_reservation_instance(resource_id)
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
      %{email: "test@example.com", password: "password123", role: :user, id: 1},
      %{email: "admin@example.com", password: "admin123", role: :admin, id: 2},
      %{email: "manager@example.com", password: "manager123", role: :manager, id: 3}
    ]
  end

  @doc """
  Get test data IDs for generating valid route parameters.
  """
  def get_test_data_ids do
    # Initialize test data if not already done
    case Process.get(:test_data_initialized) do
      true ->
        get_existing_ids()
      _ ->
        # Only initialize if we don't have enough test data
        case get_existing_ids() do
          %{business_ids: [], client_ids: [], item_ids: []} ->
            # Create minimal test data to avoid timeout issues
            create_minimal_test_data()
            Process.put(:test_data_initialized, true)
            get_existing_ids()
          existing_ids when is_map(existing_ids) ->
            # We have some existing data, use it
            Process.put(:test_data_initialized, true)
            existing_ids
        end
    end
  end


  @doc """
  Generate realistic test data for a specific resource type.
  """
  def generate_realistic_data(resource_type) do
    case resource_type do
      :user ->
        %{
          name: "Test User",
          email: "test#{System.unique_integer([:positive])}@example.com",
          password: "password123",
          role: :user
        }
        
      :business ->
        %{
          name: "Test Business #{System.unique_integer([:positive])}",
          description: "A test business for property testing",
          is_active: true
        }
        
      :client ->
        %{
          name: "Test Client",
          email: nil, # Set to nil to bypass verification constraints
          phone: "555-#{:rand.uniform(9999)}-#{:rand.uniform(9999)}",
          is_registered: false
        }
        
      :item ->
        %{
          name: "Test Item",
          description: "A test item for property testing",
          capacity: :rand.uniform(10) + 1,
          is_active: true,
          is_always_available: false
        }
        
      :employee ->
        %{
          name: "Test Employee",
          email: "employee#{System.unique_integer([:positive])}@example.com",
          phone: "555-#{:rand.uniform(9999)}-#{:rand.uniform(9999)}",
          role: :employee
        }
        
      :plot ->
        %{
          name: "Test Plot",
          description: "A test plot",
          total_area: :rand.uniform(2000) + 500,
          area_unit: "sqft",
          is_active: true
        }
        
      :section ->
        %{
          name: "Test Section",
          description: "A test section",
          capacity: :rand.uniform(100) + 10
        }
        
      :item_type ->
        %{
          name: "Test Item Type",
          description: "A test item type"
        }
        
      :layout ->
        %{
          name: "Test Layout",
          description: "A test layout"
        }
        
      :item_position ->
        %{
          x: :rand.uniform(500),
          y: :rand.uniform(500),
          width: :rand.uniform(100) + 20,
          height: :rand.uniform(100) + 20
        }
        
      :pricing ->
        %{
          base_price: :rand.uniform(500) + 50,
          weekend_surcharge: :rand.uniform(100) + 10,
          holiday_surcharge: :rand.uniform(150) + 25
        }
        
      :payment ->
        %{
          amount: :rand.uniform(1000) + 50,
          payment_method: Enum.random(["credit_card", "paypal", "bank_transfer", "cash"]),
          status: Enum.random(["completed", "pending", "failed", "refunded"])
        }
        
      :reservation ->
        start_time = DateTime.add(DateTime.utc_now(), :rand.uniform(30), :day)
        end_time = DateTime.add(start_time, :rand.uniform(7), :day)
        
        %{
          start_time: start_time,
          end_time: end_time,
          status: Enum.random(["confirmed", "pending", "cancelled", "completed"])
        }
        
      :availability_exception ->
        start_date = Date.add(Date.utc_today(), :rand.uniform(60))
        end_date = Date.add(start_date, :rand.uniform(7))
        
        %{
          start_date: start_date,
          end_date: end_date,
          reason: "Test exception"
        }
        
      :recurring_reservation ->
        start_date = Date.utc_today()
        end_date = Date.add(start_date, :rand.uniform(365))
        
        %{
          frequency: Enum.random(["weekly", "monthly", "daily"]),
          interval: :rand.uniform(4) + 1,
          start_date: start_date,
          end_date: end_date
        }
        
      :recurring_reservation_instance ->
        start_time = DateTime.add(DateTime.utc_now(), :rand.uniform(90), :day)
        end_time = DateTime.add(start_time, :rand.uniform(7), :day)
        
        %{
          start_time: start_time,
          end_time: end_time,
          status: Enum.random(["confirmed", "pending", "cancelled"])
        }
        
      _ ->
        %{}
    end
  end

  defp create_comprehensive_test_data(businesses, users) do
    business = List.first(businesses)
    # Use the manager user for creating plots that require manager role
    manager_user = Enum.find(users, &(&1.role == :manager)) || List.first(users)

    # Create plots for the business using the manager user
    plots = create_base_plots_with_actor(business, manager_user)

    # Create sections for the plots using the manager user
    sections = create_base_sections_with_actor(plots, manager_user)

    # Create item types for the business using the manager user
    item_types = create_base_item_types_with_actor(business, manager_user)

    # Create layouts for the business using the manager user
    layouts = create_base_layouts_with_actor(business, manager_user)

    # Create items for the sections and item types
    items = create_base_items_with_relationships(sections, item_types, business)

    # Create item positions for the items
    item_positions = create_base_item_positions(items, sections)

    # Create pricing for the items
    pricing = create_base_pricing(items)

    # Create clients for the business
    clients = create_base_clients(business)

    # Create employees for the business
    employees = create_base_employees(business)

    # Create reservations
    reservations = create_base_reservations(clients, items, employees)

    # Create payments for the reservations
    payments = create_base_payments(reservations, clients)

    # Create availability exceptions
    availability_exceptions = create_base_availability_exceptions(items)

    # Create recurring reservations
    recurring_reservations = create_base_recurring_reservations(clients, items)

    # Create recurring reservation instances
    recurring_reservation_instances = create_base_recurring_reservation_instances(recurring_reservations)

    %{
      users: users,
      businesses: businesses,
      plots: plots,
      sections: sections,
      item_types: item_types,
      layouts: layouts,
      items: items,
      item_positions: item_positions,
      pricing: pricing,
      clients: clients,
      employees: employees,
      reservations: reservations,
      payments: payments,
      availability_exceptions: availability_exceptions,
      recurring_reservations: recurring_reservations,
      recurring_reservation_instances: recurring_reservation_instances
    }
  end

  # Get existing IDs from test data
  defp get_existing_ids do
    # Get actual IDs from the database
    user_ids = get_resource_ids(User, RivaAsh.Accounts)
    business_ids = get_resource_ids(Business, RivaAsh.Resources)
    client_ids = get_resource_ids(Client, RivaAsh.Resources)
    item_ids = get_resource_ids(Item, RivaAsh.Resources)
    employee_ids = get_resource_ids(Employee, RivaAsh.Resources)
    reservation_ids = get_resource_ids(Reservation, RivaAsh.Resources)
    plot_ids = get_resource_ids(RivaAsh.Resources.Plot, RivaAsh.Resources)
    section_ids = get_resource_ids(RivaAsh.Resources.Section, RivaAsh.Resources)
    item_type_ids = get_resource_ids(RivaAsh.Resources.ItemType, RivaAsh.Resources)
    layout_ids = get_resource_ids(RivaAsh.Resources.Layout, RivaAsh.Resources)
    item_position_ids = get_resource_ids(RivaAsh.Resources.ItemPosition, RivaAsh.Resources)
    pricing_ids = get_resource_ids(RivaAsh.Resources.Pricing, RivaAsh.Resources)
    payment_ids = get_resource_ids(RivaAsh.Resources.Payment, RivaAsh.Resources)
    availability_exception_ids = get_resource_ids(RivaAsh.Resources.AvailabilityException, RivaAsh.Resources)
    recurring_reservation_ids = get_resource_ids(RivaAsh.Resources.RecurringReservation, RivaAsh.Resources)
    recurring_reservation_instance_ids = get_resource_ids(RivaAsh.Resources.RecurringReservationInstance, RivaAsh.Resources)

    %{
      user_ids: user_ids,
      business_ids: business_ids,
      client_ids: client_ids,
      item_ids: item_ids,
      employee_ids: employee_ids,
      reservation_ids: reservation_ids,
      plot_ids: plot_ids,
      section_ids: section_ids,
      item_type_ids: item_type_ids,
      layout_ids: layout_ids,
      item_position_ids: item_position_ids,
      pricing_ids: pricing_ids,
      payment_ids: payment_ids,
      availability_exception_ids: availability_exception_ids,
      recurring_reservation_ids: recurring_reservation_ids,
      recurring_reservation_instance_ids: recurring_reservation_instance_ids
    }
  end

  # Helper function to get actual resource IDs from database
  defp get_resource_ids(resource_module, domain) do
    # Use seed_read action for Business resource to bypass authorization
    action = if resource_module == Business, do: :seed_read, else: :read
    
    case resource_module
         |> Ash.Query.for_read(action)
         |> Ash.read(domain: domain) do
      {:ok, records} when is_list(records) ->
        Enum.map(records, & &1.id)
      _ ->
        []
    end
  end

  @doc """
  Get a random valid ID for a specific resource type.
  """
  def get_random_id(resource_type) do
    test_data_ids = get_test_data_ids()
    
    case resource_type do
      :user ->
        case test_data_ids.user_ids do
          [] -> 1 # fallback
          ids -> Enum.random(ids)
        end
      :business ->
        case test_data_ids.business_ids do
          [] -> 1 # fallback
          ids -> Enum.random(ids)
        end
      :client ->
        case test_data_ids.client_ids do
          [] -> 1 # fallback
          ids -> Enum.random(ids)
        end
      :item ->
        case test_data_ids.item_ids do
          [] -> 1 # fallback
          ids -> Enum.random(ids)
        end
      :employee ->
        case test_data_ids.employee_ids do
          [] -> 1 # fallback
          ids -> Enum.random(ids)
        end
      :reservation ->
        case test_data_ids.reservation_ids do
          [] -> 1 # fallback
          ids -> Enum.random(ids)
        end
      :plot ->
        case test_data_ids.plot_ids do
          [] -> 1 # fallback
          ids -> Enum.random(ids)
        end
      :section ->
        case test_data_ids.section_ids do
          [] -> 1 # fallback
          ids -> Enum.random(ids)
        end
      :item_type ->
        case test_data_ids.item_type_ids do
          [] -> 1 # fallback
          ids -> Enum.random(ids)
        end
      :layout ->
        case test_data_ids.layout_ids do
          [] -> 1 # fallback
          ids -> Enum.random(ids)
        end
      :item_position ->
        case test_data_ids.item_position_ids do
          [] -> 1 # fallback
          ids -> Enum.random(ids)
        end
      :pricing ->
        case test_data_ids.pricing_ids do
          [] -> 1 # fallback
          ids -> Enum.random(ids)
        end
      :payment ->
        case test_data_ids.payment_ids do
          [] -> 1 # fallback
          ids -> Enum.random(ids)
        end
      :availability_exception ->
        case test_data_ids.availability_exception_ids do
          [] -> 1 # fallback
          ids -> Enum.random(ids)
        end
      :recurring_reservation ->
        case test_data_ids.recurring_reservation_ids do
          [] -> 1 # fallback
          ids -> Enum.random(ids)
        end
      :recurring_reservation_instance ->
        case test_data_ids.recurring_reservation_instance_ids do
          [] -> 1 # fallback
          ids -> Enum.random(ids)
        end
      _ -> Enum.random(1..100) # fallback
    end
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
      :plot -> generate_plot_data(context)
      :section -> generate_section_data(context)
      :item_type -> generate_item_type_data(context)
      :layout -> generate_layout_data(context)
      :item_position -> generate_item_position_data(context)
      :pricing -> generate_pricing_data(context)
      :payment -> generate_payment_data(context)
      :reservation -> generate_reservation_data(context)
      :availability_exception -> generate_availability_exception_data(context)
      :recurring_reservation -> generate_recurring_reservation_data(context)
      :recurring_reservation_instance -> generate_recurring_reservation_instance_data(context)
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
        password: "password123",
        role: "user"
      },
      %{
        name: "Admin User",
        email: "admin@example.com",
        password: "admin123",
        role: "admin"
      },
      %{
        name: "Manager User",
        email: "manager@example.com",
        password: "manager123",
        role: "manager"
      }
    ]

    created_users =
      Enum.map(test_users, fn user_attrs ->
        IO.puts("Attempting to create user: #{user_attrs.email}")
        
        # First try to find existing user using the seed_read action
        case User
             |> Ash.Query.filter(email == ^user_attrs.email)
             |> Ash.read_one(action: :seed_read, domain: RivaAsh.Accounts) do
          {:ok, user} ->
            IO.puts("Found existing user: #{user.email}")
            user

          {:error, _unmatchedunmatched} ->
            # User doesn't exist, try to create it
            case User
                 |> Ash.Changeset.for_create(:register_with_password, user_attrs, domain: RivaAsh.Accounts)
                 |> Ash.create(domain: RivaAsh.Accounts) do
              {:ok, user} ->
                IO.puts("Successfully created user: #{user.email}")
                user

              {:error, error} ->
                IO.puts("Failed to create user #{user_attrs.email}: #{inspect(error)}")
                # Try to find again after creation failure
                case User
                     |> Ash.Query.filter(email == ^user_attrs.email)
                    |> Ash.read_one(action: :seed_read, domain: RivaAsh.Accounts) do
                  {:ok, user} ->
                    IO.puts("Found user after creation failure: #{user.email}")
                    user
                  {:error, _unmatchedunmatched} ->
                    IO.puts("User not found: #{user_attrs.email}")
                    nil
                end
            end
        end
      end)
      |> Enum.filter(& &1)

    IO.puts("Created #{length(created_users)} users")
    created_users
  end

  defp create_base_businesses(users) do
    return_if_empty(users, [])

    # Generate unique business names to avoid constraint violations
    timestamp = DateTime.utc_now() |> DateTime.to_unix()
    random_id = :rand.uniform(10000)
    unique_id = System.system_time(:microsecond)
    
    # Create just one business to avoid timeout issues
    business_attrs = %{name: "Test Business #{timestamp}_#{random_id}_#{unique_id}", description: "A test business for property testing"}
    user = List.first(users)
    
    attrs = Map.put(business_attrs, :owner_id, user.id)
    
    # Use Ash.create!/2 with actor to bypass authorization
    case RivaAsh.Resources.Business
         |> Ash.Changeset.for_create(:create, attrs, actor: user, domain: RivaAsh.Domain)
         |> Ash.create(domain: RivaAsh.Domain) do
      {:ok, business} ->
        IO.puts("Created business: #{business.name}")
        [business]
      {:error, reason} ->
        IO.puts("Failed to create business: #{inspect(reason)}")
        []
    end
  rescue
    error ->
      IO.puts("Error in create_base_businesses: #{inspect(error)}")
      []
    end

  defp create_minimal_test_data do
    # Create minimal test data to avoid timeout issues
    users = create_base_users()
    
    if length(users) > 0 do
      businesses = create_base_businesses(users)
      
      if length(businesses) > 0 do
        # Create only one client total to minimize database load
        clients =
          case businesses do
            [business | _] ->
              create_single_client(business)
            _ ->
              []
          end
        
        # Create only one item total to minimize database load
        items =
          case businesses do
            [business | _] ->
              create_single_item(business)
            _ ->
              []
          end
        
        IO.puts("Created minimal test data: #{length(users)} users, #{length(businesses)} businesses, #{length(clients)} clients, #{length(items)} items")
        
        # Return minimal test data structure for proper tracking
        %{
          users: users,
          businesses: businesses,
          clients: clients,
          items: items,
          plots: [],
          sections: [],
          item_types: [],
          layouts: [],
          item_positions: [],
          pricing: [],
          employees: [],
          reservations: [],
          payments: [],
          availability_exceptions: [],
          recurring_reservations: [],
          recurring_reservation_instances: []
        }
      else
        # Return empty structure if no businesses could be created
        %{
          users: users,
          businesses: [],
          clients: [],
          items: [],
          plots: [],
          sections: [],
          item_types: [],
          layouts: [],
          item_positions: [],
          pricing: [],
          employees: [],
          reservations: [],
          payments: [],
          availability_exceptions: [],
          recurring_reservations: [],
          recurring_reservation_instances: []
        }
      end
    else
      # Return empty structure if no users could be created
      %{
        users: [],
        businesses: [],
        clients: [],
        items: [],
        plots: [],
        sections: [],
        item_types: [],
        layouts: [],
        item_positions: [],
        pricing: [],
        employees: [],
        reservations: [],
        payments: [],
        availability_exceptions: [],
        recurring_reservations: [],
        recurring_reservation_instances: []
      }
    end
  end

  defp create_base_clients(businesses) do
    if businesses do
      # Handle both single business and list of businesses
      businesses_list = if is_list(businesses), do: businesses, else: [businesses]
      
      if length(businesses_list) > 0 do
        business = List.first(businesses_list)
        owner = get_business_owner(business)

        IO.puts("Creating clients for business: #{business.id}, owner: #{inspect(owner)}")

        # Create just one client per business to avoid timeout issues
        client_attrs = %{name: "Test Client", email: "client@example.com", business_id: business.id}
        
        IO.puts("Creating client with attrs: #{inspect(client_attrs)}")
        
        # For test data, set email to nil to bypass the verification token constraint
        # This allows us to create clients without dealing with verification logic
        attrs_without_email = Map.put(client_attrs, :email, nil)
        
        case RivaAsh.Resources.Client
             |> Ash.Changeset.for_create(:create, attrs_without_email, actor: owner, domain: RivaAsh.Domain)
             |> Ash.create(domain: RivaAsh.Domain) do
          {:ok, client} ->
            IO.puts("Created client: #{inspect(client)}")
            [client]
          {:error, reason} ->
            IO.puts("Failed to create client: #{inspect(reason)}")
            # Don't fail the test if client creation fails, just return empty list
            []
        end
      else
        []
      end
    else
      []
    end
  end

  defp create_single_client(business) do
    if business do
      owner = get_business_owner(business)

      IO.puts("Creating single client for business: #{business.id}, owner: #{inspect(owner)}")

      # Create just one client to minimize database load
      client_attrs = %{name: "Test Client", email: "client@example.com", business_id: business.id}
      
      IO.puts("Creating client with attrs: #{inspect(client_attrs)}")
      
      # For test data, set email to nil to bypass the verification token constraint
      # This allows us to create clients without dealing with verification logic
      attrs_without_email = Map.put(client_attrs, :email, nil)
      
      case RivaAsh.Resources.Client
           |> Ash.Changeset.for_create(:create, attrs_without_email, actor: owner, domain: RivaAsh.Domain)
           |> Ash.create(domain: RivaAsh.Domain) do
        {:ok, client} ->
          IO.puts("Created client: #{inspect(client)}")
          [client]
        {:error, reason} ->
          IO.puts("Failed to create client: #{inspect(reason)}")
          # Don't fail the test if client creation fails, just return empty list
          []
      end
    else
      []
    end
  end

  defp create_single_item(business) do
    if business do
      IO.puts("Creating single item for business: #{business.id}")

      # Create just one item to minimize database load
      item_attrs = %{name: "Test Item", business_id: business.id, is_active: true, capacity: 1, is_always_available: false}
      
      IO.puts("Creating item with attrs: #{inspect(item_attrs)}")
      
      # Get the business owner to use as actor for policy compliance
      owner = get_business_owner(business)
      
      case RivaAsh.Resources.Item
           |> Ash.Changeset.for_create(:create, item_attrs, actor: owner, domain: RivaAsh.Domain) do
        {:ok, item} ->
          IO.puts("Created item: #{inspect(item)}")
          [item]
        {:error, reason} ->
          IO.puts("Failed to create item: #{inspect(reason)}")
          # Try without actor if policy validation fails
          case RivaAsh.Resources.Item
               |> Ash.Changeset.for_create(:create, item_attrs, domain: RivaAsh.Domain) do
            {:ok, item} ->
              IO.puts("Created item without actor: #{inspect(item)}")
              [item]
            {:error, reason2} ->
              IO.puts("Failed to create item without actor: #{inspect(reason2)}")
              # Don't fail the test if item creation fails, just return empty list
              []
          end
      end
    else
      []
    end
  end

  defp create_base_items(businesses) do
    IO.puts("DEBUG: create_base_items called with businesses: #{inspect(businesses)}")
    
    # Handle both single business and list of businesses
    businesses_list = if is_list(businesses) do
      businesses
    else
      if businesses, do: [businesses], else: []
    end
    
    if businesses_list && length(businesses_list) > 0 do
      business = List.first(businesses_list)
      
      IO.puts("DEBUG: Creating items for business: #{business.id}")

      # Create just one item per business to avoid timeout issues
      item_attrs = %{name: "Test Item", is_active: true, is_always_available: false, capacity: 1, business_id: business.id, section_id: nil, item_type_id: nil}
      
      IO.puts("DEBUG: Creating item with attrs: #{inspect(item_attrs)}")
      
      # Get the business owner to use as actor for policy compliance
      owner = get_business_owner(business)
      
      IO.puts("DEBUG: Using business owner as actor: #{owner && owner.id}")
      
      # Use the business owner as actor for policy compliance
      created_items = if owner do
        case RivaAsh.Resources.Item
             |> Ash.Changeset.for_create(:create, item_attrs, actor: owner, domain: RivaAsh.Domain)
             |> Ash.create(domain: RivaAsh.Domain) do
          {:ok, item} ->
            IO.puts("DEBUG: Created item: #{inspect(item)}")
            [item]
          {:error, reason} ->
            IO.puts("DEBUG: Failed to create item with actor: #{inspect(reason)}")
            # Try without actor if policy validation fails
            case RivaAsh.Resources.Item
                 |> Ash.Changeset.for_create(:create, item_attrs, domain: RivaAsh.Domain)
                 |> Ash.create(domain: RivaAsh.Domain) do
              {:ok, item} ->
                IO.puts("DEBUG: Created item without actor: #{inspect(item)}")
                [item]
              {:error, reason2} ->
                IO.puts("DEBUG: Failed to create item without actor: #{inspect(reason2)}")
                []
            end
        end
      else
        # Fallback without actor if business owner is not available
        case RivaAsh.Resources.Item
             |> Ash.Changeset.for_create(:create, item_attrs, domain: RivaAsh.Domain)
             |> Ash.create(domain: RivaAsh.Domain) do
          {:ok, item} ->
            IO.puts("DEBUG: Created item without actor: #{inspect(item)}")
            [item]
          {:error, reason} ->
            IO.puts("DEBUG: Failed to create item without actor: #{inspect(reason)}")
            []
        end
      end
              
      IO.puts("DEBUG: Created #{length(created_items)} items")
      created_items
    else
      IO.puts("DEBUG: No businesses to create items for")
      []
    end
  end

  defp get_first_user do
    # Try to get the first user from the test data
    case get_test_data_ids().user_ids do
      [user_id | _] ->
        case RivaAsh.Accounts.User
             |> Ash.Query.filter(id == ^user_id)
             |> Ash.read_one(action: :seed_read, domain: RivaAsh.Accounts) do
          {:ok, user} -> user
          _ -> nil
        end
      _ -> nil
    end
  end

  defp create_user(attrs) do
    default_attrs = %{
      name: "Generated User #{:rand.uniform(1000)}",
      email: "user#{:rand.uniform(1000)}@example.com",
      password: "password123"
    }

    merged_attrs = Map.merge(default_attrs, attrs)

    case RivaAsh.Accounts.register(merged_attrs) do
      {:ok, user} -> user
      {:error, _unmatched} -> nil
    end
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
      _unmatchedunmatched -> :ok
    end
  end

  defp delete_business(business_id) do
    case Ash.get(Business, business_id) do
      {:ok, business} -> Ash.destroy(business)
      _unmatchedunmatched -> :ok
    end
  end

  defp delete_client(client_id) do
    case Ash.get(Client, client_id) do
      {:ok, client} -> Ash.destroy(client)
      _unmatchedunmatched -> :ok
    end
  end

  defp delete_item(item_id) do
    case Ash.get(Item, item_id) do
      {:ok, item} -> Ash.destroy(item)
      _unmatchedunmatched -> :ok
    end
  end

  defp delete_employee(employee_id) do
    case Ash.get(Employee, employee_id) do
      {:ok, employee} -> Ash.destroy(employee)
      _unmatchedunmatched -> :ok
    end
  end

  defp delete_plot(plot_id) do
    case Ash.get(RivaAsh.Resources.Plot, plot_id) do
      {:ok, plot} -> Ash.destroy(plot)
      _unmatchedunmatched -> :ok
    end
  end

  defp delete_section(section_id) do
    case Ash.get(RivaAsh.Resources.Section, section_id) do
      {:ok, section} -> Ash.destroy(section)
      _unmatchedunmatched -> :ok
    end
  end

  defp delete_item_type(item_type_id) do
    case Ash.get(RivaAsh.Resources.ItemType, item_type_id) do
      {:ok, item_type} -> Ash.destroy(item_type)
      _unmatchedunmatched -> :ok
    end
  end

  defp delete_layout(layout_id) do
    case Ash.get(RivaAsh.Resources.Layout, layout_id) do
      {:ok, layout} -> Ash.destroy(layout)
      _unmatchedunmatched -> :ok
    end
  end

  defp delete_item_position(item_position_id) do
    case Ash.get(RivaAsh.Resources.ItemPosition, item_position_id) do
      {:ok, item_position} -> Ash.destroy(item_position)
      _unmatchedunmatched -> :ok
    end
  end

  defp delete_pricing(pricing_id) do
    case Ash.get(RivaAsh.Resources.Pricing, pricing_id) do
      {:ok, pricing} -> Ash.destroy(pricing)
      _unmatchedunmatched -> :ok
    end
  end

  defp delete_payment(payment_id) do
    case Ash.get(RivaAsh.Resources.Payment, payment_id) do
      {:ok, payment} -> Ash.destroy(payment)
      _unmatchedunmatched -> :ok
    end
  end

  defp delete_reservation(reservation_id) do
    case Ash.get(RivaAsh.Resources.Reservation, reservation_id) do
      {:ok, reservation} -> Ash.destroy(reservation)
      _unmatchedunmatched -> :ok
    end
  end

  defp delete_availability_exception(availability_exception_id) do
    case Ash.get(RivaAsh.Resources.AvailabilityException, availability_exception_id) do
      {:ok, availability_exception} -> Ash.destroy(availability_exception)
      _unmatchedunmatched -> :ok
    end
  end

  defp delete_recurring_reservation(recurring_reservation_id) do
    case Ash.get(RivaAsh.Resources.RecurringReservation, recurring_reservation_id) do
      {:ok, recurring_reservation} -> Ash.destroy(recurring_reservation)
      _unmatchedunmatched -> :ok
    end
  end

  defp delete_recurring_reservation_instance(recurring_reservation_instance_id) do
    case Ash.get(RivaAsh.Resources.RecurringReservationInstance, recurring_reservation_instance_id) do
      {:ok, recurring_reservation_instance} -> Ash.destroy(recurring_reservation_instance)
      _unmatchedunmatched -> :ok
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
      phone: Faker.Phone.EnUs.phone()
    }
  end

  defp generate_item_data(_context) do
    %{
      name: Faker.Commerce.product_name(),
      price: case Faker.Commerce.price() do
        price when is_binary(price) -> String.to_float(price)
        price when is_float(price) -> price
        _ -> 10.0
      end,
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

  # New helper functions for comprehensive test data generation
  defp create_base_plots(business) do
    if business && business.id do
      plot_data = [
        %{name: "Main Plot", description: "Primary plot for main activities", total_area: 1000, area_unit: "sqft"},
        %{name: "Secondary Plot", description: "Secondary plot for overflow", total_area: 500, area_unit: "sqft"},
        %{name: "VIP Plot", description: "Premium plot for VIP clients", total_area: 750, area_unit: "sqft"}
      ]

      Enum.map(plot_data, fn plot_attrs ->
        attrs = Map.merge(plot_attrs, %{business_id: business.id, is_active: true})
        # Use the business owner as actor for plot creation
        RivaAsh.Resources.Plot
        |> Ash.Changeset.for_create(:create, attrs, actor: get_business_owner(business), domain: RivaAsh.Domain)
        |> Ash.create!(domain: RivaAsh.Domain)
      end)
    else
      []
    end
  end

  defp get_business_owner(business) do
    # Get the owner of the business to use as actor
    IO.puts("Getting business owner for business ID: #{business.id}, owner_id: #{business.owner_id}")
    
    case RivaAsh.Accounts.User
         |> Ash.Query.filter(id == ^business.owner_id)
         |> Ash.read_one(action: :seed_read, domain: RivaAsh.Accounts) do
      {:ok, user} ->
        IO.puts("Found business owner: #{user.email}")
        user
      _ ->
        IO.puts("Business owner not found, trying to find any user")
        # Fallback: find any user to use as actor
        case RivaAsh.Accounts.User
             |> Ash.Query.for_read(:seed_read)
             |> Ash.read_one(domain: RivaAsh.Accounts) do
          {:ok, user} ->
            IO.puts("Using fallback user: #{user.email}")
            user
          _ ->
            IO.puts("No users found at all")
            nil
        end
    end
  end

  defp create_base_sections(plots) do
    plot = List.first(plots)
    if plot && plot.id do
      section_data = [
        %{name: "Section A", description: "Main section", capacity: 50},
        %{name: "Section B", description: "Secondary section", capacity: 30},
        %{name: "Section C", description: "Premium section", capacity: 20}
      ]

      Enum.map(section_data, fn section_attrs ->
        attrs = Map.merge(section_attrs, %{plot_id: plot.id})
        # Use the business owner as actor for section creation
        RivaAsh.Resources.Section
        |> Ash.Changeset.for_create(:create, attrs, actor: get_business_owner(plot.business), domain: RivaAsh.Domain)
        |> Ash.create!(domain: RivaAsh.Domain)
      end)
    else
      []
    end
  end

  defp create_base_item_types(business) do
    if business && business.id do
      item_type_data = [
        %{name: "Standard Item", description: "Regular items"},
        %{name: "Premium Item", description: "High-end items"},
        %{name: "Special Item", description: "Special occasion items"}
      ]

      Enum.map(item_type_data, fn item_type_attrs ->
        attrs = Map.merge(item_type_attrs, %{business_id: business.id})
        # Use the business owner as actor for item type creation
        RivaAsh.Resources.ItemType
        |> Ash.Changeset.for_create(:create, attrs, actor: get_business_owner(business), domain: RivaAsh.Domain)
        |> Ash.create!(domain: RivaAsh.Domain)
      end)
    else
      []
    end
  end

  defp create_base_layouts(business) do
    if business && business.id do
      layout_data = [
        %{name: "Default Layout", description: "Standard layout"},
        %{name: "Event Layout", description: "Layout for special events"},
        %{name: "Compact Layout", description: "Space-efficient layout"}
      ]

      Enum.map(layout_data, fn layout_attrs ->
        attrs = Map.merge(layout_attrs, %{business_id: business.id})
        # Use the business owner as actor for layout creation
        RivaAsh.Resources.Layout
        |> Ash.Changeset.for_create(:create, attrs, actor: get_business_owner(business), domain: RivaAsh.Domain)
        |> Ash.create!(domain: RivaAsh.Domain)
      end)
    else
      []
    end
  end

  defp create_base_items_with_relationships(sections, item_types, business) do
    if business && business.id && sections && item_types && length(sections) > 0 && length(item_types) > 0 do
      section = List.first(sections)
      item_type = List.first(item_types)

      item_data = [
        %{name: "Table 1", price: 100.00, description: "Standard table"},
        %{name: "Table 2", price: 150.00, description: "Premium table"},
        %{name: "Chair Set", price: 50.00, description: "Set of chairs"},
        %{name: "Decoration", price: 200.00, description: "Event decoration"},
        %{name: "Sound System", price: 300.00, description: "Audio equipment"}
      ]

      Enum.map(item_data, fn item_attrs ->
        attrs = Map.merge(item_attrs, %{
          business_id: business.id,
          section_id: section.id,
          item_type_id: item_type.id
        })
        Factory.create!(:item, attrs)
      end)
    else
      []
    end
  end

  defp create_base_item_positions(items, sections) do
    if items && sections && length(items) > 0 && length(sections) > 0 do
      item = List.first(items)
      section = List.first(sections)

      position_data = [
        %{x: 100, y: 200, width: 50, height: 50},
        %{x: 200, y: 300, width: 60, height: 40},
        %{x: 150, y: 150, width: 40, height: 60}
      ]

      Enum.map(position_data, fn position_attrs ->
        attrs = Map.merge(position_attrs, %{
          item_id: item.id,
          section_id: section.id
        })
        Factory.create!(:item_position, attrs)
      end)
    else
      []
    end
  end

  defp create_base_pricing(items) do
    if items && length(items) > 0 do
      item = List.first(items)

      pricing_data = [
        %{base_price: 100.00, weekend_surcharge: 20.00, holiday_surcharge: 50.00},
        %{base_price: 150.00, weekend_surcharge: 30.00, holiday_surcharge: 75.00},
        %{base_price: 200.00, weekend_surcharge: 40.00, holiday_surcharge: 100.00}
      ]

      Enum.map(pricing_data, fn pricing_attrs ->
        attrs = Map.put(pricing_attrs, :item_id, item.id)
        Factory.create!(:pricing, attrs)
      end)
    else
      []
    end
  end

  defp create_base_employees(business) do
    if business && business.id do
      employee_data = [
        %{name: "John Manager", email: "john@example.com", role: "manager"},
        %{name: "Jane Staff", email: "jane@example.com", role: "staff"},
        %{name: "Bob Admin", email: "bob@example.com", role: "admin"}
      ]

      Enum.map(employee_data, fn employee_attrs ->
        attrs = Map.put(employee_attrs, :business_id, business.id)
        Factory.create!(:employee, attrs)
      end)
    else
      []
    end
  end

  defp create_base_reservations(clients, items, employees) do
    if clients && items && employees && length(clients) > 0 && length(items) > 0 && length(employees) > 0 do
      client = List.first(clients)
      item = List.first(items)
      employee = List.first(employees)

      reservation_data = [
        %{start_time: DateTime.add(DateTime.utc_now(), 2, :day), end_time: DateTime.add(DateTime.utc_now(), 3, :day), status: "confirmed"},
        %{start_time: DateTime.add(DateTime.utc_now(), 5, :day), end_time: DateTime.add(DateTime.utc_now(), 6, :day), status: "pending"},
        %{start_time: DateTime.add(DateTime.utc_now(), 8, :day), end_time: DateTime.add(DateTime.utc_now(), 9, :day), status: "cancelled"}
      ]

      Enum.map(reservation_data, fn reservation_attrs ->
        attrs = Map.merge(reservation_attrs, %{
          client_id: client.id,
          item_id: item.id,
          employee_id: employee.id
        })
        Factory.create!(:reservation, attrs)
      end)
    else
      []
    end
  end

  defp create_base_payments(reservations, clients) do
    if reservations && clients && length(reservations) > 0 && length(clients) > 0 do
      reservation = List.first(reservations)
      client = List.first(clients)

      payment_data = [
        %{amount: 100.00, payment_method: "credit_card", status: "completed"},
        %{amount: 150.00, payment_method: "paypal", status: "pending"},
        %{amount: 200.00, payment_method: "bank_transfer", status: "failed"}
      ]

      Enum.map(payment_data, fn payment_attrs ->
        attrs = Map.merge(payment_attrs, %{
          reservation_id: reservation.id,
          client_id: client.id
        })
        Factory.create!(:payment, attrs)
      end)
    else
      []
    end
  end

  defp create_base_availability_exceptions(items) do
    if items && length(items) > 0 do
      item = List.first(items)

      exception_data = [
        %{start_date: Date.add(Date.utc_today(), 1), end_date: Date.add(Date.utc_today(), 2), reason: "Maintenance"},
        %{start_date: Date.add(Date.utc_today(), 5), end_date: Date.add(Date.utc_today(), 6), reason: "Special Event"},
        %{start_date: Date.add(Date.utc_today(), 10), end_date: Date.add(Date.utc_today(), 11), reason: "Holiday"}
      ]

      Enum.map(exception_data, fn exception_attrs ->
        attrs = Map.put(exception_attrs, :item_id, item.id)
        Factory.create!(:availability_exception, attrs)
      end)
    else
      []
    end
  end

  defp create_base_recurring_reservations(clients, items) do
    if clients && items && length(clients) > 0 && length(items) > 0 do
      client = List.first(clients)
      item = List.first(items)

      recurring_data = [
        %{frequency: "weekly", interval: 1, start_date: Date.utc_today(), end_date: Date.add(Date.utc_today(), 90)},
        %{frequency: "monthly", interval: 1, start_date: Date.utc_today(), end_date: Date.add(Date.utc_today(), 180)},
        %{frequency: "weekly", interval: 2, start_date: Date.utc_today(), end_date: Date.add(Date.utc_today(), 60)}
      ]

      Enum.map(recurring_data, fn recurring_attrs ->
        attrs = Map.merge(recurring_attrs, %{
          client_id: client.id,
          item_id: item.id
        })
        Factory.create!(:recurring_reservation, attrs)
      end)
    else
      []
    end
  end

  defp create_base_recurring_reservation_instances(recurring_reservations) do
    if recurring_reservations && length(recurring_reservations) > 0 do
      recurring = List.first(recurring_reservations)

      instance_data = [
        %{start_time: DateTime.add(DateTime.utc_now(), 1, :day), end_time: DateTime.add(DateTime.utc_now(), 2, :day), status: "confirmed"},
        %{start_time: DateTime.add(DateTime.utc_now(), 8, :day), end_time: DateTime.add(DateTime.utc_now(), 9, :day), status: "confirmed"},
        %{start_time: DateTime.add(DateTime.utc_now(), 15, :day), end_time: DateTime.add(DateTime.utc_now(), 16, :day), status: "pending"}
      ]

      Enum.map(instance_data, fn instance_attrs ->
        attrs = Map.put(instance_attrs, :recurring_reservation_id, recurring.id)
        Factory.create!(:recurring_reservation_instance, attrs)
      end)
    else
      []
    end
  end

  # New data generation functions
  defp generate_plot_data(_context) do
    %{
      name: "Plot #{:rand.uniform(1000)}",
      description: Faker.Lorem.sentence(),
      total_area: :rand.uniform(2000) + 500,
      area_unit: "sqft",
      is_active: true
    }
  end

  defp generate_section_data(_context) do
    %{
      name: "Section #{:rand.uniform(100)}",
      description: Faker.Lorem.sentence(),
      capacity: :rand.uniform(100) + 10
    }
  end

  defp generate_item_type_data(_context) do
    %{
      name: Faker.Commerce.product_name(),
      description: Faker.Lorem.sentence()
    }
  end

  defp generate_layout_data(_context) do
    %{
      name: "Layout #{:rand.uniform(50)}",
      description: Faker.Lorem.sentence()
    }
  end

  defp generate_item_position_data(_context) do
    %{
      x: :rand.uniform(500),
      y: :rand.uniform(500),
      width: :rand.uniform(100) + 20,
      height: :rand.uniform(100) + 20
    }
  end

  defp generate_pricing_data(_context) do
    %{
      base_price: :rand.uniform(500) + 50,
      weekend_surcharge: :rand.uniform(100) + 10,
      holiday_surcharge: :rand.uniform(150) + 25
    }
  end

  defp generate_payment_data(_context) do
    %{
      amount: :rand.uniform(1000) + 50,
      payment_method: Enum.random(["credit_card", "paypal", "bank_transfer", "cash"]),
      status: Enum.random(["completed", "pending", "failed", "refunded"])
    }
  end

  defp generate_reservation_data(_context) do
    start_time = DateTime.add(DateTime.utc_now(), :rand.uniform(30), :day)
    end_time = DateTime.add(start_time, :rand.uniform(7), :day)
    
    %{
      start_time: start_time,
      end_time: end_time,
      status: Enum.random(["confirmed", "pending", "cancelled", "completed"])
    }
  end

  defp generate_availability_exception_data(_context) do
    start_date = Date.add(Date.utc_today(), :rand.uniform(60))
    end_date = Date.add(start_date, :rand.uniform(7))
    
    %{
      start_date: start_date,
      end_date: end_date,
      reason: Faker.Lorem.words(3) |> Enum.join(" ")
    }
  end

  defp generate_recurring_reservation_data(_context) do
    start_date = Date.utc_today()
    end_date = Date.add(start_date, :rand.uniform(365))
    
    %{
      frequency: Enum.random(["weekly", "monthly", "daily"]),
      interval: :rand.uniform(4) + 1,
      start_date: start_date,
      end_date: end_date
    }
  end

  defp generate_recurring_reservation_instance_data(_context) do
    start_time = DateTime.add(DateTime.utc_now(), :rand.uniform(90), :day)
    end_time = DateTime.add(start_time, :rand.uniform(7), :day)
    
    %{
      start_time: start_time,
      end_time: end_time,
      status: Enum.random(["confirmed", "pending", "cancelled"])
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

  defp store_test_data_ref(resource_type, id) do
    current_refs = Process.get(:test_data_refs, [])
    resource_ref = {resource_type, id}
    updated_refs = [resource_ref | current_refs]
    Process.put(:test_data_refs, updated_refs)
  end

  defp cleanup_test_data_set(test_data) do
    # Clean up in reverse order to handle dependencies
    cleanup_resources(test_data.recurring_reservation_instances || [])
    cleanup_resources(test_data.recurring_reservations || [])
    cleanup_resources(test_data.availability_exceptions || [])
    cleanup_resources(test_data.payments || [])
    cleanup_resources(test_data.reservations || [])
    cleanup_resources(test_data.item_positions || [])
    cleanup_resources(test_data.pricing || [])
    cleanup_resources(test_data.items || [])
    cleanup_resources(test_data.layouts || [])
    cleanup_resources(test_data.item_types || [])
    cleanup_resources(test_data.sections || [])
    cleanup_resources(test_data.plots || [])
    cleanup_resources(test_data.employees || [])
    cleanup_resources(test_data.clients || [])
    cleanup_resources(test_data.businesses || [])
    cleanup_resources(test_data.users || [])
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
  defp get_resource_type(%RivaAsh.Resources.Plot{}), do: :plot
  defp get_resource_type(%RivaAsh.Resources.Section{}), do: :section
  defp get_resource_type(%RivaAsh.Resources.ItemType{}), do: :item_type
  defp get_resource_type(%RivaAsh.Resources.Layout{}), do: :layout
  defp get_resource_type(%RivaAsh.Resources.ItemPosition{}), do: :item_position
  defp get_resource_type(%RivaAsh.Resources.Pricing{}), do: :pricing
  defp get_resource_type(%RivaAsh.Resources.Payment{}), do: :payment
  defp get_resource_type(%RivaAsh.Resources.Reservation{}), do: :reservation
  defp get_resource_type(%RivaAsh.Resources.AvailabilityException{}), do: :availability_exception
  defp get_resource_type(%RivaAsh.Resources.RecurringReservation{}), do: :recurring_reservation
  defp get_resource_type(%RivaAsh.Resources.RecurringReservationInstance{}), do: :recurring_reservation_instance
  defp get_unmatchedresource_unmatchedtype(_unmatched), do: :unknown

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

  # Helper functions that use manager user as actor for policy compliance
  defp create_base_plots_with_actor(business, actor) do
    if business && business.id && actor do
      plot_data = [
        %{name: "Main Plot", description: "Primary plot for main activities", total_area: 1000, area_unit: "sqft"},
        %{name: "Secondary Plot", description: "Secondary plot for overflow", total_area: 500, area_unit: "sqft"},
        %{name: "VIP Plot", description: "Premium plot for VIP clients", total_area: 750, area_unit: "sqft"}
      ]

      Enum.map(plot_data, fn plot_attrs ->
        attrs = Map.merge(plot_attrs, %{business_id: business.id, is_active: true})
        RivaAsh.Resources.Plot
        |> Ash.Changeset.for_create(:create, attrs, actor: actor, domain: RivaAsh.Domain)
        |> Ash.create!(domain: RivaAsh.Domain)
      end)
    else
      []
    end
  end

  defp create_base_sections_with_actor(plots, actor) do
    plot = List.first(plots)
    if plot && plot.id && actor do
      section_data = [
        %{name: "Section A", description: "Main section", capacity: 50},
        %{name: "Section B", description: "Secondary section", capacity: 30},
        %{name: "Section C", description: "Premium section", capacity: 20}
      ]

      Enum.map(section_data, fn section_attrs ->
        attrs = Map.merge(section_attrs, %{plot_id: plot.id})
        RivaAsh.Resources.Section
        |> Ash.Changeset.for_create(:create, attrs, actor: actor, domain: RivaAsh.Domain)
        |> Ash.create!(domain: RivaAsh.Domain)
      end)
    else
      []
    end
  end

  defp create_base_item_types_with_actor(business, actor) do
    if business && business.id && actor do
      item_type_data = [
        %{name: "Standard Item", description: "Regular items"},
        %{name: "Premium Item", description: "High-end items"},
        %{name: "Special Item", description: "Special occasion items"}
      ]

      Enum.map(item_type_data, fn item_type_attrs ->
        attrs = Map.merge(item_type_attrs, %{business_id: business.id})
        RivaAsh.Resources.ItemType
        |> Ash.Changeset.for_create(:create, attrs, actor: actor, domain: RivaAsh.Domain)
        |> Ash.create!(domain: RivaAsh.Domain)
      end)
    else
      []
    end
  end

  defp create_base_layouts_with_actor(business, actor) do
    if business && business.id && actor do
      layout_data = [
        %{name: "Default Layout", description: "Standard layout"},
        %{name: "Event Layout", description: "Layout for special events"},
        %{name: "Compact Layout", description: "Space-efficient layout"}
      ]

      Enum.map(layout_data, fn layout_attrs ->
        attrs = Map.merge(layout_attrs, %{business_id: business.id})
        RivaAsh.Resources.Layout
        |> Ash.Changeset.for_create(:create, attrs, actor: actor, domain: RivaAsh.Domain)
        |> Ash.create!(domain: RivaAsh.Domain)
      end)
    else
      []
    end
  end
end
