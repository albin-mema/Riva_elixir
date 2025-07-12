defmodule RivaAsh.Factory do
  @moduledoc """
  Factory module for generating test data using StreamData for property-based testing.

  This module provides generators for all Ash resources in the system,
  allowing tests to use randomized data instead of hardcoded values.

  ## Usage

      # Generate a single business
      business = RivaAsh.Factory.business() |> Enum.take(1) |> hd()

      # Generate multiple businesses
      businesses = RivaAsh.Factory.business() |> Enum.take(5)

      # Generate business with specific attributes
      business = RivaAsh.Factory.business(%{name: "Specific Name"}) |> Enum.take(1) |> hd()

      # Use in property-based tests
      property "business names are always valid" do
        check all business_attrs <- RivaAsh.Factory.business_attrs() do
          assert {:ok, business} = RivaAsh.Resources.Business.create(business_attrs)
          assert String.length(business.name) >= 2
        end
      end
  """

  use ExUnitProperties
  use Timex
  import StreamData

  alias RivaAsh.Resources.{
    Business, Plot, Section, ItemType, Item, Layout, ItemPosition,
    Client, Employee, Permission, EmployeePermission, Reservation,
    Pricing, Payment, ItemSchedule, AvailabilityException,
    RecurringReservation, RecurringReservationInstance
  }

  # ============================================================================
  # Basic Data Generators
  # ============================================================================

  @doc "Generate valid business names"
  def business_name do
    one_of([
      string(:alphanumeric, min_length: 2, max_length: 50),
      fixed_list([
        "Acme Corp", "Tech Solutions Inc", "Green Valley Farm", "City Cafe",
        "Mountain View Resort", "Ocean Breeze Hotel", "Downtown Fitness",
        "Sunset Restaurant", "Pine Tree Lodge", "River Side Marina"
      ]) |> member_of()
    ])
  end

  @doc "Generate valid email addresses"
  def email do
    bind(string(:alphanumeric, min_length: 3, max_length: 10), fn username ->
      bind(string(:alphanumeric, min_length: 3, max_length: 10), fn domain ->
        bind(member_of(["com", "org", "net", "edu", "gov"]), fn tld ->
          constant("#{username}@#{domain}.#{tld}")
        end)
      end)
    end)
  end

  @doc "Generate valid phone numbers"
  def phone do
    one_of([
      # US format
      bind(integer(100..999), fn area ->
        bind(integer(100..999), fn exchange ->
          bind(integer(1000..9999), fn number ->
            constant("(#{area}) #{exchange}-#{number}")
          end)
        end)
      end),
      # International format
      bind(integer(1..999), fn country ->
        bind(integer(1000000000..9999999999), fn number ->
          constant("+#{country} #{number}")
        end)
      end),
      # Simple format
      bind(integer(1000000000..9999999999), fn number ->
        constant("#{number}")
      end)
    ])
  end

  @doc "Generate valid descriptions"
  def description do
    one_of([
      constant(nil),
      string(:alphanumeric, min_length: 10, max_length: 200),
      fixed_list([
        constant("A wonderful place to visit and enjoy quality time"),
        constant("Professional services with excellent customer care"),
        constant("Modern facilities with state-of-the-art equipment"),
        constant("Family-friendly environment with great amenities"),
        constant("Convenient location with easy access and parking")
      ]) |> member_of()
    ])
  end

  @doc "Generate future datetime"
  def future_datetime do
    bind(integer(1..365), fn days ->
      bind(integer(0..23), fn hours ->
        bind(integer(0..59), fn minutes ->
          base_time = Timex.now()
          Timex.shift(base_time, days: days, hours: hours, minutes: minutes)
          |> constant()
        end)
      end)
    end)
  end

  @doc "Generate past datetime"
  def past_datetime do
    bind(integer(1..365), fn days ->
      bind(integer(0..23), fn hours ->
        bind(integer(0..59), fn minutes ->
          base_time = Timex.now()
          Timex.shift(base_time, days: -days, hours: -hours, minutes: -minutes)
          |> constant()
        end)
      end)
    end)
  end

  @doc "Generate valid UUIDs"
  def uuid do
    bind(binary(length: 16), fn bytes ->
      bytes
      |> Base.encode16(case: :lower)
      |> String.replace(~r/(.{8})(.{4})(.{4})(.{4})(.{12})/, "\\1-\\2-\\3-\\4-\\5")
      |> constant()
    end)
  end

  # ============================================================================
  # Resource Attribute Generators
  # ============================================================================

  @doc "Generate attributes for Business resource"
  def business_attrs(overrides \\ %{}) do
    bind(business_name(), fn name ->
      bind(description(), fn desc ->
        bind(uuid(), fn owner_id ->
          %{
            name: name,
            description: desc,
            owner_id: owner_id
          }
          |> Map.merge(overrides)
          |> constant()
        end)
      end)
    end)
  end

  @doc "Generate attributes for Client resource"
  def client_attrs(overrides \\ %{}) do
    bind(string(:alphanumeric, min_length: 2, max_length: 50), fn name ->
      bind(email(), fn email_addr ->
        bind(phone(), fn phone_num ->
          bind(boolean(), fn is_registered ->
            # Apply overrides first to check if is_registered is being overridden
            base_attrs = %{
              name: name,
              phone: phone_num,
              is_registered: is_registered
            }

            merged_attrs = Map.merge(base_attrs, overrides)
            final_is_registered = Map.get(merged_attrs, :is_registered, is_registered)

            # Ensure registered clients have email, unregistered can have nil
            final_email = if final_is_registered do
              Map.get(overrides, :email, email_addr)
            else
              Map.get(overrides, :email, if(is_registered, do: email_addr, else: nil))
            end

            %{
              name: merged_attrs.name,
              email: final_email,
              phone: merged_attrs.phone,
              is_registered: final_is_registered
            }
            |> constant()
          end)
        end)
      end)
    end)
  end

  @doc "Generate attributes for Item resource"
  def item_attrs(overrides \\ %{}) do
    bind(string(:alphanumeric, min_length: 2, max_length: 50), fn name ->
      bind(description(), fn desc ->
        bind(integer(1..100), fn capacity ->
          bind(one_of([constant(nil), uuid()]), fn section_id ->
            bind(one_of([constant(nil), uuid()]), fn item_type_id ->
              %{
                name: name,
                description: desc,
                capacity: capacity,
                section_id: section_id,
                item_type_id: item_type_id
              }
              |> Map.merge(overrides)
              |> constant()
            end)
          end)
        end)
      end)
    end)
  end

  @doc "Generate attributes for Reservation resource"
  def reservation_attrs(overrides \\ %{}) do
    bind(uuid(), fn client_id ->
      bind(uuid(), fn item_id ->
        bind(future_datetime(), fn reserved_from ->
          bind(future_datetime(), fn reserved_until ->
            bind(member_of([:pending, :provisional, :confirmed, :cancelled, :completed]), fn status ->
              bind(description(), fn notes ->
                bind(one_of([constant(nil), uuid()]), fn employee_id ->
                  # Ensure reserved_until is after reserved_from
                  actual_until = if Timex.compare(reserved_until, reserved_from) == 1 do
                    reserved_until
                  else
                    Timex.shift(reserved_from, hours: 1)  # Add 1 hour
                  end

                  %{
                    client_id: client_id,
                    item_id: item_id,
                    employee_id: employee_id,
                    reserved_from: reserved_from,
                    reserved_until: actual_until,
                    status: status,
                    notes: notes
                  }
                  |> Map.merge(overrides)
                  |> constant()
                end)
              end)
            end)
          end)
        end)
      end)
    end)
  end

  @doc "Generate attributes for Plot resource"
  def plot_attrs(overrides \\ %{}) do
    bind(string(:alphanumeric, min_length: 2, max_length: 50), fn name ->
      bind(description(), fn desc ->
        bind(uuid(), fn business_id ->
          bind(string(:alphanumeric, min_length: 5, max_length: 100), fn location ->
            %{
              name: name,
              description: desc,
              business_id: business_id,
              location: location
            }
            |> Map.merge(overrides)
            |> constant()
          end)
        end)
      end)
    end)
  end

  @doc "Generate attributes for Section resource"
  def section_attrs(overrides \\ %{}) do
    bind(string(:alphanumeric, min_length: 2, max_length: 50), fn name ->
      bind(description(), fn desc ->
        bind(uuid(), fn plot_id ->
          %{
            name: name,
            description: desc,
            plot_id: plot_id
          }
          |> Map.merge(overrides)
          |> constant()
        end)
      end)
    end)
  end

  @doc "Generate attributes for ItemType resource"
  def item_type_attrs(overrides \\ %{}) do
    bind(string(:alphanumeric, min_length: 2, max_length: 50), fn name ->
      bind(description(), fn desc ->
        bind(uuid(), fn business_id ->
          bind(integer(1..100), fn default_capacity ->
            %{
              name: name,
              description: desc,
              business_id: business_id,
              default_capacity: default_capacity
            }
            |> Map.merge(overrides)
            |> constant()
          end)
        end)
      end)
    end)
  end

  @doc "Generate attributes for Employee resource"
  def employee_attrs(overrides \\ %{}) do
    bind(string(:alphanumeric, min_length: 2, max_length: 50), fn name ->
      bind(email(), fn email_addr ->
        bind(uuid(), fn business_id ->
          bind(member_of([:admin, :manager, :staff]), fn role ->
            %{
              name: name,
              email: email_addr,
              business_id: business_id,
              role: role
            }
            |> Map.merge(overrides)
            |> constant()
          end)
        end)
      end)
    end)
  end

  # ============================================================================
  # Resource Generators (for creating actual records)
  # ============================================================================

  @doc "Generate Business records"
  def business(overrides \\ %{}) do
    business_attrs(overrides)
  end

  @doc "Generate Client records"
  def client(overrides \\ %{}) do
    client_attrs(overrides)
  end

  @doc "Generate Item records"
  def item(overrides \\ %{}) do
    item_attrs(overrides)
  end

  @doc "Generate Reservation records"
  def reservation(overrides \\ %{}) do
    reservation_attrs(overrides)
  end

  @doc "Generate Plot records"
  def plot(overrides \\ %{}) do
    plot_attrs(overrides)
  end

  @doc "Generate Section records"
  def section(overrides \\ %{}) do
    section_attrs(overrides)
  end

  @doc "Generate ItemType records"
  def item_type(overrides \\ %{}) do
    item_type_attrs(overrides)
  end

  @doc "Generate Employee records"
  def employee(overrides \\ %{}) do
    employee_attrs(overrides)
  end

  # ============================================================================
  # Helper Functions
  # ============================================================================

  @doc """
  Create a resource using generated attributes.

  ## Examples

      {:ok, business} = RivaAsh.Factory.create(:business)
      {:ok, client} = RivaAsh.Factory.create(:client, %{name: "John Doe"})
  """
  def create(resource_type, overrides \\ %{}) do
    attrs = case resource_type do
      :business -> business_attrs(overrides) |> Enum.take(1) |> hd()
      :client -> client_attrs(overrides) |> Enum.take(1) |> hd()
      :item -> item_attrs(overrides) |> Enum.take(1) |> hd()
      :reservation -> reservation_attrs(overrides) |> Enum.take(1) |> hd()
      :plot -> plot_attrs(overrides) |> Enum.take(1) |> hd()
      :section -> section_attrs(overrides) |> Enum.take(1) |> hd()
      :item_type -> item_type_attrs(overrides) |> Enum.take(1) |> hd()
      :employee -> employee_attrs(overrides) |> Enum.take(1) |> hd()
    end

    case resource_type do
      :business -> Business.create(attrs)
      :client -> Client.create(attrs)
      :item -> Item.create(attrs)
      :reservation -> Reservation.create(attrs)
      :plot -> Plot.create(attrs)
      :section -> Section.create(attrs)
      :item_type -> ItemType.create(attrs)
      :employee -> Employee.create(attrs)
    end
  end

  @doc """
  Create a resource and return it, raising on error.
  """
  def create!(resource_type, overrides \\ %{}) do
    case create(resource_type, overrides) do
      {:ok, resource} -> resource
      {:error, error} -> raise "Failed to create #{resource_type}: #{inspect(error)}"
    end
  end

  @doc """
  Generate a list of attributes for a given resource type.
  """
  def attrs_list(resource_type, count, overrides \\ %{}) do
    case resource_type do
      :business -> business_attrs(overrides) |> Enum.take(count)
      :client -> client_attrs(overrides) |> Enum.take(count)
      :item -> item_attrs(overrides) |> Enum.take(count)
      :reservation -> reservation_attrs(overrides) |> Enum.take(count)
      :plot -> plot_attrs(overrides) |> Enum.take(count)
      :section -> section_attrs(overrides) |> Enum.take(count)
      :item_type -> item_type_attrs(overrides) |> Enum.take(count)
      :employee -> employee_attrs(overrides) |> Enum.take(count)
    end
  end

  @doc """
  Generate sample data for testing with proper relationships.

  This creates a complete hierarchy: Business -> Plot -> Section -> Item
  """
  def sample_data do
    # Create business first
    business_attrs = business_attrs() |> Enum.take(1) |> hd()
    {:ok, business} = Business.create(business_attrs)

    # Create plot for the business
    plot_attrs = plot_attrs(%{business_id: business.id}) |> Enum.take(1) |> hd()
    {:ok, plot} = Plot.create(plot_attrs)

    # Create section for the plot
    section_attrs = section_attrs(%{plot_id: plot.id}) |> Enum.take(1) |> hd()
    {:ok, section} = Section.create(section_attrs)

    # Create item type for the business
    item_type_attrs = item_type_attrs(%{business_id: business.id}) |> Enum.take(1) |> hd()
    {:ok, item_type} = ItemType.create(item_type_attrs)

    # Create item for the section
    item_attrs = item_attrs(%{section_id: section.id, item_type_id: item_type.id}) |> Enum.take(1) |> hd()
    {:ok, item} = Item.create(item_attrs)

    # Create client
    client_attrs = client_attrs() |> Enum.take(1) |> hd()
    {:ok, client} = Client.create(client_attrs)

    %{
      business: business,
      plot: plot,
      section: section,
      item_type: item_type,
      item: item,
      client: client
    }
  end
end
