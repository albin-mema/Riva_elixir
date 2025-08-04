defmodule RivaAsh.Factory do
  @moduledoc """
  Factory and generators for core resources.

  This file augments existing generators with concise builders and a business context.

  Usage examples:

      # Build or insert core entities with minimal boilerplate
      attrs = RivaAsh.Factory.business_attrs() |> Enum.take(1) |> hd()
      {:ok, business} = RivaAsh.Resources.Business.create(attrs)

      # Build a ready-to-use business context (admin user + business)
      ctx = RivaAsh.Factory.build(:business_context)
      # or persist related entities
      ctx = RivaAsh.Factory.insert(:business_context)

  The business_context contains:
    - :user (admin)
    - :business (owned by user)
  """

  use ExUnitProperties
  use Timex
  import StreamData

  alias RivaAsh.Resources.{
    Business,
    Plot,
    Section,
    ItemType,
    Item,
    Layout,
    ItemPosition,
    Client,
    Employee,
    Permission,
    EmployeePermission,
    Reservation,
    Pricing,
    Payment,
    ItemSchedule,
    AvailabilityException,
    RecurringReservation,
    RecurringReservationInstance
  }

  # =============================================================================
  # Existing Generators (kept) + Minimal Additions
  # =============================================================================

  @doc "Generate valid business names"
  def business_name do
    one_of([
      string(:alphanumeric, min_length: 2, max_length: 50),
      member_of([
        "Acme Corp",
        "Tech Solutions Inc",
        "Green Valley Farm",
        "City Cafe",
        "Mountain View Resort",
        "Ocean Breeze Hotel",
        "Downtown Fitness",
        "Sunset Restaurant",
        "Pine Tree Lodge",
        "River Side Marina"
      ])
    ])
  end

  @doc "Generate valid email addresses"
  def email do
    bind(string(:alphanumeric, min_length: 3, max_length: 10), fn username ->
      bind(string(:alphanumeric, min_length: 3, max_length: 10), fn domain ->
        bind(member_of(["com", "org", "net", "edu", "gov"]), fn tld ->
          uuid = Ash.UUID.generate()
          constant("#{username}#{uuid}@#{domain}.#{tld}")
        end)
      end)
    end)
  end

  @doc "Generate valid phone numbers"
  def phone do
    one_of([
      bind(integer(100..999), fn area ->
        bind(integer(100..999), fn exchange ->
          bind(integer(1000..9999), fn number ->
            constant("(#{area}) #{exchange}-#{number}")
          end)
        end)
      end),
      bind(integer(1..999), fn country ->
        bind(integer(1_000_000_000..9_999_999_999), fn number ->
          constant("+#{country} #{number}")
        end)
      end),
      bind(integer(1_000_000_000..9_999_999_999), fn number ->
        constant("#{number}")
      end)
    ])
  end

  @doc "Generate valid descriptions"
  def description do
    one_of([
      constant(nil),
      string(:alphanumeric, min_length: 10, max_length: 200),
      member_of([
        "A wonderful place to visit and enjoy quality time",
        "Professional services with excellent customer care",
        "Modern facilities with state-of-the-art equipment",
        "Family-friendly environment with great amenities",
        "Convenient location with easy access and parking"
      ])
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

  # =============================================================================
  # Resource Attribute Generators (kept)
  # =============================================================================

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
            base_attrs = %{
              name: name,
              phone: phone_num,
              is_registered: is_registered
            }

            merged_attrs = Map.merge(base_attrs, overrides)
            final_is_registered = Map.get(merged_attrs, :is_registered, is_registered)

            final_email =
              if final_is_registered do
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
            bind(
              member_of([:pending, :provisional, :confirmed, :cancelled, :completed]),
              fn status ->
                bind(description(), fn notes ->
                  bind(one_of([constant(nil), uuid()]), fn employee_id ->
                    actual_until =
                      if Timex.compare(reserved_until, reserved_from) == 1 do
                        reserved_until
                      else
                        Timex.shift(reserved_from, hours: 1)
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
              end
            )
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

  # =============================================================================
  # Minimal Builders for Core Phase 1 resources (non-persistent generators already exist)
  # =============================================================================

  @doc """
  Create a resource using generated attributes.
  """
  def create(resource_type, overrides \\ %{}) do
    attrs =
      case resource_type do
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

  @doc "Create and raise on error."
  def create!(resource_type, overrides \\ %{}) do
    case create(resource_type, overrides) do
      {:ok, resource} -> resource
      {:error, error} -> raise "Failed to create #{resource_type}: #{inspect(error)}"
    end
  end

  @doc "Generate a list of attributes for a given resource type."
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
  """
  def sample_data do
    business_attrs = business_attrs() |> Enum.take(1) |> hd()
    {:ok, business} = Business.create(business_attrs)

    plot_attrs = plot_attrs(%{business_id: business.id}) |> Enum.take(1) |> hd()
    {:ok, plot} = Plot.create(plot_attrs)

    section_attrs = section_attrs(%{plot_id: plot.id}) |> Enum.take(1) |> hd()
    {:ok, section} = Section.create(section_attrs)

    item_type_attrs = item_type_attrs(%{business_id: business.id}) |> Enum.take(1) |> hd()
    {:ok, item_type} = ItemType.create(item_type_attrs)

    item_attrs =
      item_attrs(%{section_id: section.id, item_type_id: item_type.id}) |> Enum.take(1) |> hd()

    {:ok, item} = Item.create(item_attrs)

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

  # =============================================================================
  # Business Context Builder (admin user + business)
  # =============================================================================

  @doc """
  Build a concise business context without persisting non-Ash records.

  Returns a map with at least:
    - :user (admin)
    - :business

  Uses the Accounts domain to register the user.
  """
  def build(:business_context) do
    user =
      RivaAsh.Accounts.User
      |> Ash.Changeset.for_create(:register_with_password, %{
        name: "Admin #{System.unique_integer([:positive])}",
        email: "admin#{System.unique_integer([:positive])}@example.com",
        password: "password123",
        role: :admin
      })
      |> Ash.create!(domain: RivaAsh.Accounts)

    business =
      Business
      |> Ash.Changeset.for_create(:create, %{
        name: "Business #{System.unique_integer([:positive])}",
        description: "Test Business",
        owner_id: user.id
      })
      |> Ash.create!()

    %{user: user, business: business}
  end

  @doc """
  Insert (persist) a business context (admin user + owned business).
  Same as build/1 since both create persisted Ash resources.
  """
  def insert(:business_context), do: build(:business_context)
end
