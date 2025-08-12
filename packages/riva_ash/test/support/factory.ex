alias RivaAsh.Factory, as: Factory
alias RivaAsh.Resources.Business, as: Business
alias RivaAsh.Resources, as: Resources
alias Ash.UUID, as: UUID
alias RivaAsh.Accounts, as: Accounts
alias Ash.Changeset, as: Changeset

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
          bind(string(:alphanumeric, min_length: 5, max_length: 100), fn address ->
            bind(integer(100..10000), fn total_area ->
              bind(member_of(["sqft", "sqm", "acres"]), fn area_unit ->
                %{
                  name: name,
                  description: desc,
                  business_id: business_id,
                  address: address,
                  total_area: Decimal.new(total_area),
                  area_unit: area_unit,
                  is_active: true
                }
                |> Map.merge(overrides)
                |> constant()
              end)
            end)
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

  @doc "Generate attributes for Payment resource"
  def payment_attrs(overrides \\ %{}) do
    bind(uuid(), fn reservation_id ->
      bind(uuid(), fn client_id ->
        bind(integer(10..1000), fn amount ->
          bind(member_of([:pending, :completed, :failed, :refunded]), fn status ->
            bind(future_datetime(), fn paid_at ->
              %{
                reservation_id: reservation_id,
                client_id: client_id,
                amount: amount,
                status: status,
                paid_at: paid_at
              }
              |> Map.merge(overrides)
              |> constant()
            end)
          end)
        end)
      end)
    end)
  end

  @doc "Generate attributes for Pricing resource"
  def pricing_attrs(overrides \\ %{}) do
    bind(uuid(), fn item_id ->
      bind(integer(10..500), fn price ->
        bind(member_of([:hourly, :daily, :weekly, :fixed]), fn pricing_type ->
          bind(boolean(), fn is_active ->
            %{
              item_id: item_id,
              price: price,
              pricing_type: pricing_type,
              is_active: is_active
            }
            |> Map.merge(overrides)
            |> constant()
          end)
        end)
      end)
    end)
  end

  @doc "Generate attributes for ItemPosition resource"
  def item_position_attrs(overrides \\ %{}) do
    bind(uuid(), fn item_id ->
      bind(uuid(), fn section_id ->
        bind(integer(0..100), fn x ->
          bind(integer(0..100), fn y ->
            bind(integer(0..360), fn rotation ->
              %{
                item_id: item_id,
                section_id: section_id,
                x: x,
                y: y,
                rotation: rotation
              }
              |> Map.merge(overrides)
              |> constant()
            end)
          end)
        end)
      end)
    end)
  end

  @doc "Generate attributes for Layout resource"
  def layout_attrs(overrides \\ %{}) do
    bind(string(:alphanumeric, min_length: 2, max_length: 50), fn name ->
      bind(description(), fn desc ->
        bind(uuid(), fn business_id ->
          bind(integer(800..1920), fn width ->
            bind(integer(600..1080), fn height ->
              %{
                name: name,
                description: desc,
                business_id: business_id,
                width: width,
                height: height
              }
              |> Map.merge(overrides)
              |> constant()
            end)
          end)
        end)
      end)
    end)
  end

  @doc "Generate attributes for AvailabilityException resource"
  def availability_exception_attrs(overrides \\ %{}) do
    bind(uuid(), fn item_id ->
      bind(future_datetime(), fn start_time ->
        bind(future_datetime(), fn end_time ->
          bind(string(:alphanumeric, min_length: 10, max_length: 200), fn reason ->
            %{
              item_id: item_id,
              start_time: start_time,
              end_time: end_time,
              reason: reason
            }
            |> Map.merge(overrides)
            |> constant()
          end)
        end)
      end)
    end)
  end

  @doc "Generate attributes for RecurringReservation resource"
  def recurring_reservation_attrs(overrides \\ %{}) do
    bind(uuid(), fn client_id ->
      bind(uuid(), fn item_id ->
        bind(future_datetime(), fn start_time ->
          bind(future_datetime(), fn end_time ->
            bind(member_of([:daily, :weekly, :monthly]), fn frequency ->
              bind(integer(1..52), fn occurrences ->
                %{
                  client_id: client_id,
                  item_id: item_id,
                  start_time: start_time,
                  end_time: end_time,
                  frequency: frequency,
                  occurrences: occurrences
                }
                |> Map.merge(overrides)
                |> constant()
              end)
            end)
          end)
        end)
      end)
    end)
  end

  @doc "Generate attributes for RecurringReservationInstance resource"
  def recurring_reservation_instance_attrs(overrides \\ %{}) do
    bind(uuid(), fn recurring_reservation_id ->
      bind(future_datetime(), fn reserved_from ->
        bind(future_datetime(), fn reserved_until ->
          bind(member_of([:pending, :confirmed, :cancelled]), fn status ->
            %{
              recurring_reservation_id: recurring_reservation_id,
              reserved_from: reserved_from,
              reserved_until: reserved_until,
              status: status
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
        :payment -> payment_attrs(overrides) |> Enum.take(1) |> hd()
        :pricing -> pricing_attrs(overrides) |> Enum.take(1) |> hd()
        :item_position -> item_position_attrs(overrides) |> Enum.take(1) |> hd()
        :layout -> layout_attrs(overrides) |> Enum.take(1) |> hd()
        :availability_exception -> availability_exception_attrs(overrides) |> Enum.take(1) |> hd()
        :recurring_reservation -> recurring_reservation_attrs(overrides) |> Enum.take(1) |> hd()
        :recurring_reservation_instance -> recurring_reservation_instance_attrs(overrides) |> Enum.take(1) |> hd()
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
      :payment -> Payment.create(attrs)
      :pricing -> Pricing.create(attrs)
      :item_position -> ItemPosition.create(attrs)
      :layout -> Layout.create(attrs)
      :availability_exception -> AvailabilityException.create(attrs)
      :recurring_reservation -> RecurringReservation.create(attrs)
      :recurring_reservation_instance -> RecurringReservationInstance.create(attrs)
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
      :payment -> payment_attrs(overrides) |> Enum.take(count)
      :pricing -> pricing_attrs(overrides) |> Enum.take(count)
      :item_position -> item_position_attrs(overrides) |> Enum.take(count)
      :layout -> layout_attrs(overrides) |> Enum.take(count)
      :availability_exception -> availability_exception_attrs(overrides) |> Enum.take(count)
      :recurring_reservation -> recurring_reservation_attrs(overrides) |> Enum.take(count)
      :recurring_reservation_instance -> recurring_reservation_instance_attrs(overrides) |> Enum.take(count)
    end
  end

  @doc """
  Generate sample data for testing with proper relationships.
  """
  def sample_data do
    # Create a business first as it's the root entity
    business_attrs = business_attrs() |> Enum.take(1) |> hd()
    {:ok, business} = Business.create(business_attrs)

    # Create plot associated with business
    plot_attrs = plot_attrs(%{business_id: business.id}) |> Enum.take(1) |> hd()
    {:ok, plot} = Plot.create(plot_attrs)

    # Create section associated with plot
    section_attrs = section_attrs(%{plot_id: plot.id}) |> Enum.take(1) |> hd()
    {:ok, section} = Section.create(section_attrs)

    # Create item type associated with business
    item_type_attrs = item_type_attrs(%{business_id: business.id}) |> Enum.take(1) |> hd()
    {:ok, item_type} = ItemType.create(item_type_attrs)

    # Create layout associated with business
    layout_attrs = layout_attrs(%{business_id: business.id}) |> Enum.take(1) |> hd()
    {:ok, layout} = Layout.create(layout_attrs)

    # Create item associated with section and item type
    item_attrs = item_attrs(%{section_id: section.id, item_type_id: item_type.id}) |> Enum.take(1) |> hd()
    {:ok, item} = Item.create(item_attrs)

    # Create item position for the item
    item_position_attrs = item_position_attrs(%{item_id: item.id, section_id: section.id}) |> Enum.take(1) |> hd()
    {:ok, item_position} = ItemPosition.create(item_position_attrs)

    # Create pricing for the item
    pricing_attrs = pricing_attrs(%{item_id: item.id}) |> Enum.take(1) |> hd()
    {:ok, pricing} = Pricing.create(pricing_attrs)

    # Create client
    client_attrs = client_attrs() |> Enum.take(1) |> hd()
    {:ok, client} = Client.create(client_attrs)

    # Create employee associated with business
    employee_attrs = employee_attrs(%{business_id: business.id}) |> Enum.take(1) |> hd()
    {:ok, employee} = Employee.create(employee_attrs)

    # Create reservation
    reservation_attrs = reservation_attrs(%{client_id: client.id, item_id: item.id, employee_id: employee.id}) |> Enum.take(1) |> hd()
    {:ok, reservation} = Reservation.create(reservation_attrs)

    # Create payment for the reservation
    payment_attrs = payment_attrs(%{reservation_id: reservation.id, client_id: client.id}) |> Enum.take(1) |> hd()
    {:ok, payment} = Payment.create(payment_attrs)

    # Create availability exception
    availability_exception_attrs = availability_exception_attrs(%{item_id: item.id}) |> Enum.take(1) |> hd()
    {:ok, availability_exception} = AvailabilityException.create(availability_exception_attrs)

    # Create recurring reservation
    recurring_reservation_attrs = recurring_reservation_attrs(%{client_id: client.id, item_id: item.id}) |> Enum.take(1) |> hd()
    {:ok, recurring_reservation} = RecurringReservation.create(recurring_reservation_attrs)

    # Create recurring reservation instance
    recurring_reservation_instance_attrs = recurring_reservation_instance_attrs(%{recurring_reservation_id: recurring_reservation.id}) |> Enum.take(1) |> hd()
    {:ok, recurring_reservation_instance} = RecurringReservationInstance.create(recurring_reservation_instance_attrs)

    %{
      business: business,
      plot: plot,
      section: section,
      item_type: item_type,
      layout: layout,
      item: item,
      item_position: item_position,
      pricing: pricing,
      client: client,
      employee: employee,
      reservation: reservation,
      payment: payment,
      availability_exception: availability_exception,
      recurring_reservation: recurring_reservation,
      recurring_reservation_instance: recurring_reservation_instance
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

  # =============================================================================
  # Helper Functions for Resource Type Detection
  # =============================================================================

  defp get_resource_type(%RivaAsh.Accounts.User{}), do: :user
  defp get_resource_type(%Business{}), do: :business
  defp get_resource_type(%Client{}), do: :client
  defp get_resource_type(%Item{}), do: :item
  defp get_resource_type(%Employee{}), do: :employee
  defp get_resource_type(%Payment{}), do: :payment
  defp get_resource_type(%Pricing{}), do: :pricing
  defp get_resource_type(%ItemPosition{}), do: :item_position
  defp get_resource_type(%Layout{}), do: :layout
  defp get_resource_type(%AvailabilityException{}), do: :availability_exception
  defp get_resource_type(%RecurringReservation{}), do: :recurring_reservation
  defp get_resource_type(%RecurringReservationInstance{}), do: :recurring_reservation_instance
  defp get_unmatchedresource_unmatchedtype(_unmatched), do: :unknown
end
