defmodule RivaAsh.Domain do
  @moduledoc """
  Central Ash domain configuration for the Riva Ash application.

  This module defines the complete domain structure including all resources,
  extensions, and API configurations. It serves as the central hub for
  all domain-related functionality.
  """

  use Ash.Domain,
    extensions: [
      AshAdmin.Domain,
      AshPaperTrail.Domain
    ]



  # Configure AshPaperTrail to include version tables in the domain
  # Functional programming patterns: Use clear, declarative configuration
  paper_trail do
    # Auto-include all version resources
    include_versions?(true)
  end

  resources do
    # Authentication and authorization resources
    resource(RivaAsh.Accounts.User) do
      define(:get_user_by_id, action: :seed_read, get_by: [:id])
    end

    resource(RivaAsh.Accounts.Token)

    # Core business domain resources
    resource(RivaAsh.Resources.Business) do
      define(:list_businesses, action: :read)
      define(:get_business_by_id, action: :read, get_by: [:id])
    end

    resource(RivaAsh.Resources.Plot) do
      define(:list_plots, action: :read)
      define(:get_plot_by_id, action: :read, get_by: [:id])
    end

    resource(RivaAsh.Resources.Section) do
      define(:list_sections, action: :read)
      define(:get_section_by_id, action: :read, get_by: [:id])
    end

    resource(RivaAsh.Resources.ItemType) do
      define(:list_item_types, action: :read)
      define(:get_item_type_by_id, action: :read, get_by: [:id])
    end

    resource(RivaAsh.Resources.Item) do
      define(:list_items, action: :read)
      define(:get_item_by_id, action: :read, get_by: [:id])
    end

    resource(RivaAsh.Resources.Layout) do
      define(:list_layouts, action: :read)
      define(:get_layout_by_id, action: :read, get_by: [:id])
    end

    resource(RivaAsh.Resources.ItemPosition) do
      define(:list_item_positions, action: :read)
      define(:get_item_position_by_id, action: :read, get_by: [:id])
    end

    resource(RivaAsh.Resources.ItemHold) do
      define(:list_item_holds, action: :read)
      define(:get_item_hold_by_id, action: :read, get_by: [:id])
    end

    resource(RivaAsh.Resources.Client) do
      define(:list_clients, action: :read)
      define(:get_client_by_id, action: :read, get_by: [:id])
      define(:get_client_by_email, action: :read, get_by: [:email])
    end

    resource(RivaAsh.Resources.Employee) do
      define(:list_employees, action: :read)
      define(:get_employee_by_id, action: :read, get_by: [:id])
    end

    resource(RivaAsh.Resources.Permission) do
      define(:list_permissions, action: :read)
      define(:get_permission_by_id, action: :read, get_by: [:id])
    end

    resource(RivaAsh.Resources.EmployeePermission) do
      define(:list_employee_permissions, action: :read)
      define(:get_employee_permission_by_id, action: :read, get_by: [:id])
    end

    # Reservation and booking resources
    resource(RivaAsh.Resources.Reservation) do
      define(:list_reservations, action: :read)
      define(:get_reservation_by_id, action: :read, get_by: [:id])
    end

    resource(RivaAsh.Resources.Payment) do
      define(:list_payments, action: :read)
      define(:get_payment_by_id, action: :read, get_by: [:id])
    end

    resource(RivaAsh.Resources.Pricing) do
      define(:list_pricings, action: :read)
      define(:get_pricing_by_id, action: :read, get_by: [:id])
    end

    resource(RivaAsh.Resources.ItemSchedule) do
      define(:list_item_schedules, action: :read)
      define(:get_item_schedule_by_id, action: :read, get_by: [:id])
    end

    resource(RivaAsh.Resources.AvailabilityException) do
      define(:list_availability_exceptions, action: :read)
      define(:get_availability_exception_by_id, action: :read, get_by: [:id])
    end

    resource(RivaAsh.Resources.RecurringReservation) do
      define(:list_recurring_reservations, action: :read)
      define(:get_recurring_reservation_by_id, action: :read, get_by: [:id])
    end

    resource(RivaAsh.Resources.RecurringReservationInstance) do
      define(:list_recurring_reservation_instances, action: :read)
      define(:get_recurring_reservation_instance_by_id, action: :read, get_by: [:id])
    end

    # Communication resources
    resource(RivaAsh.Resources.ChatRoom) do
      define(:list_chat_rooms, action: :read)
      define(:get_chat_room_by_id, action: :read, get_by: [:id])
    end

    resource(RivaAsh.Resources.ChatMessage) do
      define(:list_chat_messages, action: :read)
      define(:get_chat_message_by_id, action: :read, get_by: [:id])
    end

    resource(RivaAsh.Resources.ChatParticipant) do
      define(:list_chat_participants, action: :read)
      define(:get_chat_participant_by_id, action: :read, get_by: [:id])
    end

    # PaperTrail version resources for communication
    resource(RivaAsh.Resources.ChatRoom.Version)
    resource(RivaAsh.Resources.ChatMessage.Version)
    resource(RivaAsh.Resources.ChatParticipant.Version)

    # GDPR compliance resources
    resource(RivaAsh.GDPR.ConsentRecord) do
      define(:list_consent_records, action: :read)
      define(:get_consent_record_by_id, action: :read, get_by: [:id])
    end
  end



  admin do
    show?(true)
  end

  # Public API functions for domain management
  # Type safety: Use proper type specifications for public functions

  @spec get_resource(atom()) :: module() | nil
  def get_resource(_resource_name) do
    # Helper function to get resource by name
    nil
  end

  @spec list_resources() :: [module()]
  def list_resources do
    # List all resources in the domain
    [
      RivaAsh.Accounts.User,
      RivaAsh.Accounts.Token,
      RivaAsh.Resources.Business,
      RivaAsh.Resources.Plot,
      RivaAsh.Resources.Section,
      RivaAsh.Resources.ItemType,
      RivaAsh.Resources.Item,
      RivaAsh.Resources.Layout,
      RivaAsh.Resources.ItemPosition,
      RivaAsh.Resources.ItemHold,
      RivaAsh.Resources.Client,
      RivaAsh.Resources.Employee,
      RivaAsh.Resources.Permission,
      RivaAsh.Resources.EmployeePermission,
      RivaAsh.Resources.Reservation,
      RivaAsh.Resources.Payment,
      RivaAsh.Resources.Pricing,
      RivaAsh.Resources.ItemSchedule,
      RivaAsh.Resources.AvailabilityException,
      RivaAsh.Resources.RecurringReservation,
      RivaAsh.Resources.RecurringReservationInstance,
      RivaAsh.Resources.ChatRoom,
      RivaAsh.Resources.ChatMessage,
      RivaAsh.Resources.ChatParticipant,
      RivaAsh.GDPR.ConsentRecord
    ]
  end

  @spec get_api_config() :: map()
  def get_api_config do
    %{
      json_api: %{
        prefix: "/api",
        open_api: %{
          tags: ["Authentication", "Users", "Businesses", "Reservations"],
          group_by: :resource
        }
      },
      graphql: %{
        authorize?: true,
        schema: ["http://localhost:4000/graphql"],
        persisted_queries?: true,
        batched?: true
      }
    }
  end
end
