defmodule RivaAsh.Domain do
  @moduledoc """
  Central Ash domain configuration for the Riva Ash application.

  This module defines the complete domain structure including all resources,
  extensions, and API configurations. It serves as the central hub for
  all domain-related functionality.
  """

  use Ash.Domain,
    extensions: [
      AshJsonApi.Domain,
      AshGraphql.Domain,
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

  json_api do
    prefix("/api")

    open_api do
      # API documentation tags organized by domain area
      # Code readability: Use descriptive tag names
      tag("Authentication")
      tag("Users")
      tag("Businesses")
      tag("Sections")
      tag("Item Types")
      tag("Items")
      tag("Layouts")
      tag("Item Positions")
      tag("Clients")
      tag("Employees")
      tag("Permissions")
      tag("Employee Permissions")
      tag("Reservations")
      tag("Pricing")
      tag("Payments")
      tag("Item Schedules")
      tag("Availability Exceptions")
      tag("Recurring Reservations")
      tag("Recurring Reservation Instances")
      tag("GDPR Consent Records")

      # Group resources logically
      group_by(:resource)

      # Additional OpenAPI configuration
      # security([%{"bearerAuth" => []}])
    end

    # Route configuration for all resources
    routes do
      # Authentication routes
      get("/auth/users", RivaAsh.Accounts.User)
      post("/auth/users", RivaAsh.Accounts.User)
      
      # Business routes
      get("/businesses", RivaAsh.Resources.Business)
      get("/businesses/:id", RivaAsh.Resources.Business)
      post("/businesses", RivaAsh.Resources.Business)
      patch("/businesses/:id", RivaAsh.Resources.Business)
      delete("/businesses/:id", RivaAsh.Resources.Business)
      
      # Plot routes
      get("/plots", RivaAsh.Resources.Plot)
      get("/plots/:id", RivaAsh.Resources.Plot)
      post("/plots", RivaAsh.Resources.Plot)
      patch("/plots/:id", RivaAsh.Resources.Plot)
      delete("/plots/:id", RivaAsh.Resources.Plot)
      
      # Section routes
      get("/sections", RivaAsh.Resources.Section)
      get("/sections/:id", RivaAsh.Resources.Section)
      post("/sections", RivaAsh.Resources.Section)
      patch("/sections/:id", RivaAsh.Resources.Section)
      delete("/sections/:id", RivaAsh.Resources.Section)
      
      # Item Type routes
      get("/item-types", RivaAsh.Resources.ItemType)
      get("/item-types/:id", RivaAsh.Resources.ItemType)
      post("/item-types", RivaAsh.Resources.ItemType)
      patch("/item-types/:id", RivaAsh.Resources.ItemType)
      delete("/item-types/:id", RivaAsh.Resources.ItemType)
      
      # Item routes
      get("/items", RivaAsh.Resources.Item)
      get("/items/:id", RivaAsh.Resources.Item)
      post("/items", RivaAsh.Resources.Item)
      patch("/items/:id", RivaAsh.Resources.Item)
      delete("/items/:id", RivaAsh.Resources.Item)
      
      # Layout routes
      get("/layouts", RivaAsh.Resources.Layout)
      get("/layouts/:id", RivaAsh.Resources.Layout)
      post("/layouts", RivaAsh.Resources.Layout)
      patch("/layouts/:id", RivaAsh.Resources.Layout)
      delete("/layouts/:id", RivaAsh.Resources.Layout)
      
      # Item Position routes
      get("/item-positions", RivaAsh.Resources.ItemPosition)
      get("/item-positions/:id", RivaAsh.Resources.ItemPosition)
      post("/item-positions", RivaAsh.Resources.ItemPosition)
      patch("/item-positions/:id", RivaAsh.Resources.ItemPosition)
      delete("/item-positions/:id", RivaAsh.Resources.ItemPosition)
      
      # Item Hold routes
      get("/item-holds", RivaAsh.Resources.ItemHold)
      get("/item-holds/:id", RivaAsh.Resources.ItemHold)
      post("/item-holds", RivaAsh.Resources.ItemHold)
      patch("/item-holds/:id", RivaAsh.Resources.ItemHold)
      delete("/item-holds/:id", RivaAsh.Resources.ItemHold)
      
      # Client routes
      get("/clients", RivaAsh.Resources.Client)
      get("/clients/:id", RivaAsh.Resources.Client)
      post("/clients", RivaAsh.Resources.Client)
      patch("/clients/:id", RivaAsh.Resources.Client)
      delete("/clients/:id", RivaAsh.Resources.Client)
      
      # Employee routes
      get("/employees", RivaAsh.Resources.Employee)
      get("/employees/:id", RivaAsh.Resources.Employee)
      post("/employees", RivaAsh.Resources.Employee)
      patch("/employees/:id", RivaAsh.Resources.Employee)
      delete("/employees/:id", RivaAsh.Resources.Employee)
      
      # Permission routes
      get("/permissions", RivaAsh.Resources.Permission)
      get("/permissions/:id", RivaAsh.Resources.Permission)
      post("/permissions", RivaAsh.Resources.Permission)
      patch("/permissions/:id", RivaAsh.Resources.Permission)
      delete("/permissions/:id", RivaAsh.Resources.Permission)
      
      # Employee Permission routes
      get("/employee-permissions", RivaAsh.Resources.EmployeePermission)
      get("/employee-permissions/:id", RivaAsh.Resources.EmployeePermission)
      post("/employee-permissions", RivaAsh.Resources.EmployeePermission)
      patch("/employee-permissions/:id", RivaAsh.Resources.EmployeePermission)
      delete("/employee-permissions/:id", RivaAsh.Resources.EmployeePermission)
      
      # Reservation routes
      get("/reservations", RivaAsh.Resources.Reservation)
      get("/reservations/:id", RivaAsh.Resources.Reservation)
      post("/reservations", RivaAsh.Resources.Reservation)
      patch("/reservations/:id", RivaAsh.Resources.Reservation)
      delete("/reservations/:id", RivaAsh.Resources.Reservation)
      
      # Payment routes
      get("/payments", RivaAsh.Resources.Payment)
      get("/payments/:id", RivaAsh.Resources.Payment)
      post("/payments", RivaAsh.Resources.Payment)
      patch("/payments/:id", RivaAsh.Resources.Payment)
      delete("/payments/:id", RivaAsh.Resources.Payment)
      
      # Pricing routes
      get("/pricings", RivaAsh.Resources.Pricing)
      get("/pricings/:id", RivaAsh.Resources.Pricing)
      post("/pricings", RivaAsh.Resources.Pricing)
      patch("/pricings/:id", RivaAsh.Resources.Pricing)
      delete("/pricings/:id", RivaAsh.Resources.Pricing)
      
      # Item Schedule routes
      get("/item-schedules", RivaAsh.Resources.ItemSchedule)
      get("/item-schedules/:id", RivaAsh.Resources.ItemSchedule)
      post("/item-schedules", RivaAsh.Resources.ItemSchedule)
      patch("/item-schedules/:id", RivaAsh.Resources.ItemSchedule)
      delete("/item-schedules/:id", RivaAsh.Resources.ItemSchedule)
      
      # Availability Exception routes
      get("/availability-exceptions", RivaAsh.Resources.AvailabilityException)
      get("/availability-exceptions/:id", RivaAsh.Resources.AvailabilityException)
      post("/availability-exceptions", RivaAsh.Resources.AvailabilityException)
      patch("/availability-exceptions/:id", RivaAsh.Resources.AvailabilityException)
      delete("/availability-exceptions/:id", RivaAsh.Resources.AvailabilityException)
      
      # Recurring Reservation routes
      get("/recurring-reservations", RivaAsh.Resources.RecurringReservation)
      get("/recurring-reservations/:id", RivaAsh.Resources.RecurringReservation)
      post("/recurring-reservations", RivaAsh.Resources.RecurringReservation)
      patch("/recurring-reservations/:id", RivaAsh.Resources.RecurringReservation)
      delete("/recurring-reservations/:id", RivaAsh.Resources.RecurringReservation)
      
      # Recurring Reservation Instance routes
      get("/recurring-reservation-instances", RivaAsh.Resources.RecurringReservationInstance)
      get("/recurring-reservation-instances/:id", RivaAsh.Resources.RecurringReservationInstance)
      post("/recurring-reservation-instances", RivaAsh.Resources.RecurringReservationInstance)
      patch("/recurring-reservation-instances/:id", RivaAsh.Resources.RecurringReservationInstance)
      delete("/recurring-reservation-instances/:id", RivaAsh.Resources.RecurringReservationInstance)
      
      # Chat Room routes
      get("/chat-rooms", RivaAsh.Resources.ChatRoom)
      get("/chat-rooms/:id", RivaAsh.Resources.ChatRoom)
      post("/chat-rooms", RivaAsh.Resources.ChatRoom)
      patch("/chat-rooms/:id", RivaAsh.Resources.ChatRoom)
      delete("/chat-rooms/:id", RivaAsh.Resources.ChatRoom)
      
      # Chat Message routes
      get("/chat-messages", RivaAsh.Resources.ChatMessage)
      get("/chat-messages/:id", RivaAsh.Resources.ChatMessage)
      post("/chat-messages", RivaAsh.Resources.ChatMessage)
      patch("/chat-messages/:id", RivaAsh.Resources.ChatMessage)
      delete("/chat-messages/:id", RivaAsh.Resources.ChatMessage)
      
      # Chat Participant routes
      get("/chat-participants", RivaAsh.Resources.ChatParticipant)
      get("/chat-participants/:id", RivaAsh.Resources.ChatParticipant)
      post("/chat-participants", RivaAsh.Resources.ChatParticipant)
      patch("/chat-participants/:id", RivaAsh.Resources.ChatParticipant)
      delete("/chat-participants/:id", RivaAsh.Resources.ChatParticipant)
      
      # GDPR Consent Record routes
      get("/gdpr/consent-records", RivaAsh.GDPR.ConsentRecord)
      get("/gdpr/consent-records/:id", RivaAsh.GDPR.ConsentRecord)
      post("/gdpr/consent-records", RivaAsh.GDPR.ConsentRecord)
      patch("/gdpr/consent-records/:id", RivaAsh.GDPR.ConsentRecord)
      delete("/gdpr/consent-records/:id", RivaAsh.GDPR.ConsentRecord)
    end
  end

  graphql do
    # Enable authorization at GraphQL layer; resource policies still apply.
    # Ensure your web layer passes the actor into Absinthe context.
    authorize?(true)
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
