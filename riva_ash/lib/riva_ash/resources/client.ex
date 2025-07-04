defmodule RivaAsh.Resources.Client do
  @moduledoc """
  Represents a client who can make reservations.
  Clients can be either registered or unregistered.
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer],
    extensions: [
      AshJsonApi.Resource,
      AshPaperTrail.Resource,
      AshArchival.Resource
    ]

  # Configure versioning for this resource
  paper_trail do
    # Track all changes with full diffs
    change_tracking_mode(:full_diff)

    # Don't store timestamps in the changes
    ignore_attributes([:inserted_at, :updated_at])

    # Store action name for better auditing
    store_action_name?(true)

    # Store action inputs for better auditing
    store_action_inputs?(true)

    # Store resource identifier for better querying
    store_resource_identifier?(true)
  end

  postgres do
    table("clients")
    repo(RivaAsh.Repo)
  end

  # Configure soft delete functionality
  archive do
    # Use archived_at field for soft deletes
    attribute(:archived_at)
    # Allow both soft and hard deletes
    base_filter?(false)
  end

  policies do
    # Admins can do everything
    bypass actor_attribute_equals(:role, :admin) do
      authorize_if(always())
    end

    # Managers can read and manage clients
    policy actor_attribute_equals(:role, :manager) do
      authorize_if(always())
    end

    # Staff can only read clients (for reservation purposes)
    policy actor_attribute_equals(:role, :staff) do
      authorize_if(action_type(:read))
    end

    # Clients can only access their own data
    policy actor_attribute_equals(:__struct__, RivaAsh.Resources.Client) do
      authorize_if(expr(id == ^actor(:id)))
      # Clients can read and update their own data
      forbid_unless(action_type([:read, :update]))
    end

    # Allow public client creation for booking flow
    policy action_type(:create) do
      # Allow booking system to create clients
      authorize_if(always())
    end

    # Allow public read for booking lookups (by email)
    policy action(:by_email) do
      authorize_if(always())
    end
  end

  json_api do
    type("client")

    routes do
      base("/clients")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Additional routes for client-specific actions
      get(:by_email, route: "/by-email/:email")
      get(:registered, route: "/registered")
      get(:unregistered, route: "/unregistered")
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:create_for_booking, action: :create_for_booking)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:register, action: :register)
    define(:by_id, args: [:id], action: :by_id)
    define(:by_email, args: [:email], action: :by_email)
  end

  actions do
    defaults([:read, :update, :destroy])

    create :create do
      accept([:name, :email, :phone, :is_registered])
      primary?(true)
    end

    # Create unregistered client for booking flow
    create :create_for_booking do
      accept([:name, :email, :phone])
      change(set_attribute(:is_registered, false))
      description("Create unregistered client during booking process")
    end

    # Upgrade unregistered client to registered
    update :register do
      accept([:email])
      require_atomic?(false)
      change(set_attribute(:is_registered, true))
      description("Convert unregistered client to registered client")
    end

    # Find or create client for booking (handles both cases)
    create :find_or_create_for_booking do
      accept([:name, :email, :phone])
      upsert?(true)
      upsert_identity(:email)
      change(set_attribute(:is_registered, false))
      description("Find existing client by email or create new unregistered client")
    end

    read :by_id do
      argument(:id, :uuid, allow_nil?: false)
      get?(true)
      filter(expr(id == ^arg(:id)))
    end

    read :by_email do
      argument(:email, :string, allow_nil?: false)
      filter(expr(email == ^arg(:email)))
    end

    read :registered do
      filter(expr(is_registered == true))
    end

    read :unregistered do
      filter(expr(is_registered == false))
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :name, :string do
      allow_nil?(false)
      public?(true)
      description("The name of the client")
    end

    attribute :email, :ci_string do
      allow_nil?(true)
      public?(true)
      description("Email address (required for registered clients)")
    end

    attribute :phone, :string do
      allow_nil?(true)
      public?(true)
      description("Contact phone number")
    end

    attribute :is_registered, :boolean do
      allow_nil?(false)
      default(false)
      public?(true)
      description("Whether this is a registered client")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  identities do
    identity(:email, [:email], eager_check_with: RivaAsh.Domain)
  end

  validations do
    validate(present([:email]), where: attribute_equals(:is_registered, true))
    validate(match(~r/^[^\s]+@[^\s]+$/, :email), where: attribute_equals(:is_registered, true))
    # For booking flow, email is optional for unregistered clients
    validate(present([:name]), message: "Name is required for all clients")
  end

  relationships do
    has_many :reservations, RivaAsh.Resources.Reservation do
      public?(true)
      description("Reservations made by this client")
    end

    has_many :recurring_reservations, RivaAsh.Resources.RecurringReservation do
      destination_attribute(:client_id)
      public?(true)
      description("Recurring reservation patterns for this client")
    end
  end
end
