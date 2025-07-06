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
      AshGraphql.Resource,
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

  graphql do
    type(:client)

    queries do
      get(:get_client, :read)
      list(:list_clients, :read)
      get(:client_by_email, :by_email)
      list(:registered_clients, :registered)
      list(:unregistered_clients, :unregistered)
    end

    mutations do
      create(:create_client, :create)
      create(:create_client_for_booking, :create_for_booking)
      update(:update_client, :update)
      update(:register_client, :register)
      destroy(:delete_client, :destroy)
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
      
      # Ensure email is provided for registered clients
      validate match_other([:email], [is_registered: true], "is required for registered clients")
      
      # Generate verification token for registered clients
      change fn changeset, _ ->
        if Ash.Changeset.get_attribute(changeset, :is_registered) do
          token = generate_verification_token()
          changeset
          |> Ash.Changeset.force_change_attribute(:verification_token, token)
          |> Ash.Changeset.force_change_attribute(:email_verified, false)
        else
          changeset
        end
      end
      
      # Send verification email for registered clients
      after_action fn _changeset, client ->
        if client.is_registered && client.email do
          # In a real app, you would send a verification email here
          # EmailVerification.send_verification_email(client.email, client.verification_token)
          :ok
        else
          :ok
        end
      end
    end

    # Create unregistered client for booking flow
    create :create_for_booking do
      accept([:name, :email, :phone])
      
      change set_attributes(%{
        is_registered: false,
        email_verified: false
      })
      
      validate present([:name], "is required")
      validate at_least_one_present([:email, :phone], "must provide at least one of email or phone")
      
      description("Create unregistered client during booking process")
    end

    # Register an unregistered client
    update :register do
      accept([:email, :phone, :name])
      
      # Only allow updating unregistered clients
      validate attribute_equals(:is_registered, false, "already registered")
      
      # Require email for registration
      validate present([:email], "is required for registration")
      
      # Generate verification token
      change fn changeset, _ ->
        token = generate_verification_token()
        changeset
        |> Ash.Changeset.force_change_attribute(:is_registered, true)
        |> Ash.Changeset.force_change_attribute(:verification_token, token)
        |> Ash.Changeset.force_change_attribute(:email_verified, false)
      end
      
      # Send verification email
      after_action fn _changeset, client ->
        # In a real app, you would send a verification email here
        # EmailVerification.send_verification_email(client.email, client.verification_token)
        :ok
      end
      
      description("Convert unregistered client to registered client")
    end
    
    # Verify client's email
    update :verify_email do
      accept([:verification_token])
      
      validate attribute_equals(:email_verified, false, "already verified")
      
      change fn changeset, _ ->
        token = Ash.Changeset.get_attribute(changeset, :verification_token)
        
        if token == changeset.data.verification_token do
          changeset
          |> Ash.Changeset.force_change_attribute(:email_verified, true)
          |> Ash.Changeset.force_change_attribute(:verification_token, nil)
        else
          Ash.Changeset.add_error(changeset, "Invalid verification token")
        end
      end
      
      description("Verify client's email address using verification token")
    end
    
    # Find or create client for booking (handles both cases)
    create :find_or_create_for_booking do
      accept([:name, :email, :phone])
      
      validate at_least_one_present([:email, :phone], "must provide at least one of email or phone")
      
      # Use email as upsert key if available, otherwise use phone
      upsert? true
      upsert_identity :email, [:email]
      
      change set_attributes(%{
        is_registered: false,
        email_verified: false
      })
      
      change fn changeset, _ ->
        # If we're updating an existing client, preserve their registration status
        if Ash.Changeset.get_attribute(changeset, :id) do
          changeset
        else
          Ash.Changeset.force_change_attributes(changeset, %{
            is_registered: false,
            email_verified: false
          })
        end
      end
      
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
      constraints [
        min_length: 2,
        max_length: 100,
        trim?: true,
        allow_empty?: false
      ]
    end

    attribute :email, :ci_string do
      allow_nil?(true)
      public?(true)
      description("Email address (required for registered clients)")
      constraints [
        max_length: 255,
        trim?: true,
        match: ~r/^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$/
      ]
    end
    
    attribute :phone, :string do
      allow_nil?(true)
      public?(true)
      description("Contact phone number")
      constraints [
        max_length: 20,
        trim?: true,
        match: ~r/^[\d\s\-\(\)\+]+$/
      ]
    end
    
    attribute :is_registered, :boolean do
      allow_nil?(false)
      default(false)
      public?(true)
      description("Whether the client has completed registration")
    end
    
    attribute :email_verified, :boolean do
      allow_nil?(false)
      default(false)
      public?(true)
      description("Whether the client's email has been verified")
    end
    
    attribute :verification_token, :string do
      allow_nil?(true)
      public?(false)
      description("Token used for email verification")
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
