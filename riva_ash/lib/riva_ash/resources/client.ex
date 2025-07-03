defmodule RivaAsh.Resources.Client do
  @moduledoc """
  Represents a client who can make reservations.
  Clients can be either registered or unregistered.
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    extensions: [
      AshJsonApi.Resource,
      AshPaperTrail.Resource,
      AshArchival.Resource
    ]

  # Configure versioning for this resource
  paper_trail do
    # Track all changes with full diffs
    change_tracking_mode :full_diff

    # Don't store timestamps in the changes
    ignore_attributes [:inserted_at, :updated_at]

    # Store action name for better auditing
    store_action_name? true

    # Store action inputs for better auditing
    store_action_inputs? true

    # Store resource identifier for better querying
    store_resource_identifier? true
  end

  postgres do
    table "clients"
    repo RivaAsh.Repo
  end

  # Configure soft delete functionality
  archive do
    # Use archived_at field for soft deletes
    attribute :archived_at
    # Allow both soft and hard deletes
    base_filter? false
  end

  json_api do
    type "client"

    routes do
      base "/clients"

      get :read
      index :read
      post :create
      patch :update
      delete :destroy

      # Additional routes for client-specific actions
      get :by_email, route: "/by-email/:email"
      get :registered, route: "/registered"
      get :unregistered, route: "/unregistered"
    end
  end

  code_interface do
    define :create, action: :create
    define :read, action: :read
    define :update, action: :update
    define :destroy, action: :destroy
    define :by_id, args: [:id], action: :by_id
    define :by_email, args: [:email], action: :by_email
  end

  actions do
    defaults [:read, :update, :destroy]

    create :create do
      accept [:name, :email, :phone, :is_registered]
      primary? true
    end

    read :by_id do
      argument :id, :uuid, allow_nil?: false
      get? true
      filter expr(id == ^arg(:id))
    end

    read :by_email do
      argument :email, :string, allow_nil?: false
      filter expr(email == ^arg(:email))
    end

    read :registered do
      filter expr(is_registered == true)
    end

    read :unregistered do
      filter expr(is_registered == false)
    end
  end

  attributes do
    uuid_primary_key :id

    attribute :name, :string do
      allow_nil? false
      public? true
      description "The name of the client"
    end

    attribute :email, :ci_string do
      allow_nil? true
      public? true
      description "Email address (required for registered clients)"
    end

    attribute :phone, :string do
      allow_nil? true
      public? true
      description "Contact phone number"
    end

    attribute :is_registered, :boolean do
      allow_nil? false
      default false
      public? true
      description "Whether this is a registered client"
    end

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  validations do
    validate present([:email]), where: attribute_equals(:is_registered, true)
    validate match(~r/^[^\s]+@[^\s]+$/, :email), where: attribute_equals(:is_registered, true)
  end

  relationships do
    has_many :reservations, RivaAsh.Resources.Reservation do
      public? true
      description "Reservations made by this client"
    end
  end
end
