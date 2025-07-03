defmodule RivaAsh.Resources.Reservation do
  @moduledoc """
  Represents a reservation made by a client for an item.
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

    # Create versions on destroy (for soft deletes)
    create_version_on_destroy? true
  end

  postgres do
    table "reservations"
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
    type "reservation"

    routes do
      base "/reservations"

      get :read
      index :read
      post :create
      patch :update
      delete :destroy

      # Additional routes for reservation-specific actions
      get :by_client, route: "/by-client/:client_id"
      get :by_item, route: "/by-item/:item_id"
      get :by_employee, route: "/by-employee/:employee_id"
      get :active, route: "/active"
      get :upcoming, route: "/upcoming"
      get :past, route: "/past"

      # Status update routes
      patch :confirm, route: "/:id/confirm"
      patch :cancel, route: "/:id/cancel"
      patch :complete, route: "/:id/complete"
    end
  end

  code_interface do
    define :create, action: :create
    define :read, action: :read
    define :update, action: :update
    define :destroy, action: :destroy
    define :by_id, args: [:id], action: :by_id
    define :by_client, args: [:client_id], action: :by_client
    define :by_item, args: [:item_id], action: :by_item
    define :by_employee, args: [:employee_id], action: :by_employee
    define :active, action: :active
  end

  actions do
    defaults [:read, :update, :destroy]

    create :create do
      accept [:client_id, :item_id, :employee_id, :reserved_from, :reserved_until, :notes]
      primary? true
    end

    read :by_id do
      argument :id, :uuid, allow_nil?: false
      get? true
      filter expr(id == ^arg(:id))
    end

    read :by_client do
      argument :client_id, :uuid, allow_nil?: false
      filter expr(client_id == ^arg(:client_id))
    end

    read :by_item do
      argument :item_id, :uuid, allow_nil?: false
      filter expr(item_id == ^arg(:item_id))
    end

    read :by_employee do
      argument :employee_id, :uuid, allow_nil?: false
      filter expr(employee_id == ^arg(:employee_id))
    end

    read :active do
      now = DateTime.utc_now()
      filter expr(
        reserved_from <= ^now and
        reserved_until >= ^now and
        status == "confirmed"
      )
    end

    read :upcoming do
      now = DateTime.utc_now()
      filter expr(
        reserved_from > ^now and
        status in ["pending", "confirmed"]
      )
    end

    read :past do
      now = DateTime.utc_now()
      filter expr(
        reserved_until < ^now or
        status in ["completed", "cancelled"]
      )
    end

    update :confirm do
      accept []
      change set_attribute(:status, :confirmed)
    end

    update :cancel do
      accept []
      change set_attribute(:status, :cancelled)
    end

    update :complete do
      accept []
      change set_attribute(:status, :completed)
    end

    # TODO: Re-enable reactor action once reactor syntax is fixed
    # action :create_with_validation, :struct do
    #   constraints instance_of: RivaAsh.Resources.Reservation
    #   argument :client_id, :uuid, allow_nil?: false
    #   argument :employee_id, :uuid, allow_nil?: false
    #   argument :item_id, :uuid, allow_nil?: false
    #   argument :start_datetime, :utc_datetime, allow_nil?: false
    #   argument :end_datetime, :utc_datetime, allow_nil?: false
    #   argument :notes, :string, allow_nil?: true
    #   run RivaAsh.Reactors.ReservationReactor
    # end
  end

  attributes do
    uuid_primary_key :id

    attribute :reserved_from, :utc_datetime do
      allow_nil? false
      public? true
      description "When the reservation starts"
    end

    attribute :reserved_until, :utc_datetime do
      allow_nil? false
      public? true
      description "When the reservation ends"
    end

    attribute :status, :atom do
      constraints one_of: [:pending, :confirmed, :cancelled, :completed]
      default :pending
      public? true
      description "Current status of the reservation"
    end

    attribute :notes, :string do
      allow_nil? true
      public? true
      description "Additional notes about the reservation"
    end

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    belongs_to :client, RivaAsh.Resources.Client do
      allow_nil? false
      attribute_writable? true
      public? true
      description "The client for whom the reservation was made"
    end

    belongs_to :item, RivaAsh.Resources.Item do
      allow_nil? false
      attribute_writable? true
      public? true
      description "The item being reserved"
    end

    belongs_to :employee, RivaAsh.Resources.Employee do
      allow_nil? false
      attribute_writable? true
      public? true
      description "The employee who created this reservation"
    end
  end

  validations do
    validate {RivaAsh.Validations.ReservationTimeSlot, []}
  end
end
