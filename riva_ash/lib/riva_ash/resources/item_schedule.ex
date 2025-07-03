defmodule RivaAsh.Resources.ItemSchedule do
  @moduledoc """
  Represents recurring availability schedules for items.
  Defines when items are available on a weekly recurring basis.
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshJsonApi.Resource]

  postgres do
    table "item_schedules"
    repo RivaAsh.Repo
  end

  json_api do
    type "item_schedule"

    routes do
      base "/item-schedules"

      get :read
      index :read
      post :create
      patch :update
      delete :destroy
      
      # Additional routes for schedule-specific actions
      get :by_item, route: "/by-item/:item_id"
      get :by_day, route: "/by-day/:day_of_week"
    end
  end

  code_interface do
    define :create, action: :create
    define :read, action: :read
    define :update, action: :update
    define :destroy, action: :destroy
    define :by_id, args: [:id], action: :by_id
    define :by_item, args: [:item_id], action: :by_item
    define :by_day, args: [:day_of_week], action: :by_day
  end

  actions do
    defaults [:read, :update, :destroy]

    create :create do
      accept [:item_id, :day_of_week, :start_time, :end_time, :is_available]
      primary? true
    end

    read :by_id do
      argument :id, :uuid, allow_nil?: false
      get? true
      filter expr(id == ^arg(:id))
    end

    read :by_item do
      argument :item_id, :uuid, allow_nil?: false
      filter expr(item_id == ^arg(:item_id))
    end

    read :by_day do
      argument :day_of_week, :integer, allow_nil?: false
      filter expr(day_of_week == ^arg(:day_of_week))
    end

    read :available_schedules do
      filter expr(is_available == true)
    end

    read :unavailable_schedules do
      filter expr(is_available == false)
    end
  end

  attributes do
    uuid_primary_key :id

    attribute :day_of_week, :integer do
      allow_nil? false
      public? true
      constraints min: 0, max: 6
      description "Day of week (0=Sunday, 1=Monday, ..., 6=Saturday)"
    end

    attribute :start_time, :time do
      allow_nil? false
      public? true
      description "Start time for this availability window"
    end

    attribute :end_time, :time do
      allow_nil? false
      public? true
      description "End time for this availability window"
    end

    attribute :is_available, :boolean do
      allow_nil? false
      default true
      public? true
      description "Whether the item is available (true) or blocked (false) during this time"
    end

    attribute :notes, :string do
      allow_nil? true
      public? true
      description "Optional notes about this schedule entry"
    end

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    belongs_to :item, RivaAsh.Resources.Item do
      allow_nil? false
      attribute_writable? true
      public? true
      description "The item this schedule applies to"
    end
  end

  validations do
    validate compare(:end_time, greater_than: :start_time),
      message: "End time must be after start time"
  end

  identities do
    identity :unique_item_day_time, [:item_id, :day_of_week, :start_time, :end_time]
  end
end
