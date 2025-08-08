alias RivaAsh.Resources, as: Resources
alias RivaAsh.RecurringReservations, as: RecurringReservations

defmodule RivaAsh.Resources.RecurringReservation do
  @moduledoc """
  Represents a recurring reservation pattern for consecutive days.
  This is a template that generates individual reservation instances.

  Examples:
  - 5 consecutive days starting from a specific date
  - 30 consecutive days for a long-term rental
  - 10 consecutive weekdays (excluding weekends)
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    extensions: [
      AshJsonApi.Resource,
      AshPaperTrail.Resource,
      AshArchival.Resource
    ]

  import RivaAsh.ResourceHelpers

  postgres do
    table("recurring_reservations")
    repo(RivaAsh.Repo)
  end

  standard_archive()
  standard_paper_trail()

  json_api do
    type("recurring_reservation")

    routes do
      base("/recurring-reservations")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Additional routes for recurring reservation actions
      get(:by_client, route: "/by-client/:client_id")
      get(:by_item, route: "/by-item/:item_id")
      get(:by_employee, route: "/by-employee/:employee_id")
      get(:active, route: "/active")
      get(:upcoming, route: "/upcoming")

      # Pattern management routes
      # post :generate_instances, route: "/:id/generate-instances"
      patch(:pause, route: "/:id/pause")
      patch(:resume, route: "/:id/resume")
      patch(:cancel, route: "/:id/cancel")
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:by_id, args: [:id], action: :by_id)
    define(:by_client, args: [:client_id], action: :by_client)
    define(:by_item, args: [:item_id], action: :by_item)
    define(:by_employee, args: [:employee_id], action: :by_employee)
    # define :generate_instances, args: [:id], action: :generate_instances
  end

  actions do
    defaults([:read, :destroy])

    # Private helper functions for Single Level of Abstraction
    # Placeholder: domain-specific validations can be reintroduced via dedicated modules.
    defp apply_recurring_reservation_validations(changeset) do
      changeset
    end

    update :update do
      accept([
        :client_id,
        :item_id,
        :employee_id,
        :start_date,
        :start_time,
        :end_time,
        :consecutive_days,
        :pattern_type,
        :notes,
        :title,
        :status,
        :instances_generated,
        :generated_at
      ])

      primary?(true)

      change(fn changeset, _ctx -> apply_recurring_reservation_validations(changeset) end)
    end

    create :create do
      accept([
        :client_id,
        :item_id,
        :employee_id,
        :start_date,
        :start_time,
        :end_time,
        :consecutive_days,
        :pattern_type,
        :notes,
        :title
      ])

      primary?(true)
      change(fn changeset, _ctx -> apply_recurring_reservation_validations(changeset) end)
    end

    read :by_id do
      argument(:id, :uuid, allow_nil?: false)
      get?(true)
      filter(expr(id == ^arg(:id)))
    end

    read :by_client do
      argument(:client_id, :uuid, allow_nil?: false)
      filter(expr(client_id == ^arg(:client_id)))
    end

    read :by_item do
      argument(:item_id, :uuid, allow_nil?: false)
      filter(expr(item_id == ^arg(:item_id)))
    end

    read :by_employee do
      argument(:employee_id, :uuid, allow_nil?: false)
      filter(expr(employee_id == ^arg(:employee_id)))
    end

    read :active do
      filter(expr(status == "active"))
    end

    read :upcoming do
      filter(
        expr(
          start_date >= ^Date.utc_today() and
            status in ["active", "pending"]
        )
      )
    end

    update :pause do
      accept([])
      change(set_attribute(:status, :paused))
    end

    update :resume do
      accept([])
      change(set_attribute(:status, :active))
    end

    update :cancel do
      accept([])
      change(set_attribute(:status, :cancelled))
    end

    # Generate individual reservation instances from the recurring pattern
    # Use RivaAsh.RecurringReservations.generate_instances/1 instead
  end

  attributes do
    uuid_primary_key(:id)

    attribute :title, :string do
      allow_nil?(true)
      public?(true)
      description("Optional title for this recurring reservation pattern")
    end

    attribute :start_date, :date do
      allow_nil?(false)
      public?(true)
      description("The date when the recurring pattern starts")
    end

    attribute :start_time, :time do
      allow_nil?(false)
      public?(true)
      description("Daily start time for each reservation")
    end

    attribute :end_time, :time do
      allow_nil?(false)
      public?(true)
      description("Daily end time for each reservation")
    end

    attribute :consecutive_days, :integer do
      allow_nil?(false)
      public?(true)
      constraints(min: 1, max: 365)
      description("Number of consecutive days for this pattern")
    end

    attribute :pattern_type, :atom do
      constraints(one_of: [:consecutive_days])
      default(:consecutive_days)
      public?(true)
      description("Type of consecutive pattern - currently only consecutive days is supported")
    end

    attribute :status, :atom do
      constraints(one_of: [:pending, :active, :paused, :cancelled, :completed])
      default(:pending)
      public?(true)
      description("Current status of the recurring reservation pattern")
    end

    attribute :notes, :string do
      allow_nil?(true)
      public?(true)
      description("Additional notes about this recurring reservation")
    end

    attribute :instances_generated, :boolean do
      allow_nil?(false)
      default(false)
      public?(true)
      description("Whether individual reservation instances have been generated")
    end

    attribute :generated_at, :utc_datetime do
      allow_nil?(true)
      public?(true)
      description("When the instances were generated")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to :client, RivaAsh.Resources.Client do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The client for whom the recurring reservation is made")
    end

    belongs_to :item, RivaAsh.Resources.Item do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The item being reserved")
    end

    belongs_to :employee, RivaAsh.Resources.Employee do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The employee who created this recurring reservation")
    end

    has_many :instances, RivaAsh.Resources.RecurringReservationInstance do
      destination_attribute(:recurring_reservation_id)
      public?(true)
      description("Individual reservation instances generated from this pattern")
    end
  end

  validations do
    validate(compare(:end_time, greater_than: :start_time),
      message: "End time must be after start time"
    )

    validate(compare(:consecutive_days, greater_than: 0),
      message: "Must have at least 1 consecutive day"
    )
  end

  calculations do
    calculate :total_instances, :integer, expr(count(instances)) do
      public?(true)
      description("Total number of instances generated from this pattern")
    end

    calculate :confirmed_instances,
              :integer,
              expr(count(instances, query: [filter: expr(status == "confirmed")])) do
      public?(true)
      description("Number of confirmed instances")
    end

    calculate :end_date, :date, expr(date_add(start_date, consecutive_days - 1, "day")) do
      public?(true)
      description("The calculated end date of the recurring pattern")
    end
  end

  # Removed: build_upcoming_filter/1 helper was causing compile errors
  # because read blocks expect Ash DSL, not a changeset argument.

  # helper functions removed; calculations are inlined with expr/1
end
