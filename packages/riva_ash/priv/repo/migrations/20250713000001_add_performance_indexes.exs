defmodule RivaAsh.Repo.Migrations.AddPerformanceIndexes do
  @moduledoc """
  Adds strategic database indexes to improve query performance across the application.
  These indexes target the most common query patterns and bottlenecks.
  """

  use Ecto.Migration

  def up do
    # Reservations table - most critical for performance
    create index(:reservations, [:item_id, :reserved_from, :reserved_until],
      name: :reservations_item_time_idx,
      comment: "Optimize availability checking queries")

    create index(:reservations, [:client_id, :status],
      name: :reservations_client_status_idx,
      comment: "Optimize client reservation lookups")

    create index(:reservations, [:business_id, :reserved_from],
      name: :reservations_business_date_idx,
      comment: "Optimize business dashboard queries")

    create index(:reservations, [:status, :reserved_from],
      where: "status IN ('confirmed', 'pending')",
      name: :reservations_active_status_date_idx,
      comment: "Optimize active reservation queries")

    create index(:reservations, [:employee_id],
      where: "employee_id IS NOT NULL",
      name: :reservations_employee_idx,
      comment: "Optimize employee reservation tracking")

    # Items table - frequently queried
    create index(:items, [:business_id, :is_active],
      name: :items_business_active_idx,
      comment: "Optimize business item listings")

    create index(:items, [:section_id],
      where: "section_id IS NOT NULL",
      name: :items_section_idx,
      comment: "Optimize section-based item queries")

    create index(:items, [:item_type_id],
      where: "item_type_id IS NOT NULL",
      name: :items_type_idx,
      comment: "Optimize item type filtering")

    create index(:items, [:is_active, :is_always_available],
      name: :items_availability_idx,
      comment: "Optimize availability queries")

    # Item schedules - for availability checking
    create index(:item_schedules, [:item_id, :day_of_week],
      name: :item_schedules_item_day_idx,
      comment: "Optimize schedule lookups by day")

    create index(:item_schedules, [:item_id, :start_time, :end_time],
      name: :item_schedules_item_time_idx,
      comment: "Optimize time-based schedule queries")

    # Availability exceptions - for availability checking
    create index(:availability_exceptions, [:item_id, :date],
      name: :availability_exceptions_item_date_idx,
      comment: "Optimize exception lookups by date")

    create index(:availability_exceptions, [:date, :exception_type],
      name: :availability_exceptions_date_type_idx,
      comment: "Optimize exception queries by type")

    # Clients table
    create index(:clients, [:email],
      where: "email IS NOT NULL",
      name: :clients_email_idx,
      comment: "Optimize client email lookups")

    create index(:clients, [:is_registered],
      name: :clients_registered_idx,
      comment: "Optimize registered client queries")

    # Employees table
    create index(:employees, [:business_id, :is_active],
      name: :employees_business_active_idx,
      comment: "Optimize business employee listings")

    create index(:employees, [:email],
      where: "email IS NOT NULL",
      name: :employees_email_idx,
      comment: "Optimize employee email lookups")

    # Employee permissions - for authorization
    create index(:employee_permissions, [:employee_id, :permission_id],
      name: :employee_permissions_employee_permission_idx,
      comment: "Optimize permission checking")

    create index(:employee_permissions, [:permission_id],
      name: :employee_permissions_permission_idx,
      comment: "Optimize permission-based queries")

    # Payments table
    create index(:payments, [:reservation_id],
      name: :payments_reservation_idx,
      comment: "Optimize payment lookups by reservation")

    create index(:payments, [:client_id, :payment_status],
      name: :payments_client_status_idx,
      comment: "Optimize client payment tracking")

    create index(:payments, [:business_id, :payment_date],
      name: :payments_business_date_idx,
      comment: "Optimize business payment reports")

    # Pricing table
    create index(:pricing, [:business_id, :item_type_id, :pricing_type],
      name: :pricing_business_type_idx,
      comment: "Optimize pricing lookups")

    create index(:pricing, [:effective_from, :effective_until],
      name: :pricing_date_range_idx,
      comment: "Optimize date-based pricing queries")

    # Sections table
    create index(:sections, [:plot_id, :is_active],
      name: :sections_plot_active_idx,
      comment: "Optimize plot section listings")

    # Plots table
    create index(:plots, [:business_id, :is_active],
      name: :plots_business_active_idx,
      comment: "Optimize business plot listings")

    # Item types table
    create index(:item_types, [:business_id, :is_active],
      name: :item_types_business_active_idx,
      comment: "Optimize business item type listings")

    # Layouts table
    create index(:layouts, [:plot_id, :is_active],
      name: :layouts_plot_active_idx,
      comment: "Optimize plot layout queries")

    # Item positions table
    create index(:item_positions, [:layout_id],
      name: :item_positions_layout_idx,
      comment: "Optimize layout position queries")

    create index(:item_positions, [:item_id],
      name: :item_positions_item_idx,
      comment: "Optimize item position lookups")

    # Recurring reservations
    create index(:recurring_reservations, [:client_id, :status],
      name: :recurring_reservations_client_status_idx,
      comment: "Optimize recurring reservation tracking")

    create index(:recurring_reservations, [:item_id, :status],
      name: :recurring_reservations_item_status_idx,
      comment: "Optimize item recurring reservation queries")

    # Recurring reservation instances
    create index(:recurring_reservation_instances, [:recurring_reservation_id, :scheduled_date],
      name: :recurring_instances_reservation_date_idx,
      comment: "Optimize recurring instance queries")

    create index(:recurring_reservation_instances, [:scheduled_date, :status],
      name: :recurring_instances_date_status_idx,
      comment: "Optimize date-based instance queries")

    # Item holds - for booking process
    create index(:item_holds, [:item_id, :expires_at],
      name: :item_holds_item_expiry_idx,
      comment: "Optimize hold expiry checking")

    create index(:item_holds, [:client_id, :is_active],
      name: :item_holds_client_active_idx,
      comment: "Optimize client hold tracking")

    # Additional composite indexes for complex queries
    create index(:reservations, [:item_id, :status, :reserved_from],
      name: :reservations_item_status_date_idx,
      comment: "Optimize item availability checking with status")

    create index(:reservations, [:client_id, :reserved_from],
      name: :reservations_client_date_idx,
      comment: "Optimize client reservation history queries")

    # Full-text search indexes (if using PostgreSQL)
    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS items_name_search_idx ON items USING gin(to_tsvector('english', name))"
    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS items_description_search_idx ON items USING gin(to_tsvector('english', description))"

    # Businesses table
    create index(:businesses, [:owner_id],
      where: "owner_id IS NOT NULL",
      name: :businesses_owner_idx,
      comment: "Optimize owner business lookups")
  end

  def down do
    # Drop full-text search indexes
    execute "DROP INDEX CONCURRENTLY IF EXISTS items_description_search_idx"
    execute "DROP INDEX CONCURRENTLY IF EXISTS items_name_search_idx"

    # Drop all indexes in reverse order
    drop_if_exists index(:businesses, [:owner_id], name: :businesses_owner_idx)
    drop_if_exists index(:reservations, [:client_id, :reserved_from], name: :reservations_client_date_idx)
    drop_if_exists index(:reservations, [:item_id, :status, :reserved_from], name: :reservations_item_status_date_idx)
    drop_if_exists index(:item_holds, [:client_id, :is_active], name: :item_holds_client_active_idx)
    drop_if_exists index(:item_holds, [:item_id, :expires_at], name: :item_holds_item_expiry_idx)
    drop_if_exists index(:recurring_reservation_instances, [:scheduled_date, :status], name: :recurring_instances_date_status_idx)
    drop_if_exists index(:recurring_reservation_instances, [:recurring_reservation_id, :scheduled_date], name: :recurring_instances_reservation_date_idx)
    drop_if_exists index(:recurring_reservations, [:item_id, :status], name: :recurring_reservations_item_status_idx)
    drop_if_exists index(:recurring_reservations, [:client_id, :status], name: :recurring_reservations_client_status_idx)
    drop_if_exists index(:item_positions, [:item_id], name: :item_positions_item_idx)
    drop_if_exists index(:item_positions, [:layout_id], name: :item_positions_layout_idx)
    drop_if_exists index(:layouts, [:plot_id, :is_active], name: :layouts_plot_active_idx)
    drop_if_exists index(:item_types, [:business_id, :is_active], name: :item_types_business_active_idx)
    drop_if_exists index(:plots, [:business_id, :is_active], name: :plots_business_active_idx)
    drop_if_exists index(:sections, [:plot_id, :is_active], name: :sections_plot_active_idx)
    drop_if_exists index(:pricing, [:effective_from, :effective_until], name: :pricing_date_range_idx)
    drop_if_exists index(:pricing, [:business_id, :item_type_id, :pricing_type], name: :pricing_business_type_idx)
    drop_if_exists index(:payments, [:business_id, :payment_date], name: :payments_business_date_idx)
    drop_if_exists index(:payments, [:client_id, :payment_status], name: :payments_client_status_idx)
    drop_if_exists index(:payments, [:reservation_id], name: :payments_reservation_idx)
    drop_if_exists index(:employee_permissions, [:permission_id], name: :employee_permissions_permission_idx)
    drop_if_exists index(:employee_permissions, [:employee_id, :permission_id], name: :employee_permissions_employee_permission_idx)
    drop_if_exists index(:employees, [:email], name: :employees_email_idx)
    drop_if_exists index(:employees, [:business_id, :is_active], name: :employees_business_active_idx)
    drop_if_exists index(:clients, [:is_registered], name: :clients_registered_idx)
    drop_if_exists index(:clients, [:email], name: :clients_email_idx)
    drop_if_exists index(:availability_exceptions, [:date, :exception_type], name: :availability_exceptions_date_type_idx)
    drop_if_exists index(:availability_exceptions, [:item_id, :date], name: :availability_exceptions_item_date_idx)
    drop_if_exists index(:item_schedules, [:item_id, :start_time, :end_time], name: :item_schedules_item_time_idx)
    drop_if_exists index(:item_schedules, [:item_id, :day_of_week], name: :item_schedules_item_day_idx)
    drop_if_exists index(:items, [:is_active, :is_always_available], name: :items_availability_idx)
    drop_if_exists index(:items, [:item_type_id], name: :items_type_idx)
    drop_if_exists index(:items, [:section_id], name: :items_section_idx)
    drop_if_exists index(:items, [:business_id, :is_active], name: :items_business_active_idx)
    drop_if_exists index(:reservations, [:employee_id], name: :reservations_employee_idx)
    drop_if_exists index(:reservations, [:status, :reserved_from], name: :reservations_active_status_date_idx)
    drop_if_exists index(:reservations, [:business_id, :reserved_from], name: :reservations_business_date_idx)
    drop_if_exists index(:reservations, [:client_id, :status], name: :reservations_client_status_idx)
    drop_if_exists index(:reservations, [:item_id, :reserved_from, :reserved_until], name: :reservations_item_time_idx)
  end
end
