defmodule RivaAsh.Repo.Migrations.AddPerformanceIndexes do
  @moduledoc """
  Adds performance indexes for common query patterns
  """
  use Ecto.Migration

  def up do
    # Business-related indexes
    create_if_not_exists(index(:reservations, [:business_id]))
    create_if_not_exists(index(:reservations, [:business_id, :reserved_from]))
    create_if_not_exists(index(:reservations, [:business_id, :status]))
    create_if_not_exists(index(:reservations, [:business_id, :reserved_from, :reserved_until]))

    # Item-related indexes
    create_if_not_exists(index(:items, [:business_id, :is_active]))
    create_if_not_exists(index(:items, [:business_id, :item_type_id]))

    # Client-related indexes
    create_if_not_exists(index(:clients, [:business_id, :email]))

    # Employee-related indexes
    create_if_not_exists(index(:employees, [:business_id, :is_active]))

    # Payment-related indexes
    create_if_not_exists(index(:payments, [:business_id, :payment_date]))
    create_if_not_exists(index(:payments, [:business_id, :payment_method]))

    # Pricing-related indexes
    create_if_not_exists(index(:pricing, [:business_id, :item_type_id]))
    create_if_not_exists(index(:pricing, [:business_id, :effective_from]))

    # Plot and section indexes
    create_if_not_exists(index(:plots, [:business_id, :name]))
    create_if_not_exists(index(:sections, [:plot_id, :name]))

    # Layout and item position indexes
    create_if_not_exists(index(:layouts, [:plot_id, :name]))
    create_if_not_exists(index(:item_positions, [:layout_id, :grid_row, :grid_column]))

    # Item hold indexes
    create_if_not_exists(index(:item_holds, [:item_id, :is_active]))
    create_if_not_exists(index(:item_holds, [:client_id, :is_active]))
    create_if_not_exists(index(:item_holds, [:expires_at]))

    # Time-based indexes for reservations
    create_if_not_exists(index(:reservations, [:reserved_from, :reserved_until]))
    create_if_not_exists(index(:reservations, [:inserted_at]))
    create_if_not_exists(index(:reservations, [:updated_at]))
  end

  def down do
    # Drop indexes in reverse order
    drop_if_exists(index(:reservations, [:updated_at]))
    drop_if_exists(index(:reservations, [:inserted_at]))
    drop_if_exists(index(:reservations, [:reserved_from, :reserved_until]))

    drop_if_exists(index(:item_holds, [:expires_at]))
    drop_if_exists(index(:item_holds, [:client_id, :is_active]))
    drop_if_exists(index(:item_holds, [:item_id, :is_active]))

    drop_if_exists(index(:item_positions, [:layout_id, :grid_row, :grid_column]))
    drop_if_exists(index(:layouts, [:plot_id, :name]))

    drop_if_exists(index(:sections, [:plot_id, :name]))
    drop_if_exists(index(:plots, [:business_id, :name]))

    drop_if_exists(index(:pricing, [:business_id, :effective_from]))
    drop_if_exists(index(:pricing, [:business_id, :item_type_id]))

    drop_if_exists(index(:payments, [:business_id, :payment_method]))
    drop_if_exists(index(:payments, [:business_id, :payment_date]))

    drop_if_exists(index(:employees, [:business_id, :is_active]))

    drop_if_exists(index(:clients, [:business_id, :email]))

    drop_if_exists(index(:items, [:business_id, :item_type_id]))
    drop_if_exists(index(:items, [:business_id, :is_active]))

    drop_if_exists(index(:reservations, [:business_id, :reserved_from, :reserved_until]))
    drop_if_exists(index(:reservations, [:business_id, :status]))
    drop_if_exists(index(:reservations, [:business_id, :reserved_from]))
    drop_if_exists(index(:reservations, [:business_id]))
  end
end
