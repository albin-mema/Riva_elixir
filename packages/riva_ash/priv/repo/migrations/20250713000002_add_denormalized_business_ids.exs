defmodule RivaAsh.Repo.Migrations.AddDenormalizedBusinessIds do
  @moduledoc """
  Add denormalized business_id columns to reservation and payment tables for performance optimization.
  This eliminates the need for deep joins like item.section.business_id in queries.
  """
  use Ecto.Migration

  def up do
    # Add business_id to reservations table
    alter table(:reservations) do
      add :business_id, :uuid, null: false
    end

    # Add business_id to payments table
    alter table(:payments) do
      add :business_id, :uuid, null: false
    end

    # Add foreign key constraints
    create constraint(:reservations, :reservations_business_id_fkey,
      foreign_key: [:business_id],
      references: [:businesses, :id],
      on_delete: :restrict,
      on_update: :update_all
    )

    create constraint(:payments, :payments_business_id_fkey,
      foreign_key: [:business_id],
      references: [:businesses, :id],
      on_delete: :restrict,
      on_update: :update_all
    )

    # Add indexes for the new business_id columns
    create index(:reservations, [:business_id], 
      name: :reservations_business_id_idx,
      comment: "Optimize business-scoped reservation queries")

    create index(:reservations, [:business_id, :status], 
      name: :reservations_business_status_idx,
      comment: "Optimize business reservation status queries")

    create index(:reservations, [:business_id, :reserved_from, :reserved_until], 
      name: :reservations_business_date_range_idx,
      comment: "Optimize business reservation date range queries")

    create index(:payments, [:business_id], 
      name: :payments_business_id_idx,
      comment: "Optimize business-scoped payment queries")

    create index(:payments, [:business_id, :status], 
      name: :payments_business_status_idx,
      comment: "Optimize business payment status queries")

    # Populate the business_id columns with data from existing records
    execute """
    UPDATE reservations 
    SET business_id = items.business_id 
    FROM items 
    WHERE reservations.item_id = items.id
    """

    execute """
    UPDATE payments 
    SET business_id = items.business_id 
    FROM reservations, items 
    WHERE payments.reservation_id = reservations.id 
    AND reservations.item_id = items.id
    """

    # Add not null constraints after populating data
    alter table(:reservations) do
      modify :business_id, :uuid, null: false
    end

    alter table(:payments) do
      modify :business_id, :uuid, null: false
    end
  end

  def down do
    # Remove indexes
    drop index(:reservations, [:business_id, :reserved_from, :reserved_until])
    drop index(:reservations, [:business_id, :status])
    drop index(:reservations, [:business_id])
    drop index(:payments, [:business_id, :status])
    drop index(:payments, [:business_id])

    # Remove foreign key constraints
    drop constraint(:reservations, :reservations_business_id_fkey)
    drop constraint(:payments, :payments_business_id_fkey)

    # Remove columns
    alter table(:reservations) do
      remove :business_id
    end

    alter table(:payments) do
      remove :business_id
    end
  end
end
