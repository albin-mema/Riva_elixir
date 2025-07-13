defmodule RivaAsh.Repo.Migrations.AddDenormalizedBusinessIds do
  @moduledoc """
  Adds denormalized business_id columns to tables that need them for efficient querying
  """
  use Ecto.Migration

  def up do
    # Note: reservations table already has business_id from previous migration

    # Add business_id to item_positions table
    alter table(:item_positions) do
      add(:business_id, :uuid, null: true)
    end

    # Populate business_id in item_positions from layouts -> plots -> businesses
    execute("""
      UPDATE item_positions
      SET business_id = plots.business_id
      FROM layouts, plots
      WHERE item_positions.layout_id = layouts.id
      AND layouts.plot_id = plots.id
    """)

    # Make business_id non-null and add foreign key constraint
    alter table(:item_positions) do
      modify(:business_id, :uuid, null: false)
    end

    execute("ALTER TABLE item_positions ADD CONSTRAINT item_positions_business_id_fkey FOREIGN KEY (business_id) REFERENCES businesses(id)")

    # Add business_id to layouts table
    alter table(:layouts) do
      add(:business_id, :uuid, null: true)
    end

    # Populate business_id in layouts from plots
    execute("""
      UPDATE layouts
      SET business_id = plots.business_id
      FROM plots
      WHERE layouts.plot_id = plots.id
    """)

    # Make business_id non-null and add foreign key constraint
    alter table(:layouts) do
      modify(:business_id, :uuid, null: false)
    end

    execute("ALTER TABLE layouts ADD CONSTRAINT layouts_business_id_fkey FOREIGN KEY (business_id) REFERENCES businesses(id)")

    # Add business_id to sections table
    alter table(:sections) do
      add(:business_id, :uuid, null: true)
    end

    # Populate business_id in sections from plots
    execute("""
      UPDATE sections
      SET business_id = plots.business_id
      FROM plots
      WHERE sections.plot_id = plots.id
    """)

    # Make business_id non-null and add foreign key constraint
    alter table(:sections) do
      modify(:business_id, :uuid, null: false)
    end

    execute("ALTER TABLE sections ADD CONSTRAINT sections_business_id_fkey FOREIGN KEY (business_id) REFERENCES businesses(id)")

    # Create indexes for the new business_id columns
    # Note: reservations business_id index already created in previous migration
    create_if_not_exists(index(:item_positions, [:business_id]))
    create_if_not_exists(index(:layouts, [:business_id]))
    create_if_not_exists(index(:sections, [:business_id]))
  end

  def down do
    # Drop indexes
    drop_if_exists(index(:sections, [:business_id]))
    drop_if_exists(index(:layouts, [:business_id]))
    drop_if_exists(index(:item_positions, [:business_id]))
    # Note: reservations business_id index handled by previous migration

    # Drop foreign key constraints and columns
    execute("ALTER TABLE sections DROP CONSTRAINT IF EXISTS sections_business_id_fkey")
    alter table(:sections) do
      remove(:business_id)
    end

    execute("ALTER TABLE layouts DROP CONSTRAINT IF EXISTS layouts_business_id_fkey")
    alter table(:layouts) do
      remove(:business_id)
    end

    execute("ALTER TABLE item_positions DROP CONSTRAINT IF EXISTS item_positions_business_id_fkey")
    alter table(:item_positions) do
      remove(:business_id)
    end

    # Note: reservations business_id column handled by previous migration
  end
end
