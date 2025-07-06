defmodule RivaAsh.Repo.Migrations.AddReservationExclusionConstraint do
  use Ecto.Migration
  
  def up do
    # Enable btree_gist extension if not already enabled
    execute("CREATE EXTENSION IF NOT EXISTS btree_gist")
    
    # Add exclusion constraint to prevent overlapping reservations for the same item
    execute """
    ALTER TABLE reservations 
    ADD CONSTRAINT reservations_item_id_reserved_range_excl 
    EXCLUDE USING GIST (
      item_id WITH =, 
      tstzrange(reserved_from, reserved_until, '[]') WITH &&
    ) 
    WHERE (status = 'pending' OR status = 'confirmed')
    """
  end

  def down do
    execute """
    ALTER TABLE reservations 
    DROP CONSTRAINT IF EXISTS reservations_item_id_reserved_range_excl
    """
  end
end
