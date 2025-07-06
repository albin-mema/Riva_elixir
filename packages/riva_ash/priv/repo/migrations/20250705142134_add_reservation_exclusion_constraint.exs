defmodule RivaAsh.Repo.Migrations.AddReservationExclusionConstraint do
  use Ecto.Migration

  def up do
    # For now, we'll add a simpler constraint and handle overlap checking in application logic
    # This avoids the PostgreSQL IMMUTABLE function issues with exclusion constraints

    # Add a unique index to help with performance of overlap queries
    create index(:reservations, [:item_id, :reserved_from, :reserved_until],
           where: "status IN ('pending', 'confirmed')",
           name: :reservations_item_time_index)

    # Add a check constraint to ensure reserved_from < reserved_until
    execute """
    ALTER TABLE reservations
    ADD CONSTRAINT reservations_valid_time_range
    CHECK (reserved_from < reserved_until)
    """
  end

  def down do
    # Drop the check constraint
    execute """
    ALTER TABLE reservations
    DROP CONSTRAINT IF EXISTS reservations_valid_time_range
    """

    # Drop the index
    drop index(:reservations, [:item_id, :reserved_from, :reserved_until],
         name: :reservations_item_time_index)
  end
end
