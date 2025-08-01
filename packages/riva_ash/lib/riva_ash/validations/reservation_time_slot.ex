defmodule RivaAsh.Validations.ReservationTimeSlot do
  @moduledoc """
  Validates that a reservation's time slot doesn't overlap with existing reservations for the same item.
  Uses the standardized overlap detection logic from RivaAsh.Validations.
  """
  use Ash.Resource.Validation
  require Ash.Expr

  alias Ash.Error.Changes.InvalidChanges

  @impl true
  def validate(changeset, opts, _context) do
    item_id = Ash.Changeset.get_attribute(changeset, :item_id)
    reserved_from = Ash.Changeset.get_attribute(changeset, :reserved_from)
    reserved_until = Ash.Changeset.get_attribute(changeset, :reserved_until)
    reservation_id = Ash.Changeset.get_attribute(changeset, :id)

    # Use standardized overlap checking logic
    case RivaAsh.Validations.check_reservation_overlap(
           item_id,
           reserved_from,
           reserved_until,
           reservation_id,
           opts
         ) do
      {:ok, :no_overlap} ->
        :ok

      {:ok, :overlap_found} ->
        {:error,
         InvalidChanges.exception(
           field: :reserved_from,
           message: "Time slot overlaps with an existing reservation"
         )}

      {:error, reason} ->
        {:error,
         InvalidChanges.exception(
           field: :reserved_from,
           message: "Failed to validate time slot: #{reason}"
         )}
    end
  end
end
