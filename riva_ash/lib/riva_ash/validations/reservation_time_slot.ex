defmodule RivaAsh.Validations.ReservationTimeSlot do
  @moduledoc """
  Validates that a reservation's time slot doesn't overlap with existing reservations for the same item.
  """
  use Ash.Resource.Validation
  require Ash.Expr
  import Ash.Expr
  alias Ash.Error.Changes.InvalidChanges

  @impl true
  def validate(changeset, _opts) do
    item_id = Ash.Changeset.get_attribute(changeset, :item_id)
    reserved_from = Ash.Changeset.get_attribute(changeset, :reserved_from)
    reserved_until = Ash.Changeset.get_attribute(changeset, :reserved_until)
    reservation_id = Ash.Changeset.get_attribute(changeset, :id)

    if overlapping_reservation?(item_id, reserved_from, reserved_until, reservation_id) do
      {:error, 
        InvalidChanges.exception(
          field: :reserved_from,
          message: "Time slot overlaps with an existing reservation"
        )
      }
    else
      :ok
    end
  end

  defp overlapping_reservation?(item_id, reserved_from, reserved_until, reservation_id) do
    query = RivaAsh.Resources.Reservation
      |> Ash.Query.filter(
        item_id == ^item_id and
        reserved_from < ^reserved_until and
        reserved_until > ^reserved_from
      )

    query = if reservation_id do
      Ash.Query.filter(query, id != ^reservation_id)
    else
      query
    end

    case RivaAsh.Domain.read_one(query) do
      {:ok, nil} -> false
      {:ok, _reservation} -> true
      _ -> false
    end
  end
end
