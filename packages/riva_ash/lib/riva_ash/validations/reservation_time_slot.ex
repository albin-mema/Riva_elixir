defmodule RivaAsh.Validations.ReservationTimeSlot do
  @moduledoc """
  Validates that a reservation's time slot doesn't overlap with existing reservations for the same item.
  Uses the standardized overlap detection logic from RivaAsh.Validations.
  """
  use Ash.Resource.Validation
  require Ash.Expr

  alias Ash.Error.Changes.InvalidChanges

  @impl true
  @spec validate(changeset, opts, context :: map()) :: :ok | {:error, map()}
  def validate(changeset, opts, _context) do
    with {:ok, item_id} <- Ash.Changeset.get_attribute(changeset, :item_id),
         {:ok, reserved_from} <- Ash.Changeset.get_attribute(changeset, :reserved_from),
         {:ok, reserved_until} <- Ash.Changeset.get_attribute(changeset, :reserved_until),
         {:ok, reservation_id} <- Ash.Changeset.get_attribute(changeset, :id) do
      validate_time_slot_availability(item_id, reserved_from, reserved_until, reservation_id, opts)
    else
      {:error, :missing_attribute} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @type changeset :: Ash.Changeset.t()
  @type opts :: Keyword.t()

  @spec validate_time_slot_availability(
          item_id :: String.t() | integer(),
          DateTime.t(),
          DateTime.t(),
          String.t() | integer() | nil,
          opts :: Keyword.t()
        ) :: :ok | {:error, map()}
  defp validate_time_slot_availability(item_id, reserved_from, reserved_until, reservation_id, opts) do
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

  @spec get_required_attribute(changeset, atom()) :: {:ok, any()} | {:error, :missing_attribute}
  defp get_required_attribute(changeset, attribute) do
    case Ash.Changeset.get_attribute(changeset, attribute) do
      nil -> {:error, :missing_attribute}
      value -> {:ok, value}
    end
  end

  @spec get_optional_attribute(changeset, atom()) :: {:ok, any()} | {:error, :missing_attribute}
  defp get_optional_attribute(changeset, attribute) do
    case Ash.Changeset.get_attribute(changeset, attribute) do
      nil -> {:ok, nil}
      value -> {:ok, value}
    end
  end
end
