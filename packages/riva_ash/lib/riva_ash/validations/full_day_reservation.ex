defmodule RivaAsh.Validations.FullDayReservation do
  @moduledoc """
  Validates that a reservation is for full calendar days only.

  According to business rules:
  - Reservations must start at the beginning of a day (00:00:00)
  - Reservations must end at the end of a day (23:59:59) or beginning of next day (00:00:00)
  - No hourly or partial day bookings are allowed
  """
  use Ash.Resource.Validation
  require Ash.Expr
  import Ash.Expr
  alias Ash.Error.Changes.InvalidChanges

  @impl true
  def validate(changeset, _opts, _context) do
    reserved_from = Ash.Changeset.get_attribute(changeset, :reserved_from)
    reserved_until = Ash.Changeset.get_attribute(changeset, :reserved_until)

    cond do
      is_nil(reserved_from) or is_nil(reserved_until) ->
        :ok

      not full_day_reservation?(reserved_from, reserved_until) ->
        {:error,
         InvalidChanges.exception(
           field: :reserved_from,
           message: "Reservations must be for full calendar days only. Start time must be 00:00:00 and end time must be 23:59:59 or 00:00:00 of the next day."
         )}

      true ->
        :ok
    end
  end

  defp full_day_reservation?(reserved_from, reserved_until) do
    # Convert to date and time components
    from_date = DateTime.to_date(reserved_from)
    from_time = DateTime.to_time(reserved_from)

    until_date = DateTime.to_date(reserved_until)
    until_time = DateTime.to_time(reserved_until)

    # Check if start time is at beginning of day (00:00:00)
    start_is_beginning_of_day = Timex.compare(from_time, ~T[00:00:00]) == 0

    # Check if end time is at end of day (23:59:59) or beginning of next day (00:00:00)
    end_is_end_of_day =
      Timex.compare(until_time, ~T[23:59:59]) == 0 or
      (Timex.compare(until_time, ~T[00:00:00]) == 0 and Timex.diff(until_date, from_date, :days) >= 1)

    start_is_beginning_of_day and end_is_end_of_day
  end
end
