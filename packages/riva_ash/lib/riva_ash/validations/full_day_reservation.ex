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

  alias Ash.Error.Changes.InvalidChanges

  @impl true
  @spec validate(changeset, opts, context :: map()) :: :ok | {:error, map()}
  def validate(changeset, _opts, _context) do
    with {:ok, reserved_from} <- Ash.Changeset.get_attribute(changeset, :reserved_from),
         {:ok, reserved_until} <- Ash.Changeset.get_attribute(changeset, :reserved_until) do
      validate_full_day_reservation(reserved_from, reserved_until)
    else
      {:error, :missing_attribute} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @type changeset :: Ash.Changeset.t()
  @type opts :: Keyword.t()

  @spec validate_full_day_reservation(DateTime.t(), DateTime.t()) :: :ok | {:error, map()}
  defp validate_full_day_reservation(reserved_from, reserved_until) do
    if full_day_reservation?(reserved_from, reserved_until) do
      :ok
    else
      {:error,
       InvalidChanges.exception(
         field: :reserved_from,
         message:
           "Reservations must be for full calendar days only. Start time must be 00:00:00 and end time must be 23:59:59 or 00:00:00 of the next day."
       )}
    end
  end

  @spec full_day_reservation?(DateTime.t(), DateTime.t()) :: boolean()
  defp full_day_reservation?(reserved_from, reserved_until) do
    {from_date, from_time, until_date, until_time} =
      {DateTime.to_date(reserved_from), DateTime.to_time(reserved_from),
       DateTime.to_date(reserved_until), DateTime.to_time(reserved_until)}
    
    start_is_beginning_of_day = Timex.compare(from_time, ~T[00:00:00]) == 0
    end_is_end_of_day = validate_end_time(until_time, until_date, from_date)
    
    start_is_beginning_of_day and end_is_end_of_day
  end

  @spec validate_end_time(Time.t(), Date.t(), Date.t()) :: boolean()
  defp validate_end_time(until_time, until_date, from_date) do
    cond do
      Timex.compare(until_time, ~T[23:59:59]) == 0 -> true
      Timex.compare(until_time, ~T[00:00:00]) == 0 and Timex.diff(until_date, from_date, :days) >= 1 -> true
      true -> false
    end
  end

  @spec get_required_attribute(changeset, atom()) :: {:ok, any()} | {:error, :missing_attribute}
  defp get_required_attribute(changeset, attribute) do
    case Ash.Changeset.get_attribute(changeset, attribute) do
      nil -> {:error, :missing_attribute}
      value -> {:ok, value}
    end
  end
end
