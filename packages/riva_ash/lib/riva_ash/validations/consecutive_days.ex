defmodule RivaAsh.Validations.ConsecutiveDays do
  @moduledoc """
  Validates that multi-day reservations span consecutive calendar days.
  
  According to business rules:
  - Multi-day reservations must be for consecutive days
  - No gaps are allowed between reservation days
  - Single day reservations are always valid
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

      not consecutive_days?(reserved_from, reserved_until) ->
        {:error,
         InvalidChanges.exception(
           field: :reserved_from,
           message: "Multi-day reservations must be for consecutive calendar days with no gaps."
         )}

      true ->
        :ok
    end
  end

  defp consecutive_days?(reserved_from, reserved_until) do
    from_date = DateTime.to_date(reserved_from)
    until_date = DateTime.to_date(reserved_until)
    
    # Calculate the number of days between start and end
    days_diff = Date.diff(until_date, from_date)
    
    # For single day reservations (same day), always valid
    if days_diff == 0 do
      true
    else
      # For multi-day reservations, check if they are consecutive
      # The difference should equal the number of days in the reservation
      # For example: Jan 1 to Jan 3 should have diff of 2 (3 days total)
      days_diff >= 1 and consecutive_date_range?(from_date, until_date)
    end
  end

  defp consecutive_date_range?(start_date, end_date) do
    # Generate all dates in the range and check if they are consecutive
    expected_dates = Date.range(start_date, end_date) |> Enum.to_list()
    actual_days = Date.diff(end_date, start_date) + 1
    
    # The number of expected dates should match the actual day difference + 1
    length(expected_dates) == actual_days
  end
end
