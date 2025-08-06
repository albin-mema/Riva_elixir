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
  @spec validate(changeset, opts, context :: map()) :: :ok | {:error, map()}
  def validate(changeset, _opts, _context) do
    with {:ok, reserved_from} <- Ash.Changeset.get_attribute(changeset, :reserved_from),
         {:ok, reserved_until} <- Ash.Changeset.get_attribute(changeset, :reserved_until) do
      validate_consecutive_days(reserved_from, reserved_until)
    else
      {:error, :missing_attribute} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @type changeset :: Ash.Changeset.t()
  @type opts :: Keyword.t()

  @spec validate_consecutive_days(DateTime.t(), DateTime.t()) :: :ok | {:error, map()}
  defp validate_consecutive_days(reserved_from, reserved_until) do
    if consecutive_days?(reserved_from, reserved_until) do
      :ok
    else
      {:error,
       InvalidChanges.exception(
         field: :reserved_from,
         message: "Multi-day reservations must be for consecutive calendar days with no gaps."
       )}
    end
  end

  @spec consecutive_days?(DateTime.t(), DateTime.t()) :: boolean()
  defp consecutive_days?(reserved_from, reserved_until) do
    {from_date, until_date} = {Timex.to_date(reserved_from), Timex.to_date(reserved_until)}

    case Timex.diff(until_date, from_date, :days) do
      # Single day reservations are always valid
      0 -> true
      days_diff when days_diff >= 1 -> consecutive_date_range?(from_date, until_date)
      _ -> false
    end
  end

  @spec consecutive_date_range?(Date.t(), Date.t()) :: boolean()
  defp consecutive_date_range?(start_date, end_date) do
    expected_dates = Timex.Interval.new(from: start_date, until: end_date) |> Enum.to_list()
    actual_days = Timex.diff(end_date, start_date, :days) + 1

    length(expected_dates) == actual_days
  end

  @spec get_required_attribute(changeset, atom()) :: {:ok, any()} | {:error, :missing_attribute}
  defp get_required_attribute(changeset, attribute) do
    case Ash.Changeset.get_attribute(changeset, attribute) do
      nil -> {:error, :missing_attribute}
      value -> {:ok, value}
    end
  end
end
