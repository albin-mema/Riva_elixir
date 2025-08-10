defmodule RivaAsh.AvailabilityPropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  alias RivaAsh.Availability

  @moduledoc false

  # Initial lightweight property-based scaffold to exercise Availability invariants.
  # This does not hit DB-backed items; it ensures function robustness against random windows.

  property "availability check handles random hourly windows without crashing" do
    check all(
            hour <- StreamData.integer(0..20),
            day_offset <- StreamData.integer(0..5)
          ) do
      date = Date.add(Date.utc_today(), day_offset)
      start_dt = DateTime.new!(date, Time.new!(hour, 0, 0))
      end_dt = DateTime.add(start_dt, 3600, :second)

      # Random unknown item id; function should return ok or error but not raise
      case Availability.check_availability(Ecto.UUID.generate(), start_dt, end_dt) do
        {:ok, _} -> assert true
        {:error, _} -> assert true
      end
    end
  end
end
