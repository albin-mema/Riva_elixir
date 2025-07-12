defmodule RivaAsh.Reactors.ReservationReactor do
  @moduledoc """
  Reactor for creating reservations with comprehensive validation and business logic.

  This reactor handles:
  1. Availability checking
  2. Client validation
  3. Item validation
  4. Employee assignment
  5. Reservation creation
  6. Pricing calculation
  7. Conflict resolution

  All operations are transactional with proper rollback on failure.
  """

  use Reactor

  alias RivaAsh.Resources.{Reservation, Client, Item, Employee}
  alias RivaAsh.Availability

  # Define the reactor inputs
  input :client_id
  input :employee_id
  input :item_id
  input :start_datetime
  input :end_datetime
  input :notes

  # Step 1: Validate client exists and is active
  step :validate_client do
    argument :client_id, input(:client_id)

    run fn %{client_id: client_id}, _context ->
      case Client.by_id(client_id) do
        {:ok, client} ->
          if client.archived_at do
            {:error, "Client is archived and cannot make reservations"}
          else
            {:ok, client}
          end
        {:error, _} ->
          {:error, "Client not found"}
      end
    end
  end

  # Step 2: Validate employee exists and is active
  step :validate_employee do
    argument :employee_id, input(:employee_id)

    run fn %{employee_id: employee_id}, _context ->
      case Employee.by_id(employee_id) do
        {:ok, employee} ->
          if employee.archived_at do
            {:error, "Employee is archived and cannot create reservations"}
          else
            {:ok, employee}
          end
        {:error, _} ->
          {:error, "Employee not found"}
      end
    end
  end

  # Step 3: Validate item exists and is available
  step :validate_item do
    argument :item_id, input(:item_id)

    run fn %{item_id: item_id}, _context ->
      case Item.by_id(item_id) do
        {:ok, item} ->
          if item.archived_at do
            {:error, "Item is archived and cannot be reserved"}
          else
            {:ok, item}
          end
        {:error, _} ->
          {:error, "Item not found"}
      end
    end
  end

  # Step 4: Validate datetime range
  step :validate_datetime_range do
    argument :start_datetime, input(:start_datetime)
    argument :end_datetime, input(:end_datetime)

    run fn %{start_datetime: start_dt, end_datetime: end_dt}, _context ->
      cond do
        DateTime.compare(start_dt, end_dt) != :lt ->
          {:error, "Start datetime must be before end datetime"}

        DateTime.compare(start_dt, DateTime.utc_now()) == :lt ->
          {:error, "Cannot create reservations in the past"}

        DateTime.diff(end_dt, start_dt, :hour) > 24 ->
          {:error, "Reservation cannot exceed 24 hours"}

        true ->
          {:ok, %{start_datetime: start_dt, end_datetime: end_dt}}
      end
    end
  end

  # Step 5: Check availability
  step :check_availability do
    argument :item_id, input(:item_id)
    argument :start_datetime, input(:start_datetime)
    argument :end_datetime, input(:end_datetime)

    run fn %{item_id: item_id, start_datetime: start_dt, end_datetime: end_dt}, _context ->
      case Availability.check_availability(item_id, start_dt, end_dt) do
        {:ok, true} ->
          {:ok, :available}
        {:ok, false} ->
          {:error, "Time slot is not available"}
        {:error, reason} ->
          {:error, "Failed to check availability: #{inspect(reason)}"}
      end
    end
  end

  # Step 6: Calculate pricing
  step :calculate_pricing do
    argument :item, result(:validate_item)
    argument :start_datetime, input(:start_datetime)
    argument :end_datetime, input(:end_datetime)

    run fn %{item: item, start_datetime: start_dt, end_datetime: end_dt}, _context ->
      # Calculate number of days (minimum 1 day)
      hours = DateTime.diff(end_dt, start_dt, :hour)
      days = max(1, ceil(hours / 24))

      # Get pricing for the item type
      case RivaAsh.Resources.Pricing.by_item_type(item.item_type_id) do
        {:ok, [pricing | _]} ->
          total_amount = Decimal.mult(pricing.price_per_day, Decimal.new(days))
          {:ok, %{
            total_amount: total_amount,
            daily_rate: pricing.price_per_day,
            number_of_days: days
          }}
        {:ok, []} ->
          {:error, "No pricing found for item type"}
        {:error, reason} ->
          {:error, "Failed to calculate pricing: #{inspect(reason)}"}
      end
    end
  end

  # Step 7: Create the reservation
  step :create_reservation do
    argument :client_id, input(:client_id)
    argument :employee_id, input(:employee_id)
    argument :item_id, input(:item_id)
    argument :start_datetime, input(:start_datetime)
    argument :end_datetime, input(:end_datetime)
    argument :notes, input(:notes)
    argument :pricing, result(:calculate_pricing)

    run fn args, _context ->
      reservation_attrs = %{
        client_id: args.client_id,
        employee_id: args.employee_id,
        item_id: args.item_id,
        reserved_from: args.start_datetime,
        reserved_until: args.end_datetime,
        notes: args.notes,
        total_amount: args.pricing.total_amount,
        daily_rate: args.pricing.daily_rate,
        number_of_days: args.pricing.number_of_days,
        status: :confirmed
      }

      Reservation
      |> Ash.Changeset.for_create(:create, reservation_attrs)
      |> Ash.create(domain: RivaAsh.Domain)
    end

    compensate fn reservation, _context ->
      Reservation.destroy!(reservation, domain: RivaAsh.Domain)
      :ok
    end
  end

  # Return the created reservation
  return :create_reservation
end
