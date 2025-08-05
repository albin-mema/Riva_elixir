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

  require Logger
  alias RivaAsh.Resources.{Reservation, Client, Item, Employee}
  alias RivaAsh.Availability
  alias RivaAsh.Config.AppConfig

  @type client_id :: integer()
  @type employee_id :: integer()
  @type item_id :: integer()
  @type datetime :: DateTime.t()
  @type notes :: String.t() | nil

  @type pricing_result :: %{
          required(:total_amount) => Decimal.t(),
          required(:daily_rate) => Decimal.t(),
          required(:number_of_days) => integer()
        }

  @type reservation_attrs :: %{
          required(:client_id) => client_id(),
          required(:employee_id) => employee_id(),
          required(:item_id) => item_id(),
          required(:reserved_from) => datetime(),
          required(:reserved_until) => datetime(),
          optional(:notes) => notes(),
          required(:total_amount) => Decimal.t(),
          required(:daily_rate) => Decimal.t(),
          required(:number_of_days) => integer(),
          required(:status) => atom()
        }

  @type result :: Reservation.t()

  @spec validate_client(client_id()) :: {:ok, Client.t()} | {:error, String.t()}
  @spec validate_employee(employee_id()) :: {:ok, Employee.t()} | {:error, String.t()}
  @spec validate_item(item_id()) :: {:ok, Item.t()} | {:error, String.t()}
  @spec validate_datetime_range(datetime(), datetime()) :: {:ok, map()} | {:error, String.t()}
  @spec check_availability(item_id(), datetime(), datetime()) :: {:ok, atom()} | {:error, String.t()}
  @spec calculate_pricing(Item.t(), datetime(), datetime()) :: {:ok, pricing_result()} | {:error, String.t()}
  @spec create_reservation(reservation_attrs()) :: {:ok, Reservation.t()} | {:error, String.t()}

  # Define the reactor inputs
  input(:client_id)
  input(:employee_id)
  input(:item_id)
  input(:start_datetime)
  input(:end_datetime)
  input(:notes)

  # Step 1: Validate client exists and is active
  step :validate_client do
    argument(:client_id, input(:client_id))

    run(fn %{client_id: client_id}, _context ->
      Logger.info("Validating client with ID: #{client_id}")
      
      with :ok <- validate_client_id(client_id),
           {:ok, client} <- fetch_client(client_id),
           :ok <- check_client_active(client) do
        Logger.info("Client validation successful: #{client.id}")
        {:ok, client}
      else
        {:error, reason} ->
          Logger.error("Client validation failed: #{inspect(reason)}")
          {:error, reason}
      end
    end)
  end

  # Helper functions for client validation
  defp validate_client_id(client_id) when is_integer(client_id) and client_id > 0 do
    :ok
  end

  defp validate_client_id(_client_id) do
    {:error, "Valid client ID is required"}
  end

  defp fetch_client(client_id) do
    case Client.by_id(client_id) do
      {:ok, client} -> {:ok, client}
      {:error, _} -> {:error, "Client not found"}
    end
  end

  defp check_client_active(%{archived_at: nil}), do: :ok
  defp check_client_active(%{archived_at: _archived_at}), do: {:error, "Client is archived and cannot make reservations"}

  # Step 2: Validate employee exists and is active
  step :validate_employee do
    argument(:employee_id, input(:employee_id))

    run(fn %{employee_id: employee_id}, _context ->
      Logger.info("Validating employee with ID: #{employee_id}")
      
      case Employee.by_id(employee_id) do
        {:ok, employee} ->
          if employee.archived_at do
            Logger.error("Employee validation failed: Employee is archived")
            {:error, "Employee is archived and cannot create reservations"}
          else
            Logger.info("Employee validation successful: #{employee.id}")
            {:ok, employee}
          end

        {:error, _} ->
          Logger.error("Employee validation failed: Employee not found")
          {:error, "Employee not found"}
      end
    end)
  end

  # Step 3: Validate item exists and is available
  step :validate_item do
    argument(:item_id, input(:item_id))

    run(fn %{item_id: item_id}, _context ->
      Logger.info("Validating item with ID: #{item_id}")
      
      case Item.by_id(item_id) do
        {:ok, item} ->
          if item.archived_at do
            Logger.error("Item validation failed: Item is archived")
            {:error, "Item is archived and cannot be reserved"}
          else
            Logger.info("Item validation successful: #{item.id}")
            {:ok, item}
          end

        {:error, _} ->
          Logger.error("Item validation failed: Item not found")
          {:error, "Item not found"}
      end
    end)
  end

  # Step 4: Validate datetime range
  step :validate_datetime_range do
    argument(:start_datetime, input(:start_datetime))
    argument(:end_datetime, input(:end_datetime))

    run(fn %{start_datetime: start_dt, end_datetime: end_dt}, _context ->
      Logger.info("Validating datetime range: #{start_dt} - #{end_dt}")
      
      cond do
        Timex.compare(start_dt, end_dt) != -1 ->
          Logger.error("Datetime validation failed: Start datetime must be before end datetime")
          {:error, "Start datetime must be before end datetime"}

        Timex.compare(start_dt, Timex.now()) == -1 ->
          Logger.error("Datetime validation failed: Cannot create reservations in the past")
          {:error, "Cannot create reservations in the past"}

        Timex.diff(end_dt, start_dt, :hours) > 24 ->
          Logger.error("Datetime validation failed: Reservation cannot exceed 24 hours")
          {:error, "Reservation cannot exceed 24 hours"}

        true ->
          Logger.info("Datetime validation successful")
          {:ok, %{start_datetime: start_dt, end_datetime: end_dt}}
      end
    end)
  end

  # Step 5: Check availability
  step :check_availability do
    argument(:item_id, input(:item_id))
    argument(:start_datetime, input(:start_datetime))
    argument(:end_datetime, input(:end_datetime))

    run(fn %{item_id: item_id, start_datetime: start_dt, end_datetime: end_dt}, _context ->
      Logger.info("Checking availability for item #{item_id} during #{start_dt} - #{end_dt}")
      
      case Availability.check_availability(item_id, start_dt, end_dt) do
        {:ok, true} ->
          Logger.info("Availability check successful: Item is available")
          {:ok, :available}

        {:ok, false} ->
          Logger.error("Availability check failed: Time slot is not available")
          {:error, "Time slot is not available"}

        {:error, reason} ->
          Logger.error("Availability check failed: #{inspect(reason)}")
          {:error, "Failed to check availability: #{inspect(reason)}"}
      end
    end)
  end

  # Step 6: Calculate pricing
  step :calculate_pricing do
    argument(:item, result(:validate_item))
    argument(:start_datetime, input(:start_datetime))
    argument(:end_datetime, input(:end_datetime))

    run(fn %{item: item, start_datetime: start_dt, end_datetime: end_dt}, _context ->
      Logger.info("Calculating pricing for item #{item.id} during #{start_dt} - #{end_dt}")
      
      # Calculate number of days (minimum 1 day)
      hours = Timex.diff(end_dt, start_dt, :hours)
      days = max(1, ceil(hours / 24))

      # Get pricing for the item type
      case RivaAsh.Resources.Pricing.by_item_type(item.item_type_id) do
        {:ok, [pricing | _]} ->
          total_amount = Decimal.mult(pricing.price_per_day, Decimal.new(days))
          Logger.info("Pricing calculation successful: #{total_amount} for #{days} days")

          {:ok,
           %{
             total_amount: total_amount,
             daily_rate: pricing.price_per_day,
             number_of_days: days
           }}

        {:ok, []} ->
          Logger.error("Pricing calculation failed: No pricing found for item type")
          {:error, "No pricing found for item type"}

        {:error, reason} ->
          Logger.error("Pricing calculation failed: #{inspect(reason)}")
          {:error, "Failed to calculate pricing: #{inspect(reason)}"}
      end
    end)
  end

  # Step 7: Create the reservation
  step :create_reservation do
    argument(:client_id, input(:client_id))
    argument(:employee_id, input(:employee_id))
    argument(:item_id, input(:item_id))
    argument(:start_datetime, input(:start_datetime))
    argument(:end_datetime, input(:end_datetime))
    argument(:notes, input(:notes))
    argument(:pricing, result(:calculate_pricing))

    run(fn args, _context ->
      Logger.info("Creating reservation for client #{args.client_id}, item #{args.item_id}")
      
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

      case Reservation
           |> Ash.Changeset.for_create(:create, reservation_attrs)
           |> Ash.create(domain: RivaAsh.Domain) do
        {:ok, reservation} ->
          Logger.info("Reservation created successfully: #{reservation.id}")
          {:ok, reservation}

        {:error, changeset} ->
          Logger.error("Reservation creation failed: #{format_changeset_errors(changeset)}")
          {:error, format_changeset_errors(changeset)}
      end
    end)

    compensate(fn reservation, _context ->
      Logger.info("Compensating reservation creation: Destroying reservation #{reservation.id}")
      Reservation.destroy!(reservation, domain: RivaAsh.Domain)
      :ok
    end)
  end

  # Helper function to format changeset errors
  defp format_changeset_errors(changeset) do
    case Ash.Changeset.errors(changeset) do
      [] -> "Validation failed"
      errors ->
        error_messages = errors
          |> Enum.map(fn error ->
            case error do
              %{message: message, field: field} -> "#{field}: #{message}"
              %{message: message} -> message
              error -> inspect(error)
            end
          end)
        Enum.join(error_messages, "; ")
    end
  end

  # Return the created reservation
  return(:create_reservation)
end
