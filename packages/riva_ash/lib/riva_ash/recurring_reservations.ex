defmodule RivaAsh.RecurringReservations do
  @moduledoc """
  Business logic module for handling recurring reservations.

  This module provides high-level functions for:
  - Creating recurring reservation patterns
  - Generating individual instances
  - Processing instances into actual reservations
  - Managing recurring reservation lifecycle
  """

  use Timex

  alias RivaAsh.Resources.{RecurringReservation, RecurringReservationInstance, Reservation}
  import OK, only: [success: 1, failure: 1, ~>>: 2]

  @doc """
  Creates a new recurring reservation and optionally generates instances immediately.

  ## Parameters
  - attrs: Map of attributes for the recurring reservation
  - generate_immediately: Boolean, whether to generate instances right away (default: true)

  ## Returns
  - {:ok, recurring_reservation} if successful
  - {:error, reason} if failed
  """
  def create_recurring_reservation(attrs, generate_immediately \\ true) do
    create_recurring_reservation_record(attrs)
    ~>> fn recurring_reservation ->
      maybe_generate_instances(recurring_reservation, generate_immediately)
    end
  end

  @doc """
  Generates instances for an existing recurring reservation.

  ## Parameters
  - recurring_reservation_id: UUID of the recurring reservation

  ## Returns
  - {:ok, updated_recurring_reservation} if successful
  - {:error, reason} if failed
  """
  def generate_instances(recurring_reservation_id) do
    get_recurring_reservation(recurring_reservation_id)
    ~>> fn recurring_reservation ->
      calculate_consecutive_dates(recurring_reservation)
      ~>> fn dates ->
        create_instances_for_dates(recurring_reservation, dates)
        ~>> fn _instances ->
          update_recurring_reservation_status(recurring_reservation, :active)
        end
      end
    end
    |> case do
      {:ok, result} -> success(result)
      {:error, reason} -> failure("Failed to generate instances: #{inspect(reason)}")
    end
  end

  @doc """
  Processes a specific instance to create the actual reservation.

  ## Parameters
  - instance_id: UUID of the recurring reservation instance

  ## Returns
  - {:ok, updated_instance} if successful
  - {:error, reason} if failed
  """
  def process_instance(instance_id) do
    get_instance(instance_id)
    ~>> fn instance ->
      get_recurring_reservation(instance.recurring_reservation_id)
      ~>> fn recurring_reservation ->
        check_availability(instance, recurring_reservation)
        ~>> fn availability ->
          create_or_fail_reservation(instance, recurring_reservation, availability)
        end
      end
    end
    |> case do
      {:ok, result} -> success(result)
      {:error, reason} -> failure("Failed to process instance: #{inspect(reason)}")
    end
  end

  @doc """
  Processes all pending instances for a recurring reservation.

  ## Parameters
  - recurring_reservation_id: UUID of the recurring reservation

  ## Returns
  - {:ok, results} with list of processed instances
  - {:error, reason} if failed
  """
  def process_all_instances(recurring_reservation_id) do
    get_pending_instances(recurring_reservation_id)
    ~>> fn instances ->
      results = instances
      |> Enum.map(fn instance ->
        case process_instance(instance.id) do
          {:ok, updated_instance} -> success(updated_instance)
          {:error, reason} -> failure({instance.id, reason})
        end
      end)

      successes = results |> Enum.filter(&match?({:ok, _}, &1)) |> Enum.map(fn {:ok, instance} -> instance end)
      failures = results |> Enum.filter(&match?({:error, _}, &1)) |> Enum.map(fn {:error, error} -> error end)

      %{successes: successes, failures: failures, total: length(instances)}
    end
  end

  @doc """
  Cancels a recurring reservation and all its pending instances.

  ## Parameters
  - recurring_reservation_id: UUID of the recurring reservation
  - reason: Optional reason for cancellation

  ## Returns
  - {:ok, updated_recurring_reservation} if successful
  - {:error, reason} if failed
  """
  def cancel_recurring_reservation(recurring_reservation_id, reason \\ nil) do
    get_recurring_reservation(recurring_reservation_id)
    ~>> fn recurring_reservation ->
      cancel_pending_instances(recurring_reservation_id, reason)
      ~>> fn _ ->
        update_recurring_reservation_status(recurring_reservation, :cancelled)
      end
    end
  end

  @doc """
  Gets statistics for a recurring reservation.

  ## Parameters
  - recurring_reservation_id: UUID of the recurring reservation

  ## Returns
  - {:ok, stats} with counts of instances by status
  - {:error, reason} if failed
  """
  def get_recurring_reservation_stats(recurring_reservation_id) do
    RecurringReservationInstance
    |> Ash.Query.for_read(:by_recurring_reservation, %{recurring_reservation_id: recurring_reservation_id})
    |> Ash.read(domain: RivaAsh.Domain)
    ~>> fn instances ->
      stats = instances
      |> Enum.group_by(& &1.status)
      |> Enum.map(fn {status, instances} -> {status, length(instances)} end)
      |> Enum.into(%{})

      Map.merge(%{pending: 0, confirmed: 0, failed: 0, skipped: 0}, stats)
    end
  end

  # Private helper functions

  defp create_recurring_reservation_record(attrs) do
    RecurringReservation
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.create(domain: RivaAsh.Domain)
  end

  defp maybe_generate_instances(recurring_reservation, true) do
    generate_instances(recurring_reservation.id)
  end

  defp maybe_generate_instances(recurring_reservation, false) do
    {:ok, recurring_reservation}
  end

  defp get_recurring_reservation(id) do
    RecurringReservation
    |> Ash.get(id, domain: RivaAsh.Domain)
  end

  defp get_pending_instances(recurring_reservation_id) do
    # Get all instances for this recurring reservation, then filter in memory
    case RecurringReservationInstance
         |> Ash.Query.for_read(:by_recurring_reservation, %{recurring_reservation_id: recurring_reservation_id})
         |> Ash.read(domain: RivaAsh.Domain) do
      {:ok, instances} ->
        pending_instances = instances
        |> Enum.filter(&(&1.status == :pending))
        |> Enum.sort_by(&(&1.sequence_number))

        success(pending_instances)
      {:error, reason} ->
        failure(reason)
    end
  end

  defp cancel_pending_instances(recurring_reservation_id, reason) do
    with {:ok, instances} <- get_pending_instances(recurring_reservation_id) do
      results = instances
      |> Enum.map(fn instance ->
        attrs = %{
          status: :skipped,
          skip_reason: reason || "Recurring reservation cancelled",
          failed_at: Timex.now()
        }

        instance
        |> Ash.Changeset.for_update(:update, attrs)
        |> Ash.update(domain: RivaAsh.Domain)
      end)

      # Check if all updates succeeded
      errors = results |> Enum.filter(&match?({:error, _}, &1))

      if Enum.empty?(errors) do
        success(:cancelled)
      else
        failure("Failed to cancel some instances: #{inspect(errors)}")
      end
    else
      {:error, reason} -> failure(reason)
    end
  end

  defp update_recurring_reservation_status(recurring_reservation, status) do
    recurring_reservation
    |> Ash.Changeset.for_update(:update, %{status: status})
    |> Ash.update(domain: RivaAsh.Domain)
  end

  # New helper functions for the simplified implementation

  defp calculate_consecutive_dates(recurring_reservation) do
    start_date = recurring_reservation.start_date
    consecutive_days = recurring_reservation.consecutive_days
    pattern_type = recurring_reservation.pattern_type

    dates = case pattern_type do
      :all_days ->
        # Generate all consecutive days
        0..(consecutive_days - 1)
        |> Enum.map(fn day_offset ->
          Timex.shift(start_date, days: day_offset)
        end)

      :weekdays_only ->
        # Generate only weekdays (Monday-Friday)
        generate_weekdays_only(start_date, consecutive_days)

      _ ->
        # Default to all days
        0..(consecutive_days - 1)
        |> Enum.map(fn day_offset ->
          Timex.shift(start_date, days: day_offset)
        end)
    end

    success(dates)
  end

  defp generate_weekdays_only(start_date, target_days) do
    Stream.iterate(start_date, &Timex.shift(&1, days: 1))
    |> Stream.filter(fn date ->
      # Monday = 1, Sunday = 7 in Timex
      Timex.weekday(date) in 1..5
    end)
    |> Enum.take(target_days)
  end

  defp create_instances_for_dates(recurring_reservation, dates) do
    instances = dates
    |> Enum.with_index(1)
    |> Enum.map(fn {date, sequence_number} ->
      %{
        recurring_reservation_id: recurring_reservation.id,
        scheduled_date: date,
        sequence_number: sequence_number,
        status: :pending,
        notes: nil,
        skip_reason: nil
      }
    end)

    # Batch create all instances
    results = instances
    |> Enum.map(fn instance_attrs ->
      RecurringReservationInstance
      |> Ash.Changeset.for_create(:create, instance_attrs)
      |> Ash.create(domain: RivaAsh.Domain)
    end)

    # Check if all succeeded
    errors = results |> Enum.filter(&match?({:error, _}, &1))

    if Enum.empty?(errors) do
      successes = results |> Enum.map(fn {:ok, instance} -> instance end)
      success(successes)
    else
      failure(errors)
    end
  end

  defp get_instance(instance_id) do
    RecurringReservationInstance
    |> Ash.get(instance_id, domain: RivaAsh.Domain)
  end

  defp check_availability(instance, recurring_reservation) do
    scheduled_date = instance.scheduled_date
    start_time = recurring_reservation.start_time
    end_time = recurring_reservation.end_time

    # Convert date + time to datetime
    start_datetime = DateTime.new!(scheduled_date, start_time, "Etc/UTC")
    end_datetime = DateTime.new!(scheduled_date, end_time, "Etc/UTC")

    item_id = recurring_reservation.item_id

    # Use standardized overlap checking logic
    RivaAsh.Validations.check_reservation_overlap(
       item_id,
       start_datetime,
       end_datetime,
       nil,
       [include_provisional: false]  # Don't include provisional for recurring reservations
     )
    ~>> case do
      :no_overlap ->
        # Also check item availability (schedules, holds, etc.)
        RivaAsh.Validations.check_item_availability(
           item_id,
           start_datetime,
           end_datetime,
           [check_holds: true]
         )
        |> case do
          {:ok, :available} ->
            success(%{
              available: true,
              start_datetime: start_datetime,
              end_datetime: end_datetime
            })
          {:ok, :unavailable, reason} ->
            success(%{
              available: false,
              reason: reason
            })
          {:error, error} ->
            failure("Failed to check item availability: #{error}")
        end
      :overlap_found ->
        success(%{
          available: false,
          reason: "Time slot conflicts with existing reservation"
        })
    end
    |> case do
      {:ok, result} -> success(result)
      {:error, error} -> failure("Failed to check reservation overlap: #{error}")
    end
  end

  defp create_or_fail_reservation(instance, recurring_reservation, availability) do
    if availability.available do
      create_reservation_for_instance(instance, recurring_reservation, availability)
    else
      mark_instance_failed(instance, availability.reason)
    end
  end

  defp create_reservation_for_instance(instance, recurring_reservation, availability) do
    reservation_attrs = %{
      client_id: recurring_reservation.client_id,
      item_id: recurring_reservation.item_id,
      employee_id: recurring_reservation.employee_id,
      reserved_from: availability.start_datetime,
      reserved_until: availability.end_datetime,
      notes: build_reservation_notes(instance, recurring_reservation)
    }

    Reservation
    |> Ash.Changeset.for_create(:create, reservation_attrs)
    |> Ash.create(domain: RivaAsh.Domain)
    ~>> fn reservation ->
      # Link reservation and mark as confirmed
      update_instance_with_reservation(instance, reservation)
    end
    |> case do
      {:ok, result} -> success(result)
      {:error, error} ->
        # Mark as failed due to reservation creation error
        error_message = format_error_message(error)
        mark_instance_failed(instance, error_message)
    end
  end

  defp mark_instance_failed(instance, reason) do
    attrs = %{
      status: :failed,
      error_message: reason,
      failed_at: Timex.now()
    }

    instance
    |> Ash.Changeset.for_update(:update, attrs)
    |> Ash.update(domain: RivaAsh.Domain)
  end

  defp update_instance_with_reservation(instance, reservation) do
    attrs = %{
      status: :confirmed,
      reservation_id: reservation.id,
      created_at: Timex.now(),
      error_message: nil,
      failed_at: nil
    }

    instance
    |> Ash.Changeset.for_update(:update, attrs)
    |> Ash.update(domain: RivaAsh.Domain)
  end

  defp build_reservation_notes(instance, recurring_reservation) do
    base_notes = recurring_reservation.notes || ""
    instance_notes = instance.notes || ""

    recurring_info = "Recurring reservation instance #{instance.sequence_number}/#{recurring_reservation.consecutive_days}"

    [recurring_info, base_notes, instance_notes]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" | ")
  end

  defp format_error_message(error) do
    # Simple error formatting without relying on specific error structs
    case error do
      %{errors: errors} when is_list(errors) ->
        errors
        |> Enum.map(fn
          %{message: message} -> message
          error -> inspect(error)
        end)
        |> Enum.join(", ")
      %{message: message} ->
        message
      _ ->
        inspect(error)
    end
  end
end
