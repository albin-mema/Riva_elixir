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
  alias RivaAsh.ErrorHelpers

  @doc """
  Creates a new recurring reservation and optionally generates instances immediately.

  ## Parameters
  - attrs: Map of attributes for the recurring reservation
  - generate_immediately: Boolean, whether to generate instances right away (default: true)

  ## Returns
  - {:ok, recurring_reservation} if successful
  - {:error, reason} if failed
  """
  @spec create_recurring_reservation(map(), boolean()) ::
        {:ok, RecurringReservation.t()} | {:error, any()}
  def create_recurring_reservation(attrs, generate_immediately \\ true) do
    with {:ok, recurring_reservation} <- create_recurring_reservation_record(attrs),
         {:ok, result} <- maybe_generate_instances(recurring_reservation, generate_immediately) do
      {:ok, result}
    else
      {:error, reason} -> {:error, reason}
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
  @spec generate_instances(String.t()) :: {:ok, RecurringReservation.t()} | {:error, any()}
  def generate_instances(recurring_reservation_id) when is_binary(recurring_reservation_id) do
    with {:ok, recurring_reservation} <- get_recurring_reservation(recurring_reservation_id),
         {:ok, dates} <- calculate_consecutive_dates(recurring_reservation),
         {:ok, _instances} <- create_instances_for_dates(recurring_reservation, dates),
         {:ok, result} <- update_recurring_reservation_status(recurring_reservation, :active) do
      {:ok, result}
    else
      {:error, reason} -> {:error, "Failed to generate instances: #{inspect(reason)}"}
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
  @spec process_instance(String.t()) :: {:ok, RecurringReservationInstance.t()} | {:error, any()}
  def process_instance(instance_id) when is_binary(instance_id) do
    with {:ok, instance} <- get_instance(instance_id),
         {:ok, recurring_reservation} <-
           get_recurring_reservation(instance.recurring_reservation_id),
         {:ok, availability} <- check_availability(instance, recurring_reservation),
         {:ok, result} <- create_or_fail_reservation(instance, recurring_reservation, availability) do
      {:ok, result}
    else
      {:error, reason} -> {:error, "Failed to process instance: #{inspect(reason)}"}
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
  @spec process_all_instances(String.t()) ::
        {:ok, %{successes: list(), failures: list(), total: non_neg_integer()}} | {:error, any()}
  def process_all_instances(recurring_reservation_id) when is_binary(recurring_reservation_id) do
    with {:ok, instances} <- get_pending_instances(recurring_reservation_id),
         {:ok, results} <- process_instances_batch(instances) do
      {:ok, results}
    else
      {:error, reason} -> {:error, reason}
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
  @spec cancel_recurring_reservation(String.t(), String.t() | nil) ::
        {:ok, RecurringReservation.t()} | {:error, any()}
  def cancel_recurring_reservation(recurring_reservation_id, reason \\ nil)
      when is_binary(recurring_reservation_id) do
    with {:ok, recurring_reservation} <- get_recurring_reservation(recurring_reservation_id),
         {:ok, _} <- cancel_pending_instances(recurring_reservation_id, reason),
         {:ok, result} <- update_recurring_reservation_status(recurring_reservation, :cancelled) do
      {:ok, result}
    else
      {:error, reason} -> {:error, reason}
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
  @spec get_recurring_reservation_stats(String.t()) :: {:ok, map()} | {:error, any()}
  def get_recurring_reservation_stats(recurring_reservation_id) when is_binary(recurring_reservation_id) do
    with {:ok, instances} <- get_recurring_reservation_instances(recurring_reservation_id),
         {:ok, stats} <- calculate_instance_stats(instances) do
      {:ok, stats}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # Private helper functions

  @spec create_recurring_reservation_record(map()) :: {:ok, RecurringReservation.t()} | {:error, any()}
  defp create_recurring_reservation_record(attrs) when is_map(attrs) do
    RecurringReservation
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.create(domain: RivaAsh.Domain)
  end

  @spec maybe_generate_instances(RecurringReservation.t(), boolean()) ::
        {:ok, RecurringReservation.t()} | {:error, any()}
  defp maybe_generate_instances(recurring_reservation, true) do
    generate_instances(recurring_reservation.id)
  end

  defp maybe_generate_instances(recurring_reservation, false) do
    {:ok, recurring_reservation}
  end

  @spec get_recurring_reservation(String.t()) :: {:ok, RecurringReservation.t()} | {:error, any()}
  defp get_recurring_reservation(id) when is_binary(id) do
    RecurringReservation
    |> Ash.get(id, domain: RivaAsh.Domain)
  end

  @spec get_pending_instances(String.t()) :: {:ok, list(RecurringReservationInstance.t())} | {:error, any()}
  defp get_pending_instances(recurring_reservation_id) when is_binary(recurring_reservation_id) do
    with {:ok, instances} <- get_recurring_reservation_instances(recurring_reservation_id) do
      pending_instances =
        instances
        |> Enum.filter(&(&1.status == :pending))
        |> Enum.sort_by(& &1.sequence_number)

      {:ok, pending_instances}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @spec get_recurring_reservation_instances(String.t()) ::
        {:ok, list(RecurringReservationInstance.t())} | {:error, any()}
  defp get_recurring_reservation_instances(recurring_reservation_id) do
    RecurringReservationInstance
    |> Ash.Query.for_read(:by_recurring_reservation, %{
      recurring_reservation_id: recurring_reservation_id
    })
    |> Ash.read(domain: RivaAsh.Domain)
  end

  @spec process_instances_batch(list(RecurringReservationInstance.t())) ::
        {:ok, %{successes: list(), failures: list(), total: non_neg_integer()}} | {:error, any()}
  defp process_instances_batch(instances) do
    results =
      instances
      |> Enum.map(fn instance ->
        case process_instance(instance.id) do
          {:ok, updated_instance} -> {:ok, updated_instance}
          {:error, reason} -> {:error, {instance.id, reason}}
        end
      end)

    successes =
      results
      |> Enum.filter(&match?({:ok, _}, &1))
      |> Enum.map(fn {:ok, instance} -> instance end)

    failures =
      results
      |> Enum.filter(&match?({:error, _}, &1))
      |> Enum.map(fn {:error, error} -> error end)

    {:ok, %{successes: successes, failures: failures, total: length(instances)}}
  end

  @spec cancel_pending_instances(String.t(), String.t() | nil) ::
        {:ok, :cancelled} | {:error, any()}
  defp cancel_pending_instances(recurring_reservation_id, reason)
      when is_binary(recurring_reservation_id) do
    with {:ok, instances} <- get_pending_instances(recurring_reservation_id),
         {:ok, _} <- update_instances_status(instances, :skipped, reason) do
      {:ok, :cancelled}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @spec update_instances_status(list(RecurringReservationInstance.t()), atom(), String.t() | nil) ::
        {:ok, :updated} | {:error, any()}
  defp update_instances_status(instances, status, reason) when is_list(instances) and is_atom(status) do
    update_attrs = %{
      status: status,
      skip_reason: reason || "Recurring reservation cancelled",
      failed_at: Timex.now()
    }

    results =
      instances
      |> Enum.map(fn instance ->
        instance
        |> Ash.Changeset.for_update(:update, update_attrs)
        |> Ash.update(domain: RivaAsh.Domain)
      end)

    # Check if all updates succeeded
    errors = Enum.filter(results, &match?({:error, _}, &1))

    if Enum.empty?(errors) do
      {:ok, :updated}
    else
      {:error, "Failed to update some instances: #{inspect(errors)}"}
    end
  end

  @spec update_recurring_reservation_status(RecurringReservation.t(), atom()) ::
        {:ok, RecurringReservation.t()} | {:error, any()}
  defp update_recurring_reservation_status(recurring_reservation, status) when is_atom(status) do
    recurring_reservation
    |> Ash.Changeset.for_update(:update, %{status: status})
    |> Ash.update(domain: RivaAsh.Domain)
  end

  @spec calculate_instance_stats(list(RecurringReservationInstance.t())) ::
        {:ok, map()} | {:error, any()}
  defp calculate_instance_stats(instances) when is_list(instances) do
    stats =
      instances
      |> Enum.group_by(& &1.status)
      |> Enum.map(fn {status, instances} -> {status, length(instances)} end)
      |> Enum.into(%{})

    default_stats = %{pending: 0, confirmed: 0, failed: 0, skipped: 0}
    {:ok, Map.merge(default_stats, stats)}
  end

  @spec calculate_consecutive_dates(RecurringReservation.t()) :: {:ok, list(Date.t())} | {:error, any()}
  defp calculate_consecutive_dates(recurring_reservation) do
    with {:ok, start_date} <- validate_date(recurring_reservation.start_date),
         {:ok, consecutive_days} <- validate_consecutive_days(recurring_reservation.consecutive_days),
         {:ok, pattern_type} <- validate_pattern_type(recurring_reservation.pattern_type),
         {:ok, dates} <- generate_dates_for_pattern(start_date, consecutive_days, pattern_type) do
      {:ok, dates}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @spec validate_date(Date.t()) :: {:ok, Date.t()} | {:error, any()}
  defp validate_date(date) when is_struct(date, Date) do
    {:ok, date}
  end

  defp validate_date(_), do: {:error, "Invalid start date"}

  @spec validate_consecutive_days(non_neg_integer()) :: {:ok, non_neg_integer()} | {:error, any()}
  defp validate_consecutive_days(consecutive_days) when is_integer(consecutive_days) and consecutive_days >= 0 do
    {:ok, consecutive_days}
  end

  defp validate_consecutive_days(_), do: {:error, "Invalid consecutive days"}

  @spec validate_pattern_type(atom()) :: {:ok, atom()} | {:error, any()}
  defp validate_pattern_type(pattern_type) when is_atom(pattern_type) do
    valid_patterns = [:all_days, :weekdays_only]
    if pattern_type in valid_patterns do
      {:ok, pattern_type}
    else
      {:error, "Invalid pattern type: #{inspect(pattern_type)}"}
    end
  end

  defp validate_pattern_type(_), do: {:error, "Invalid pattern type"}

  @spec generate_dates_for_pattern(Date.t(), non_neg_integer(), atom()) ::
        {:ok, list(Date.t())} | {:error, any()}
  defp generate_dates_for_pattern(start_date, consecutive_days, :all_days) do
    dates =
      0..(consecutive_days - 1)
      |> Enum.map(fn day_offset ->
        Timex.shift(start_date, days: day_offset)
      end)

    {:ok, dates}
  end

  defp generate_dates_for_pattern(start_date, consecutive_days, :weekdays_only) do
    dates = generate_weekdays_only(start_date, consecutive_days)
    {:ok, dates}
  end

  defp generate_dates_for_pattern(start_date, consecutive_days, _pattern_type) do
    # Default to all days
    dates =
      0..(consecutive_days - 1)
      |> Enum.map(fn day_offset ->
        Timex.shift(start_date, days: day_offset)
      end)

    {:ok, dates}
  end

  @spec generate_weekdays_only(Date.t(), non_neg_integer()) :: list(Date.t())
  defp generate_weekdays_only(start_date, target_days)
      when is_struct(start_date, Date) and is_integer(target_days) and target_days >= 0 do
    Stream.iterate(start_date, &Timex.shift(&1, days: 1))
    |> Stream.filter(fn date ->
      # Monday = 1, Sunday = 7 in Timex
      Timex.weekday(date) in 1..5
    end)
    |> Enum.take(target_days)
  end

  @spec create_instances_for_dates(RecurringReservation.t(), list(Date.t())) ::
        {:ok, list(RecurringReservationInstance.t())} | {:error, any()}
  defp create_instances_for_dates(recurring_reservation, dates) when is_list(dates) do
    instances =
      dates
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
    results =
      instances
      |> Enum.map(fn instance_attrs ->
        RecurringReservationInstance
        |> Ash.Changeset.for_create(:create, instance_attrs)
        |> Ash.create(domain: RivaAsh.Domain)
      end)

    # Check if all succeeded
    errors = Enum.filter(results, &match?({:error, _}, &1))

    if Enum.empty?(errors) do
      successes = Enum.map(results, fn {:ok, instance} -> instance end)
      {:ok, successes}
    else
      {:error, errors}
    end
  end

  @spec get_instance(String.t()) :: {:ok, RecurringReservationInstance.t()} | {:error, any()}
  defp get_instance(instance_id) when is_binary(instance_id) do
    RecurringReservationInstance
    |> Ash.get(instance_id, domain: RivaAsh.Domain)
  end

  @spec check_availability(RecurringReservationInstance.t(), RecurringReservation.t()) ::
        {:ok, map()} | {:error, any()}
  defp check_availability(instance, recurring_reservation) do
    with {:ok, scheduled_date} <- validate_date(instance.scheduled_date),
         {:ok, start_time} <- validate_time(recurring_reservation.start_time),
         {:ok, end_time} <- validate_time(recurring_reservation.end_time),
         {:ok, start_datetime} <- create_datetime(scheduled_date, start_time),
         {:ok, end_datetime} <- create_datetime(scheduled_date, end_time),
         {:ok, item_id} <- validate_id(recurring_reservation.item_id),
         {:ok, overlap_result} <- check_reservation_overlap(item_id, start_datetime, end_datetime),
         {:ok, availability_result} <- check_item_availability(item_id, start_datetime, end_datetime) do
      build_availability_result(overlap_result, availability_result, start_datetime, end_datetime)
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @spec validate_time(Time.t()) :: {:ok, Time.t()} | {:error, any()}
  defp validate_time(time) when is_struct(time, Time) do
    {:ok, time}
  end

  defp validate_time(_), do: {:error, "Invalid time"}

  @spec validate_id(String.t()) :: {:ok, String.t()} | {:error, any()}
  defp validate_id(id) when is_binary(id) do
    {:ok, id}
  end

  defp validate_id(_), do: {:error, "Invalid ID"}

  @spec create_datetime(Date.t(), Time.t()) :: {:ok, DateTime.t()} | {:error, any()}
  defp create_datetime(date, time) do
    case DateTime.new(date, time, "Etc/UTC") do
      {:ok, datetime} -> {:ok, datetime}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec check_reservation_overlap(String.t(), DateTime.t(), DateTime.t()) ::
        {:ok, atom()} | {:error, any()}
  defp check_reservation_overlap(item_id, start_datetime, end_datetime) do
    RivaAsh.Validations.check_reservation_overlap(
      item_id,
      start_datetime,
      end_datetime,
      nil,
      include_provisional: false
    )
  end

  @spec check_item_availability(String.t(), DateTime.t(), DateTime.t()) ::
        {:ok, atom()} | {:ok, atom(), any()} | {:error, any()}
  defp check_item_availability(item_id, start_datetime, end_datetime) do
    RivaAsh.Validations.check_item_availability(
      item_id,
      start_datetime,
      end_datetime,
      check_holds: true
    )
  end

  @spec build_availability_result(atom(), atom() | {:ok, atom(), any()}, DateTime.t(), DateTime.t()) ::
        {:ok, map()} | {:error, any()}
  defp build_availability_result(:no_overlap, {:ok, :available}, start_datetime, end_datetime) do
    {:ok, %{
      available: true,
      start_datetime: start_datetime,
      end_datetime: end_datetime
    }}
  end

  defp build_availability_result(:no_overlap, {:ok, :unavailable, reason}, _start_datetime, _end_datetime) do
    {:ok, %{
      available: false,
      reason: reason
    }}
  end

  defp build_availability_result(:no_overlap, _result, _start_datetime, _end_datetime) do
    {:ok, %{
      available: false,
      reason: "Time slot conflicts with existing reservation"
    }}
  end

  defp build_availability_result({:error, error}, _result, _start_datetime, _end_datetime) do
    {:error, "Failed to check reservation overlap: #{error}"}
  end

  defp build_availability_result(_overlap_result, {:error, error}, _start_datetime, _end_datetime) do
    {:error, "Failed to check item availability: #{error}"}
  end

  @spec create_or_fail_reservation(RecurringReservationInstance.t(), RecurringReservation.t(), map()) ::
        {:ok, RecurringReservationInstance.t()} | {:error, any()}
  defp create_or_fail_reservation(instance, recurring_reservation, availability) do
    if availability.available do
      create_reservation_for_instance(instance, recurring_reservation, availability)
    else
      mark_instance_failed(instance, availability.reason)
    end
  end

  @spec create_reservation_for_instance(RecurringReservationInstance.t(), RecurringReservation.t(), map()) ::
        {:ok, RecurringReservationInstance.t()} | {:error, any()}
  defp create_reservation_for_instance(instance, recurring_reservation, availability) do
    reservation_attrs = %{
      client_id: recurring_reservation.client_id,
      item_id: recurring_reservation.item_id,
      employee_id: recurring_reservation.employee_id,
      reserved_from: availability.start_datetime,
      reserved_until: availability.end_datetime,
      notes: build_reservation_notes(instance, recurring_reservation)
    }

    case Reservation
         |> Ash.Changeset.for_create(:create, reservation_attrs)
         |> Ash.create(domain: RivaAsh.Domain) do
      {:ok, reservation} ->
        # Link reservation and mark as confirmed
        update_instance_with_reservation(instance, reservation)

      {:error, error} ->
        # Mark as failed due to reservation creation error
        error_message = format_error_message(error)
        mark_instance_failed(instance, error_message)
    end
  end

  @spec mark_instance_failed(RecurringReservationInstance.t(), String.t()) ::
        {:ok, RecurringReservationInstance.t()} | {:error, any()}
  defp mark_instance_failed(instance, reason) when is_binary(reason) do
    attrs = %{
      status: :failed,
      error_message: reason,
      failed_at: Timex.now()
    }

    instance
    |> Ash.Changeset.for_update(:update, attrs)
    |> Ash.update(domain: RivaAsh.Domain)
  end

  @spec update_instance_with_reservation(RecurringReservationInstance.t(), Reservation.t()) ::
        {:ok, RecurringReservationInstance.t()} | {:error, any()}
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

  @spec build_reservation_notes(RecurringReservationInstance.t(), RecurringReservation.t()) :: String.t()
  defp build_reservation_notes(instance, recurring_reservation) do
    base_notes = recurring_reservation.notes || ""
    instance_notes = instance.notes || ""

    recurring_info =
      "Recurring reservation instance #{instance.sequence_number}/#{recurring_reservation.consecutive_days}"

    [recurring_info, base_notes, instance_notes]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" | ")
  end

  @spec format_error_message(any()) :: String.t()
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
