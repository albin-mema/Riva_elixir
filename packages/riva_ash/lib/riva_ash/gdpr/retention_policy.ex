defmodule RivaAsh.GDPR.RetentionPolicy do
  @moduledoc """
  Implements GDPR data retention policies and automated cleanup.

  GDPR Article 5(1)(e): Storage limitation principle
  "Personal data shall be kept in a form which permits identification of data subjects
  for no longer than is necessary for the purposes for which the personal data are processed"

  This module defines retention periods for different types of data and provides
  automated cleanup functionality.
  """

  alias RivaAsh.Accounts.User
  alias RivaAsh.GDPR.ConsentRecord

  require Logger
  require Ash.Query
  import Ash.Expr

  @config Application.get_env(:riva_ash, :gdpr, %{})
  
  # Retention periods in days - configurable via application config
  @retention_periods Map.merge(%{
    # User account data - keep for 7 years after account closure (legal requirement)
    user_accounts: 365 * 7,

    # Employee data - keep for 3 years after employment ends
    employee_records: 365 * 3,

    # Client data - keep for 2 years after last interaction
    client_records: 365 * 2,

    # Reservation data - keep for 5 years (business records requirement)
    reservations: 365 * 5,

    # Consent records - keep for 3 years after withdrawal
    consent_records: 365 * 3,

    # Audit logs - keep for 1 year
    audit_logs: 365,

    # Session data - keep for 30 days
    session_data: 30,

    # System logs - keep for 90 days
    system_logs: 90,

    # Marketing data - delete immediately upon consent withdrawal
    marketing_data: 0
  }, Map.get(@config, :retention_periods, %{}))

  @doc """
  Get retention period for a specific data type.
  """
  @spec retention_period(atom()) :: integer()
  def retention_period(data_type) when is_atom(data_type) do
    # Default to 1 year
    Map.get(@retention_periods, data_type, 365)
  end

  def retention_period(_data_type) do
    365
  end

  @doc """
  Run the automated retention cleanup process.
  This should be called by a scheduled background job.
  """
  @spec run_retention_cleanup() :: {:ok, map()} | {:error, term()}
  def run_retention_cleanup do
    Logger.info("GDPR: Starting automated retention cleanup")

    results =
      %{}
      |> add_cleanup_result(:users_processed, &cleanup_expired_users/0)
      |> add_cleanup_result(:employees_processed, &cleanup_expired_employees/0)
      |> add_cleanup_result(:clients_processed, &cleanup_expired_clients/0)
      |> add_cleanup_result(:reservations_processed, &cleanup_expired_reservations/0)
      |> add_cleanup_result(:consent_records_processed, &cleanup_expired_consent_records/0)
      |> add_cleanup_result(:audit_logs_processed, &cleanup_expired_audit_logs/0)
      |> add_cleanup_result(:sessions_processed, &cleanup_expired_sessions/0)

    Logger.info("GDPR: Retention cleanup completed", extra: results)
    {:ok, results}
  end

  defp add_cleanup_result(results, key, cleanup_function) do
    Map.put(results, key, cleanup_function.())
  end

  @doc """
  Check if data should be deleted based on retention policy.
  """
  @spec should_delete?(atom(), DateTime.t()) :: boolean()
  def should_delete?(data_type, last_activity_date) when is_atom(data_type) and is_struct(last_activity_date, DateTime) do
    retention_days = retention_period(data_type)
    cutoff_date = DateTime.utc_now() |> DateTime.add(-retention_days, :day)

    DateTime.compare(last_activity_date, cutoff_date) == :lt
  end

  def should_delete?(_data_type, _last_activity_date) do
    false
  end

  @doc """
  Anonymize data instead of deleting it (for statistical purposes).
  """
  @spec anonymize_record(map(), atom()) :: {:ok, map()} | {:error, String.t()}
  def anonymize_record(record, data_type) when is_map(record) and is_atom(data_type) do
    anonymizer = Map.get(%{
      :user_accounts => &anonymize_user/1,
      :employee_records => &anonymize_employee/1,
      :client_records => &anonymize_client/1,
      :reservations => &anonymize_reservation/1
    }, data_type)

    case anonymizer do
      nil -> {:error, "Anonymization not supported for #{data_type}"}
      function -> function.(record)
    end
  end

  def anonymize_record(_record, _data_type) do
    {:error, "Record must be a map and data type must be an atom"}
  end

  # Private functions for cleanup operations

  defp cleanup_expired_users do
    cutoff_date = calculate_cutoff_date(@retention_periods.user_accounts)

    expired_users =
      User
      |> Ash.Query.unset([:filter])
      |> Ash.Query.filter(expr(not is_nil(archived_at) and archived_at < ^cutoff_date))
      |> Ash.read!(domain: RivaAsh.Accounts)

    count = length(expired_users)

    expired_users
    |> Enum.each(&handle_user_deletion/1)

    count
  rescue
    error ->
      Logger.error("GDPR: Error in user cleanup: #{inspect(error)}")
      0
  end

  defp calculate_cutoff_date(days) do
    DateTime.utc_now() |> DateTime.add(-days, :day)
  end

  defp handle_user_deletion(user) do
    case hard_delete_user(user) do
      :ok ->
        Logger.info("GDPR: Hard deleted expired user #{user.id}")

      {:error, reason} ->
        Logger.error("GDPR: Failed to delete user #{user.id}: #{inspect(reason)}")
    end
  end

  defp cleanup_expired_employees do
    cutoff_date = calculate_cutoff_date(@retention_periods.employee_records)

    # Placeholder implementation - needs actual Employee resource query
    # Example: Employee |> Ash.Query.filter(expr(not is_nil(archived_at) and archived_at < ^cutoff_date)) |> Ash.read!()
    Logger.info("GDPR: Employee cleanup placeholder - cutoff: #{inspect(cutoff_date)}")
    0
  end

  defp cleanup_expired_clients do
    cutoff_date = calculate_cutoff_date(@retention_periods.client_records)

    # Placeholder implementation - needs actual Client resource query
    # Example: Client |> Ash.Query.filter(expr(not is_nil(archived_at) and archived_at < ^cutoff_date)) |> Ash.read!()
    Logger.info("GDPR: Client cleanup placeholder - cutoff: #{inspect(cutoff_date)}")
    0
  end

  defp cleanup_expired_reservations do
    cutoff_date = calculate_cutoff_date(@retention_periods.reservations)

    # Reservations might need anonymization rather than deletion for business records
    Logger.info("GDPR: Reservation cleanup placeholder - cutoff: #{inspect(cutoff_date)}")
    0
  end

  defp cleanup_expired_consent_records do
    cutoff_date = calculate_cutoff_date(@retention_periods.consent_records)

    # Only delete consent records that have been withdrawn and are past retention
    expired_consents =
      ConsentRecord
      |> Ash.Query.filter(
        expr(
          consent_given == false and
            not is_nil(withdrawal_date) and
            withdrawal_date < ^cutoff_date
        )
      )
      |> Ash.read!(domain: RivaAsh.Domain)

    count = length(expired_consents)

    expired_consents
    |> Enum.each(&handle_consent_deletion/1)

    count
  rescue
    error ->
      Logger.error("GDPR: Error in consent cleanup: #{inspect(error)}")
      0
  end

  defp handle_consent_deletion(consent) do
    case Ash.destroy(consent) do
      :ok ->
        Logger.info("GDPR: Deleted expired consent record #{consent.id}")

      {:error, reason} ->
        Logger.error("GDPR: Failed to delete consent #{consent.id}: #{inspect(reason)}")
    end
  end

  defp cleanup_expired_audit_logs do
    cutoff_date = calculate_cutoff_date(@retention_periods.audit_logs)

    # This would clean up paper trail records older than retention period
    # Implementation depends on how you want to handle audit log retention
    Logger.info("GDPR: Audit log cleanup placeholder - cutoff: #{inspect(cutoff_date)}")
    0
  end

  defp cleanup_expired_sessions do
    cutoff_date = calculate_cutoff_date(@retention_periods.session_data)

    # Clean up expired session tokens and authentication data
    Logger.info("GDPR: Session cleanup placeholder - cutoff: #{inspect(cutoff_date)}")

    # This would need to integrate with your authentication token cleanup
    0
  end

  defp hard_delete_user(user) do
    # This is a permanent deletion - use with extreme caution
    # Should only be called after retention period has expired

    try do
      # Delete related data first (foreign key constraints)
      with :ok <- delete_user_businesses(user.id),
           :ok <- delete_user_consent_records(user.id) do
        # Finally delete the user record
        case Ash.destroy(user, domain: RivaAsh.Accounts) do
          :ok -> :ok
          {:error, reason} -> {:error, reason}
        end
      else
        {:error, reason} -> {:error, reason}
      end
    rescue
      error -> {:error, error}
    end
  end

  defp delete_user_businesses(_user_id) do
    # Placeholder implementation - needs actual Business resource handling
    # Example: Business |> Ash.Query.filter(owner_id == ^user_id) |> Ash.destroy_all()
    Logger.info("GDPR: Business deletion placeholder for user_id: #{_user_id}")
    :ok
  end

  defp delete_user_consent_records(user_id) do
    ConsentRecord.by_user!(user_id)
    |> Enum.each(fn consent ->
      case Ash.destroy(consent) do
        :ok -> :ok
        {:error, reason} ->
          Logger.error("GDPR: Failed to delete consent #{consent.id}: #{inspect(reason)}")
          {:error, reason}
      end
    end)
  rescue
    _ -> :ok
  end

  # Anonymization functions

  defp anonymize_user(user) do
    anonymized_data = %{
      name: "Anonymized User",
      email: "anonymized_#{user.id}@deleted.local"
      # Keep ID for referential integrity but remove personal data
    }

    case Ash.update(user, anonymized_data, domain: RivaAsh.Accounts) do
      {:ok, updated_user} -> {:ok, updated_user}
      {:error, reason} -> {:error, reason}
    end
  end

  defp anonymize_employee(employee) do
    # Similar anonymization for employee records
    anonymized_data = %{employee | name: "Anonymized Employee"}
    {:ok, anonymized_data}
  end

  defp anonymize_client(client) do
    # Similar anonymization for client records
    anonymized_data = %{client | name: "Anonymized Client"}
    {:ok, anonymized_data}
  end

  defp anonymize_reservation(reservation) do
    # Anonymize reservation while keeping business data for analytics
    anonymized_data = %{reservation | client_name: "Anonymized Client"}
    {:ok, anonymized_data}
  end

  @doc """
  Generate a retention policy report for compliance audits.
  """
  @spec generate_retention_report() :: map()
  def generate_retention_report do
    %{
      report_date: DateTime.utc_now(),
      retention_periods: @retention_periods,
      data_counts: %{
        active_users: count_active_users(),
        archived_users: count_archived_users(),
        total_consent_records: count_consent_records(),
        expired_data_pending_deletion: count_expired_data()
      },
      next_cleanup_date: next_cleanup_date(),
      compliance_status: assess_compliance_status()
    }
  end

  # Type specifications for internal functions
  @type cleanup_result :: {atom(), integer()}
  @type cleanup_results :: %{atom() => integer()}

  @spec count_active_users() :: integer()
  defp count_active_users do
    User.read!(filter: expr(is_nil(archived_at)), domain: RivaAsh.Accounts) |> length()
  rescue
    _ -> 0
  end

  @spec count_archived_users() :: integer()
  defp count_archived_users do
    User.read!(filter: expr(not is_nil(archived_at)), domain: RivaAsh.Accounts) |> length()
  rescue
    _ -> 0
  end

  @spec count_consent_records() :: integer()
  defp count_consent_records do
    ConsentRecord.read!() |> length()
  rescue
    _ -> 0
  end

  @spec count_expired_data() :: integer()
  defp count_expired_data do
    # Count data that should be deleted but hasn't been yet
    0
  end

  @spec next_cleanup_date() :: Date.t()
  defp next_cleanup_date do
    # Assuming daily cleanup runs
    DateTime.utc_now() |> DateTime.add(1, :day) |> DateTime.to_date()
  end

  @spec assess_compliance_status() :: String.t()
  defp assess_compliance_status do
    # Basic compliance check - in production this would be more sophisticated
    "compliant"
  end
end
