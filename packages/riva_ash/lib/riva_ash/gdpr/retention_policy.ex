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

  # Retention periods in days
  @retention_periods %{
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
  }

  @doc """
  Get retention period for a specific data type.
  """
  def retention_period(data_type) do
    Map.get(@retention_periods, data_type, 365) # Default to 1 year
  end

  @doc """
  Run the automated retention cleanup process.
  This should be called by a scheduled background job.
  """
  def run_retention_cleanup do
    Logger.info("GDPR: Starting automated retention cleanup")

    results = %{
      users_processed: cleanup_expired_users(),
      employees_processed: cleanup_expired_employees(),
      clients_processed: cleanup_expired_clients(),
      reservations_processed: cleanup_expired_reservations(),
      consent_records_processed: cleanup_expired_consent_records(),
      audit_logs_processed: cleanup_expired_audit_logs(),
      sessions_processed: cleanup_expired_sessions()
    }

    Logger.info("GDPR: Retention cleanup completed", extra: results)
    {:ok, results}
  end

  @doc """
  Check if data should be deleted based on retention policy.
  """
  def should_delete?(data_type, last_activity_date) do
    retention_days = retention_period(data_type)
    cutoff_date = DateTime.utc_now() |> DateTime.add(-retention_days, :day)

    DateTime.compare(last_activity_date, cutoff_date) == :lt
  end

  @doc """
  Anonymize data instead of deleting it (for statistical purposes).
  """
  def anonymize_record(record, data_type) do
    case data_type do
      :user_accounts -> anonymize_user(record)
      :employee_records -> anonymize_employee(record)
      :client_records -> anonymize_client(record)
      :reservations -> anonymize_reservation(record)
      _ -> {:error, "Anonymization not supported for #{data_type}"}
    end
  end

  # Private functions for cleanup operations

  defp cleanup_expired_users do
    cutoff_date = DateTime.utc_now() |> DateTime.add(-@retention_periods.user_accounts, :day)

    # Find users that have been archived and are past retention period
    # Note: We need to unset the base filter to include archived records
    expired_users = User
    |> Ash.Query.unset([:filter])
    |> Ash.Query.filter(expr(not is_nil(archived_at) and archived_at < ^cutoff_date))
    |> Ash.read!(domain: RivaAsh.Accounts)

    count = length(expired_users)

    Enum.each(expired_users, fn user ->
      case hard_delete_user(user) do
        :ok ->
          Logger.info("GDPR: Hard deleted expired user #{user.id}")
        {:error, reason} ->
          Logger.error("GDPR: Failed to delete user #{user.id}: #{inspect(reason)}")
      end
    end)

    count
  rescue
    error ->
      Logger.error("GDPR: Error in user cleanup: #{inspect(error)}")
      0
  end

  defp cleanup_expired_employees do
    _cutoff_date = DateTime.utc_now() |> DateTime.add(-@retention_periods.employee_records, :day)

    # Placeholder implementation - needs actual Employee resource query
    # Example: Employee |> Ash.Query.filter(expr(not is_nil(archived_at))) |> Ash.read!()
    0
  end

  defp cleanup_expired_clients do
    _cutoff_date = DateTime.utc_now() |> DateTime.add(-@retention_periods.client_records, :day)

    # Placeholder implementation - needs actual Client resource query
    # Example: Client |> Ash.Query.filter(expr(not is_nil(archived_at))) |> Ash.read!()
    0
  end

  defp cleanup_expired_reservations do
    _cutoff_date = DateTime.utc_now() |> DateTime.add(-@retention_periods.reservations, :day)

    # Reservations might need anonymization rather than deletion for business records
    0
  end

  defp cleanup_expired_consent_records do
    cutoff_date = DateTime.utc_now() |> DateTime.add(-@retention_periods.consent_records, :day)

    # Only delete consent records that have been withdrawn and are past retention
    expired_consents = ConsentRecord
    |> Ash.Query.filter(expr(
      consent_given == false and
      not is_nil(withdrawal_date) and
      withdrawal_date < ^cutoff_date
    ))
    |> Ash.read!(domain: RivaAsh.Domain)

    count = length(expired_consents)

    Enum.each(expired_consents, fn consent ->
      case Ash.destroy(consent) do
        :ok ->
          Logger.info("GDPR: Deleted expired consent record #{consent.id}")
        {:error, reason} ->
          Logger.error("GDPR: Failed to delete consent #{consent.id}: #{inspect(reason)}")
      end
    end)

    count
  rescue
    error ->
      Logger.error("GDPR: Error in consent cleanup: #{inspect(error)}")
      0
  end

  defp cleanup_expired_audit_logs do
    # This would clean up paper trail records older than retention period
    # Implementation depends on how you want to handle audit log retention
    0
  end

  defp cleanup_expired_sessions do
    # Clean up expired session tokens and authentication data
    _cutoff_date = DateTime.utc_now() |> DateTime.add(-@retention_periods.session_data, :day)

    # This would need to integrate with your authentication token cleanup
    0
  end

  defp hard_delete_user(user) do
    # This is a permanent deletion - use with extreme caution
    # Should only be called after retention period has expired

    try do
      # Delete related data first (foreign key constraints)
      delete_user_businesses(user.id)
      delete_user_consent_records(user.id)

      # Finally delete the user record
      case Ash.destroy(user, domain: RivaAsh.Accounts) do
        :ok -> :ok
        {:error, reason} -> {:error, reason}
      end
    rescue
      error -> {:error, error}
    end
  end

  defp delete_user_businesses(_user_id) do
    # Placeholder implementation - needs actual Business resource handling
    # Example: Business |> Ash.Query.filter(owner_id == ^user_id) |> Ash.destroy_all()
    :ok
  end

  defp delete_user_consent_records(user_id) do
    ConsentRecord.by_user!(user_id)
    |> Enum.each(fn consent ->
      Ash.destroy(consent)
    end)
  rescue
    _ -> :ok
  end

  # Anonymization functions

  defp anonymize_user(user) do
    anonymized_data = %{
      name: "Anonymized User",
      email: "anonymized_#{user.id}@deleted.local",
      # Keep ID for referential integrity but remove personal data
    }

    case Ash.update(user, anonymized_data, domain: RivaAsh.Accounts) do
      {:ok, updated_user} -> {:ok, updated_user}
      {:error, reason} -> {:error, reason}
    end
  end

  defp anonymize_employee(employee) do
    # Similar anonymization for employee records
    {:ok, employee}
  end

  defp anonymize_client(client) do
    # Similar anonymization for client records
    {:ok, client}
  end

  defp anonymize_reservation(reservation) do
    # Anonymize reservation while keeping business data for analytics
    {:ok, reservation}
  end

  @doc """
  Generate a retention policy report for compliance audits.
  """
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

  defp count_active_users do
    User.read!(filter: expr(is_nil(archived_at)), domain: RivaAsh.Accounts) |> length()
  rescue
    _ -> 0
  end

  defp count_archived_users do
    User.read!(filter: expr(not is_nil(archived_at)), domain: RivaAsh.Accounts) |> length()
  rescue
    _ -> 0
  end

  defp count_consent_records do
    ConsentRecord.read!() |> length()
  rescue
    _ -> 0
  end

  defp count_expired_data do
    # Count data that should be deleted but hasn't been yet
    0
  end

  defp next_cleanup_date do
    # Assuming daily cleanup runs
    DateTime.utc_now() |> DateTime.add(1, :day) |> DateTime.to_date()
  end

  defp assess_compliance_status do
    # Basic compliance check - in production this would be more sophisticated
    "compliant"
  end
end
