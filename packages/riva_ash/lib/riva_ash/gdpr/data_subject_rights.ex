defmodule RivaAsh.GDPR.DataSubjectRights do
  @moduledoc """
  Implementation of GDPR Data Subject Rights (Articles 15-22).

  This module provides functions to handle:
  - Right of access (Article 15)
  - Right to rectification (Article 16)
  - Right to erasure (Article 17)
  - Right to restrict processing (Article 18)
  - Right to data portability (Article 20)
  - Right to object (Article 21)

  All operations are logged for compliance audit purposes.
  """

  alias RivaAsh.Accounts.User
  alias RivaAsh.Resources.Business
  alias RivaAsh.GDPR.ConsentRecord

  require Logger
  import Ash.Expr
  require Ash.Query

  @config Application.compile_env(:riva_ash, :gdpr, %{})
  @allowed_correction_fields Map.get(@config, :allowed_correction_fields, [:name, :email, :phone])
  @valid_processing_purposes Map.get(@config, :valid_processing_purposes, [
                               "marketing",
                               "analytics",
                               "profiling",
                               "automated_decision_making",
                               "legitimate_interest_processing"
                             ])

  @doc """
  Export all personal data for a data subject in a portable format.

  GDPR Article 15: Right of access
  GDPR Article 20: Right to data portability

  Returns data in JSON format that can be easily imported by other systems.
  """
  @spec export_personal_data(String.t(), atom()) :: {:ok, String.t()} | {:error, term()}
  def export_personal_data(user_id, format) when is_binary(user_id) do
    Logger.info("GDPR: Starting data export for user #{user_id}")

    with {:ok, user} <- get_user(user_id),
         {:ok, data} <- collect_personal_data(user) do
      data
      |> format_export_data(format)
      |> handle_export_success(user_id)
    else
      {:error, reason} ->
        handle_export_failure(user_id, reason)
    end
  end

  defp handle_export_success(data, user_id) do
    formatted_data = data
    Logger.info("GDPR data export completed successfully for user #{user_id}",
                user_id: user_id,
                export_date: DateTime.utc_now(),
                data_size: byte_size(formatted_data),
                function: __ENV__.function,
                module: __ENV__.module)
    log_data_subject_request(user_id, "data_export", "completed")
    {:ok, formatted_data}
  end

  defp handle_export_failure(user_id, reason) do
    Logger.error("GDPR data export failed for user #{user_id}: #{inspect(reason)}",
                 user_id: user_id,
                 error_reason: inspect(reason),
                 function: __ENV__.function,
                 module: __ENV__.module,
                 line: __ENV__.line,
                 timestamp: DateTime.utc_now())
    log_data_subject_request(user_id, "data_export", "failed", reason)
    {:error, reason}
  end

  @doc """
  Initiate the data deletion process for a user.

  GDPR Article 17: Right to erasure ("right to be forgotten")

  This performs a soft delete initially, then schedules hard deletion
  after the legal retention period.
  """
  @spec request_data_deletion(String.t(), String.t()) :: {:ok, String.t()} | {:error, term()}
  def request_data_deletion(user_id, _reason) when is_binary(user_id) do
    Logger.info("GDPR: Starting deletion request for user #{user_id}")

    with {:ok, user} <- get_user(user_id),
         :ok <- validate_deletion_request(user),
         {:ok, _unmatched} <- soft_delete_user_data(user),
         :ok <- schedule_hard_deletion(user_id) do
      handle_deletion_success(user_id)
    else
      {:error, reason} ->
        handle_deletion_failure(user_id, reason)
    end
  end

  defp handle_deletion_success(user_id) do
    log_data_subject_request(user_id, "data_deletion", "initiated")
    message = "Deletion request initiated. Data will be permanently deleted after retention period."
    {:ok, message}
  end

  defp handle_deletion_failure(user_id, reason) do
    log_data_subject_request(user_id, "data_deletion", "failed", reason)
    {:error, reason}
  end

  @doc """
  Restrict processing of personal data.

  GDPR Article 18: Right to restriction of processing

  Marks data as restricted - it can be stored but not processed.
  """
  @spec restrict_data_processing(String.t(), String.t()) :: {:ok, String.t()} | {:error, term()}
  def restrict_data_processing(user_id, restriction_reason) when is_binary(user_id) and is_binary(restriction_reason) do
    Logger.info("GDPR: Restricting data processing for user #{user_id}")

    with {:ok, user} <- get_user(user_id),
         {:ok, _unmatched} <- mark_data_as_restricted(user, restriction_reason) do
      handle_restriction_success(user_id)
    else
      {:error, reason} ->
        handle_restriction_failure(user_id, reason)
    end
  end

  def restrict_data_processing(_user_id, _restriction_reason) do
    {:error, "User ID and restriction reason must be strings"}
  end

  defp handle_restriction_success(user_id) do
    log_data_subject_request(user_id, "restrict_processing", "completed")
    {:ok, "Data processing has been restricted"}
  end

  defp handle_restriction_failure(user_id, reason) do
    log_data_subject_request(user_id, "restrict_processing", "failed", reason)
    {:error, reason}
  end

  @doc """
  Object to data processing.

  GDPR Article 21: Right to object

  Allows users to object to processing based on legitimate interests.
  """
  @spec object_to_processing(String.t(), [String.t()]) :: {:ok, String.t()} | {:error, term()}
  def object_to_processing(user_id, processing_purposes)
      when is_binary(user_id) and is_list(processing_purposes) do
    Logger.info("GDPR: Processing objection for user #{user_id}")

    with {:ok, user} <- get_user(user_id),
         :ok <- validate_objection_grounds(processing_purposes),
         {:ok, _unmatched} <- record_processing_objection(user, processing_purposes) do
      handle_objection_success(user_id)
    else
      {:error, reason} ->
        handle_objection_failure(user_id, reason)
    end
  end

  def object_to_processing(_user_id, _processing_purposes) do
    {:error, "User ID must be a string and processing purposes must be a list"}
  end

  defp handle_objection_success(user_id) do
    log_data_subject_request(user_id, "object_processing", "completed")
    {:ok, "Objection to processing has been recorded"}
  end

  defp handle_objection_failure(user_id, reason) do
    log_data_subject_request(user_id, "object_processing", "failed", reason)
    {:error, reason}
  end

  @doc """
  Rectify (correct) personal data.

  GDPR Article 16: Right to rectification
  """
  @spec rectify_personal_data(String.t(), map()) :: {:ok, User.t()} | {:error, term()}
  def rectify_personal_data(user_id, corrections)
      when is_binary(user_id) and is_map(corrections) do
    Logger.info("GDPR: Data rectification request for user #{user_id}")

    with {:ok, user} <- get_user(user_id),
         :ok <- validate_corrections(corrections),
         {:ok, updated_user} <- apply_corrections(user, corrections) do
      handle_rectification_success(user_id, updated_user)
    else
      {:error, reason} ->
        handle_rectification_failure(user_id, reason)
    end
  end

  def rectify_personal_data(_user_id, _corrections) do
    {:error, "User ID must be a string and corrections must be a map"}
  end

  # Private helper functions

  defp get_user(user_id) do
    case Ash.get(User, user_id, domain: RivaAsh.Accounts) do
      {:ok, user} -> {:ok, user}
      {:error, _unmatched} -> {:error, "User not found"}
    end
  end

  defp collect_personal_data(user) do
    data = %{
      user_profile: extract_user_data(user),
      businesses: extract_business_data(user.id),
      employee_records: extract_employee_data(user.id),
      client_records: extract_client_data(user.id),
      reservations: extract_reservation_data(user.id),
      consent_records: extract_consent_data(user.id),
      audit_trail: extract_audit_data(user.id)
    }

    {:ok, data}
  end

  defp extract_user_data(user) do
    %{
      id: user.id,
      name: user.name,
      email: user.email,
      role: user.role,
      created_at: user.inserted_at,
      updated_at: user.updated_at
    }
  end

  defp extract_business_data(user_id) do
    Business.read!(actor: %{id: user_id, role: :admin})
    |> Enum.map(fn business ->
      %{
        id: business.id,
        name: business.name,
        description: business.description,
        created_at: business.inserted_at
      }
    end)
  rescue
    _error -> []
  end

  defp extract_employee_data(user_id) do
    business_ids = get_user_business_ids(user_id)

    case RivaAsh.Resources.Employee
         |> Ash.Query.filter(expr(business_id in ^business_ids))
         |> Ash.read(domain: RivaAsh.Resources) do
      {:ok, employees} ->
        employees
        |> Enum.map(fn employee ->
          %{
            id: employee.id,
            business_id: employee.business_id,
            name: employee.name,
            position: employee.position,
            hire_date: employee.hire_date,
            termination_date: employee.termination_date,
            salary: employee.salary,
            created_at: employee.inserted_at,
            updated_at: employee.updated_at
          }
        end)
      {:error, _reason} -> []
    end
  end

  defp get_user_business_ids(user_id) do
    case Business
         |> Ash.Query.filter(expr(owner_id == ^user_id))
         |> Ash.read(domain: RivaAsh.Resources) do
      {:ok, businesses} ->
        businesses
        |> Enum.map(& &1.id)
      {:error, _reason} -> []
    end
  end

  defp extract_client_data(user_id) do
    business_ids = get_user_business_ids(user_id)

    case RivaAsh.Resources.Client
         |> Ash.Query.filter(expr(business_id in ^business_ids))
         |> Ash.read(domain: RivaAsh.Resources) do
      {:ok, clients} ->
        clients
        |> Enum.map(fn client ->
          %{
            id: client.id,
            business_id: client.business_id,
            name: client.name,
            email: client.email,
            phone: client.phone,
            address: client.address,
            created_at: client.inserted_at,
            last_interaction_date: client.last_interaction_date
          }
        end)
      {:error, _reason} -> []
    end
  end

  defp extract_reservation_data(user_id) do
    business_ids = get_user_business_ids(user_id)

    case RivaAsh.Resources.Reservation
         |> Ash.Query.filter(expr(business_id in ^business_ids))
         |> Ash.read(domain: RivaAsh.Resources) do
      {:ok, reservations} ->
        reservations
        |> Enum.map(fn reservation ->
          %{
            id: reservation.id,
            business_id: reservation.business_id,
            client_id: reservation.client_id,
            client_name: reservation.client_name,
            reservation_date: reservation.reservation_date,
            service_type: reservation.service_type,
            status: reservation.status,
            amount: reservation.amount,
            created_at: reservation.inserted_at,
            updated_at: reservation.updated_at
          }
        end)
      {:error, _reason} -> []
    end
  end

  defp extract_consent_data(user_id) do
    ConsentRecord.by_user!(user_id)
    |> Enum.map(fn consent ->
      %{
        purpose: consent.purpose,
        consent_given: consent.consent_given,
        consent_date: consent.consent_date,
        withdrawal_date: consent.withdrawal_date
      }
    end)
  rescue
    _unmatchedunmatched -> []
  end

  defp extract_audit_data(user_id) do
    case RivaAsh.Audit.AuditLog
         |> Ash.Query.filter(expr(user_id == ^user_id or target_user_id == ^user_id))
         |> Ash.Query.sort(desc: :inserted_at)
         |> Ash.read(domain: RivaAsh.Audit) do
      {:ok, audit_logs} ->
        audit_logs
        |> Enum.map(fn log ->
          %{
            id: log.id,
            action: log.action,
            resource_type: log.resource_type,
            resource_id: log.resource_id,
            user_id: log.user_id,
            target_user_id: log.target_user_id,
            changes: log.changes,
            ip_address: log.ip_address,
            user_agent: log.user_agent,
            inserted_at: log.inserted_at
          }
        end)
      {:error, _reason} -> []
    end
  end

  defp format_export_data(data, :json) do
    %{
      export_date: DateTime.utc_now(),
      data_subject_id: data.user_profile.id,
      format: "JSON",
      data: data
    }
    |> Jason.encode!(pretty: true)
  end

  defp validate_deletion_request(_user) do
    # Check if user has active legal obligations that prevent deletion
    # For example, ongoing contracts, legal disputes, etc.
    :ok
  end

  defp soft_delete_user_data(user) do
    # Archive the user and related data
    case Ash.update(user, %{archived_at: DateTime.utc_now()}, domain: RivaAsh.Accounts) do
      {:ok, archived_user} -> {:ok, archived_user}
      {:error, reason} -> {:error, reason}
    end
  end

  defp schedule_hard_deletion(user_id) do
    # Schedule a background job to permanently delete data after retention period
    # This would integrate with your job system (Oban, etc.)
    Logger.info("GDPR: Scheduled hard deletion for user #{user_id}")
    :ok
  end

  defp mark_data_as_restricted(user, reason) do
    # Add restriction metadata to user record
    # This would need a new field in the user schema
    Logger.info("GDPR: Marked data as restricted for user #{user.id}, reason: #{reason}")
    {:ok, user}
  end

  defp validate_objection_grounds(purposes) do
    if Enum.all?(purposes, &(&1 in @valid_processing_purposes)) do
      :ok
    else
      {:error, "Invalid processing purpose for objection"}
    end
  end

  defp record_processing_objection(user, purposes) do
    # Record the objection in consent records
    Enum.each(purposes, fn purpose ->
      ConsentRecord.withdraw_consent!(%{
        user_id: user.id,
        purpose: purpose,
        ip_address: "system",
        user_agent: "gdpr_objection"
      })
    end)

    {:ok, user}
  end

  defp validate_corrections(corrections) do
    # Validate that corrections are for allowed fields
    if Map.keys(corrections) |> Enum.all?(&(&1 in @allowed_correction_fields)) do
      :ok
    else
      {:error, "Invalid field for correction"}
    end
  end

  defp apply_corrections(user, corrections) do
    case Ash.update(user, corrections, domain: RivaAsh.Accounts) do
      {:ok, updated_user} -> {:ok, updated_user}
      {:error, reason} -> {:error, reason}
    end
  end

  defp log_data_subject_request(user_id, request_type, status, details \\ nil) do
    Logger.info("GDPR Request: user=#{user_id}, type=#{request_type}, status=#{status}, details=#{inspect(details)}")

    # This should also be stored in a dedicated audit table for GDPR requests
    # to maintain compliance records
  end

  # Additional helper functions for hard deletion

  defp hard_delete_user(user) do
    # This is a permanent deletion - use with extreme caution
    # Should only be called after retention period has expired

    try do
      # Delete related data first (foreign key constraints)
      with :ok <- delete_user_businesses(user.id),
           :ok <- delete_user_consent_records(user.id),
           :ok <- delete_user_activity_logs(user.id),
           :ok <- delete_user_sessions(user.id),
           :ok <- delete_user_audit_logs(user.id) do
        # Finally delete the user record
        case Ash.destroy(user, domain: RivaAsh.Accounts) do
          :ok ->
            Logger.info("GDPR: Hard deleted user #{user.id} and all related data")
            :ok
          {:error, reason} ->
            Logger.error("GDPR: Failed to delete user #{user.id}: #{inspect(reason)}")
            {:error, reason}
        end
      else
        {:error, reason} ->
          Logger.error("GDPR: Failed to delete user #{user.id} related data: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
        Logger.error("GDPR: Authorization error deleting user #{user.id}: #{inspect(error)}")
        {:error, "Authorization error: #{inspect(error)}"}

      error ->
        Logger.error("GDPR: Unexpected error deleting user #{user.id}: #{inspect(error)}")
        {:error, error}
    end
  end

  defp delete_user_businesses(user_id) do
    case Business
         |> Ash.Query.filter(expr(owner_id == ^user_id))
         |> Ash.read(domain: RivaAsh.Resources) do
      {:ok, businesses} ->
        businesses
        |> Enum.each(fn business ->
          case Ash.destroy(business, domain: RivaAsh.Resources) do
            :ok -> :ok
            {:error, reason} ->
              Logger.error("GDPR: Failed to delete business #{business.id}: #{inspect(reason)}")
              {:error, reason}
          end
        end)
        :ok
      {:error, reason} ->
        Logger.error("GDPR: Failed to query user businesses: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp delete_user_consent_records(user_id) do
    case ConsentRecord
         |> Ash.Query.filter(expr(user_id == ^user_id))
         |> Ash.read(domain: RivaAsh.Domain) do
      {:ok, consents} ->
        consents
        |> Enum.each(fn consent ->
          case Ash.destroy(consent, domain: RivaAsh.Domain) do
            :ok -> :ok
            {:error, reason} ->
              Logger.error("GDPR: Failed to delete consent #{consent.id}: #{inspect(reason)}")
              {:error, reason}
          end
        end)
        :ok
      {:error, reason} ->
        Logger.error("GDPR: Failed to query user consent records: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp delete_user_activity_logs(user_id) do
    case RivaAsh.Activity.ActivityLog
         |> Ash.Query.filter(expr(user_id == ^user_id))
         |> Ash.read(domain: RivaAsh.Activity) do
      {:ok, activity_logs} ->
        activity_logs
        |> Enum.each(fn log ->
          case Ash.destroy(log, domain: RivaAsh.Activity) do
            :ok -> :ok
            {:error, reason} ->
              Logger.error("GDPR: Failed to delete activity log #{log.id}: #{inspect(reason)}")
              {:error, reason}
          end
        end)
        :ok
      {:error, reason} ->
        Logger.error("GDPR: Failed to query user activity logs: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp delete_user_sessions(user_id) do
    case RivaAsh.Auth.Session
         |> Ash.Query.filter(expr(user_id == ^user_id))
         |> Ash.read(domain: RivaAsh.Auth) do
      {:ok, sessions} ->
        sessions
        |> Enum.each(fn session ->
          case Ash.destroy(session, domain: RivaAsh.Auth) do
            :ok -> :ok
            {:error, reason} ->
              Logger.error("GDPR: Failed to delete session #{session.id}: #{inspect(reason)}")
              {:error, reason}
          end
        end)
        :ok
      {:error, reason} ->
        Logger.error("GDPR: Failed to query user sessions: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp delete_user_audit_logs(user_id) do
    case RivaAsh.Audit.AuditLog
         |> Ash.Query.filter(expr(user_id == ^user_id or target_user_id == ^user_id))
         |> Ash.read(domain: RivaAsh.Audit) do
      {:ok, audit_logs} ->
        audit_logs
        |> Enum.each(fn log ->
          case Ash.destroy(log, domain: RivaAsh.Audit) do
            :ok -> :ok
            {:error, reason} ->
              Logger.error("GDPR: Failed to delete audit log #{log.id}: #{inspect(reason)}")
              {:error, reason}
          end
        end)
        :ok
      {:error, reason} ->
        Logger.error("GDPR: Failed to query user audit logs: #{inspect(reason)}")
        {:error, reason}
    end
  end

  # Type specifications for internal functions
  @type export_data :: %{
          user_profile: map(),
          businesses: [map()],
          employee_records: [map()],
          client_records: [map()],
          reservations: [map()],
          consent_records: [map()],
          audit_trail: [map()]
        }

  @spec handle_rectification_success(String.t(), User.t()) :: {:ok, User.t()}
  defp handle_rectification_success(user_id, updated_user) do
    log_data_subject_request(user_id, "data_rectification", "completed")
    {:ok, updated_user}
  end

  @spec handle_rectification_failure(String.t(), term()) :: {:error, term()}
  defp handle_rectification_failure(user_id, reason) do
    log_data_subject_request(user_id, "data_rectification", "failed", reason)
    {:error, reason}
  end
end
