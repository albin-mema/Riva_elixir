defmodule RivaAsh.Schedule.ScheduleService do
  @moduledoc """
  Service for handling item schedule operations.
  Separates business logic from LiveView concerns.
  """
  
  alias RivaAsh.Resources.ItemSchedule
  alias Ash.Query

  @doc """
  Gets all item schedules for a specific user.
  
  ## Parameters
  - user: The user to get schedules for
  
  ## Returns
  {:ok, {[ItemSchedule.t()], meta}} | {:error, reason}
  """
  @spec get_user_schedules(User.t()) :: {:ok, {[ItemSchedule.t()], map()}} | {:error, term()}
  def get_user_schedules(user) do
    try do
      case ItemSchedule.read!(actor: user, sort: [start_time: :desc]) do
        item_schedules ->
          meta = %{
            total: length(item_schedules),
            page: 1,
            page_size: length(item_schedules)
          }
          {:ok, {item_schedules, meta}}
      end
    rescue
      error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
        {:error, error}
    end
  end

  @doc """
  Gets a specific item schedule by ID for a user.
  
  ## Parameters
  - schedule_id: ID of the schedule to retrieve
  - user: The user requesting the schedule
  
  ## Returns
  {:ok, ItemSchedule.t()} | {:error, reason}
  """
  @spec get_schedule(String.t(), User.t()) :: {:ok, ItemSchedule.t()} | {:error, term()}
  def get_schedule(schedule_id, user) do
    try do
      case ItemSchedule.read!(id: schedule_id, actor: user) do
        [schedule] -> {:ok, schedule}
        [] -> {:error, :not_found}
        _ -> {:error, :multiple_found}
      end
    rescue
      error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
        {:error, error}
    end
  end

  @doc """
  Creates a new item schedule.
  
  ## Parameters
  - schedule_params: Map of schedule parameters
  - user: The user creating the schedule
  
  ## Returns
  {:ok, ItemSchedule.t()} | {:error, reason}
  """
  @spec create_schedule(map(), User.t()) :: {:ok, ItemSchedule.t()} | {:error, term()}
  def create_schedule(schedule_params, user) do
    with :ok <- validate_schedule_params(schedule_params),
         {:ok, schedule} <- ItemSchedule.create!(schedule_params, actor: user) do
      {:ok, schedule}
    end
  end

  @doc """
  Updates an existing item schedule.
  
  ## Parameters
  - schedule_id: ID of the schedule to update
  - update_params: Map of update parameters
  - user: The user performing the update
  
  ## Returns
  {:ok, ItemSchedule.t()} | {:error, reason}
  """
  @spec update_schedule(String.t(), map(), User.t()) :: {:ok, ItemSchedule.t()} | {:error, term()}
  def update_schedule(schedule_id, update_params, user) do
    with {:ok, schedule} <- get_schedule(schedule_id, user),
         :ok <- validate_update_params(schedule, update_params),
         {:ok, updated_schedule} <- ItemSchedule.update!(schedule, update_params, actor: user) do
      {:ok, updated_schedule}
    end
  end

  @doc """
  Deletes an item schedule.
  
  ## Parameters
  - schedule_id: ID of the schedule to delete
  - user: The user performing the deletion
  
  ## Returns
  {:ok, ItemSchedule.t()} | {:error, reason}
  """
  @spec delete_schedule(String.t(), User.t()) :: {:ok, ItemSchedule.t()} | {:error, term()}
  def delete_schedule(schedule_id, user) do
    with {:ok, schedule} <- get_schedule(schedule_id, user),
         :ok <- validate_deletion(schedule),
         {:ok, deleted_schedule} <- ItemSchedule.destroy!(schedule, actor: user) do
      {:ok, deleted_schedule}
    end
  end

  @doc """
  Activates an item schedule.
  
  ## Parameters
  - schedule_id: ID of the schedule to activate
  - user: The user performing the activation
  
  ## Returns
  {:ok, ItemSchedule.t()} | {:error, reason}
  """
  @spec activate_schedule(String.t(), User.t()) :: {:ok, ItemSchedule.t()} | {:error, term()}
  def activate_schedule(schedule_id, user) do
    with {:ok, schedule} <- get_schedule(schedule_id, user),
         :ok <- validate_activation(schedule),
         {:ok, updated_schedule} <- update_schedule_status(schedule, :active, user) do
      {:ok, updated_schedule}
    end
  end

  @doc """
  Deactivates an item schedule.
  
  ## Parameters
  - schedule_id: ID of the schedule to deactivate
  - user: The user performing the deactivation
  
  ## Returns
  {:ok, ItemSchedule.t()} | {:error, reason}
  """
  @spec deactivate_schedule(String.t(), User.t()) :: {:ok, ItemSchedule.t()} | {:error, term()}
  def deactivate_schedule(schedule_id, user) do
    with {:ok, schedule} <- get_schedule(schedule_id, user),
         :ok <- validate_deactivation(schedule),
         {:ok, updated_schedule} <- update_schedule_status(schedule, :inactive, user) do
      {:ok, updated_schedule}
    end
  end

  @doc """
  Checks for schedule conflicts.
  
  ## Parameters
  - schedule_id: ID of the schedule to check (can be nil for new schedules)
  - item_id: Item ID to check within
  - start_time: Start time of the schedule
  - end_time: End time of the schedule
  - day_of_week: Day of week (0-6, where 0 is Sunday)
  - user: The user performing the check
  
  ## Returns
  {:ok, boolean()} | {:error, reason}
  """
  @spec check_schedule_conflict(String.t() | nil, String.t(), DateTime.t(), DateTime.t() | nil, integer() | nil, User.t()) :: {:ok, boolean()} | {:error, term()}
  def check_schedule_conflict(schedule_id, item_id, start_time, end_time, day_of_week, user) do
    # This would check for overlapping schedules for the same item
    # For now, we'll implement a basic placeholder
    {:ok, false}
  end

  @doc """
  Validates if schedule times are logical.
  
  ## Parameters
  - start_time: Start time of the schedule
  - end_time: End time of the schedule
  
  ## Returns
  {:ok, boolean()} | {:error, reason}
  """
  @spec validate_schedule_times(DateTime.t(), DateTime.t() | nil) :: {:ok, boolean()} | {:error, term()}
  def validate_schedule_times(start_time, end_time) do
    case end_time do
      nil -> {:ok, true} # Open-ended schedule
      _end_time ->
        case DateTime.compare(start_time, end_time) do
          :lt -> {:ok, true} # Start time is before end time
          _ -> {:error, :invalid_time_range}
        end
    end
  end

  # Private helper functions

  defp validate_schedule_params(params) do
    required_fields = [:item_id, :start_time]
    
    missing_fields = required_fields |> Enum.filter(&(!Map.has_key?(params, &1)))
    
    case missing_fields do
      [] -> 
        # Validate time range
        case validate_schedule_times(params.start_time, params.end_time) do
          {:ok, _} -> :ok
          {:error, reason} -> {:error, reason}
        end
      _ -> {:error, {:missing_fields, missing_fields}}
    end
  end

  defp validate_update_params(schedule, update_params) do
    # Don't allow changing item as it would affect relationships
    case Map.has_key?(update_params, :item_id) do
      true -> {:error, :cannot_change_item}
      false -> :ok
    end
  end

  defp validate_deletion(schedule) do
    # Check if schedule has active bookings or reservations
    case has_active_associations(schedule) do
      true -> {:error, :cannot_delete_schedule_with_active_associations}
      false -> :ok
    end
  end

  defp validate_activation(schedule) do
    case schedule.status do
      :inactive -> :ok
      :active -> {:error, :already_active}
      :expired -> {:error, :cannot_activate_expired_schedule}
      _ -> {:error, :cannot_activate_schedule}
    end
  end

  defp validate_deactivation(schedule) do
    case schedule.status do
      :active -> :ok
      :inactive -> {:error, :already_inactive}
      :expired -> {:error, :cannot_deactivate_expired_schedule}
      _ -> {:error, :cannot_deactivate_schedule}
    end
  end

  defp update_schedule_status(schedule, status, user) do
    update_params = %{status: status, updated_at: DateTime.utc_now()}
    ItemSchedule.update!(schedule, update_params, actor: user)
  rescue
    error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
      {:error, error}
  end

  defp has_active_associations(schedule) do
    # This would check for active bookings, reservations, etc. associated with this schedule
    # For now, return false as a placeholder
    false
  end
end