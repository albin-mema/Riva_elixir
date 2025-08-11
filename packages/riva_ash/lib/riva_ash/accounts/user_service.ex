defmodule RivaAsh.Accounts.UserService do
  @moduledoc """
  User service for managing user operations.

  This module provides functionality for:
  - User management
  - User statistics
  - User analytics
  - User preferences
  - User settings
  """

  require Logger

  @type user :: map()
  @type user_params :: map()
  @type stats :: map()
  @type result :: {:ok, any()} | {:error, String.t()}

  @doc """
  Gets system statistics.
  
  ## Returns
    - {:ok, stats} - System statistics
    - {:error, reason} - Failed to get statistics
  """
  @spec get_system_stats() :: {:ok, stats()} | {:error, String.t()}
  def get_system_stats do
    Logger.info("Getting system statistics")

    try do
      stats = %{
        total_users: count_total_users(),
        active_users: count_active_users(),
        new_users_this_month: count_new_users_this_month(),
        user_growth_rate: calculate_user_growth_rate(),
        user_activity_stats: get_user_activity_stats(),
        user_demographics: get_user_demographics(),
        system_performance: get_system_performance_metrics(),
        generated_at: DateTime.utc_now()
      }

      {:ok, stats}
    rescue
      error ->
        Logger.error("Failed to get system stats: #{inspect(error)}", 
                    function: __ENV__.function,
                    module: __ENV__.module,
                    line: __ENV__.line)
        {:error, "Failed to retrieve system statistics"}
    end
  end

  @doc """
  Gets user statistics for a specific user.

  ## Parameters
    - user_id: ID of the user
    - date_range: Date range for statistics (optional)

  ## Returns
    - {:ok, stats} - User statistics
    - {:error, reason} - Failed to get user statistics
  """
  @spec get_user_stats(String.t(), map() | nil) :: {:ok, stats()} | {:error, String.t()}
  def get_user_stats(user_id, date_range \\ nil) when is_binary(user_id) do
    Logger.info("Getting user stats for user: #{user_id}")

    try do
      with {:ok, user} <- get_user(user_id),
           {:ok, start_date, end_date} <- parse_date_range(date_range),
           stats <- calculate_user_statistics(user, start_date, end_date) do
        {:ok, stats}
      else
        {:error, reason} -> {:error, reason}
      end
    rescue
      error ->
        Logger.error("Failed to get user stats for user #{user_id}: #{inspect(error)}", 
                    function: __ENV__.function,
                    module: __ENV__.module,
                    line: __ENV__.line,
                    user_id: user_id)
        {:error, "Failed to retrieve user statistics"}
    end
  end

  @doc """
  Gets user preferences.

  ## Parameters
    - user_id: ID of the user

  ## Returns
    - {:ok, preferences} - User preferences
    - {:error, reason} - Failed to get preferences
  """
  @spec get_user_preferences(String.t()) :: {:ok, map()} | {:error, String.t()}
  def get_user_preferences(user_id) when is_binary(user_id) do
    Logger.debug("Getting user preferences for user: #{user_id}")

    try do
      with {:ok, user} <- get_user(user_id),
           preferences <- extract_user_preferences(user) do
        {:ok, preferences}
      else
        {:error, reason} -> {:error, reason}
      end
    rescue
      error ->
        Logger.error("Error getting user preferences: #{inspect(error)}")
        {:error, "Failed to retrieve user preferences"}
    end
  end

  @doc """
  Updates user preferences.

  ## Parameters
    - user_id: ID of the user
    - preferences: New preferences to set

  ## Returns
    - {:ok, user} - Updated user
    - {:error, reason} - Failed to update preferences
  """
  @spec update_user_preferences(String.t(), map()) :: {:ok, user()} | {:error, String.t()}
  def update_user_preferences(user_id, preferences) when is_binary(user_id) do
    Logger.info("Updating user preferences for user: #{user_id}")

    try do
      with {:ok, user} <- get_user(user_id),
           :ok <- validate_preferences(preferences),
           updated_user <- apply_user_preferences(user, preferences) do
        {:ok, updated_user}
      else
        {:error, reason} -> {:error, reason}
      end
    rescue
      error ->
        Logger.error("Error updating user preferences: #{inspect(error)}")
        {:error, "Failed to update user preferences"}
    end
  end

  @doc """
  Gets user activity history.

  ## Parameters
    - user_id: ID of the user
    - opts: Options for filtering and pagination

  ## Returns
    - {:ok, activities} - List of user activities
    - {:error, reason} - Failed to get activity history
  """
  @spec get_user_activity(String.t(), keyword()) :: {:ok, [map()]} | {:error, String.t()}
  def get_user_activity(user_id, opts \\ []) when is_binary(user_id) do
    Logger.debug("Getting user activity for user: #{user_id}")

    try do
      limit = Keyword.get(opts, :limit, 50)
      offset = Keyword.get(opts, :offset, 0)
      activity_type = Keyword.get(opts, :activity_type, nil)

      activities = query_user_activities(user_id, limit, offset, activity_type)

      {:ok, activities}
    rescue
      error ->
        Logger.error("Error getting user activity: #{inspect(error)}")
        {:error, "Failed to retrieve user activity"}
    end
  end

  @doc """
  Validates user parameters.

  ## Parameters
    - params: Parameters to validate

  ## Returns
    - {:ok, validated_params} - Validated parameters
    - {:error, reason} - Validation failed
  """
  @spec validate_user_params(map()) :: {:ok, map()} | {:error, String.t()}
  def validate_user_params(params) do
    Logger.debug("Validating user parameters")

    required_fields = [:user_id]

    case validate_required_fields(params, required_fields) do
      :ok ->
        with :ok <- validate_user_id(Map.get(params, :user_id)) do
          {:ok, params}
        else
          {:error, reason} -> {:error, reason}
        end

      {:error, reason} -> {:error, reason}
    end
  end

  # Private helper functions

  defp validate_required_fields(params, required_fields) do
    missing_fields = Enum.filter(required_fields, &(!Map.has_key?(params, &1)))

    case length(missing_fields) do
      0 -> :ok
      _ -> {:error, "Missing required fields: #{inspect(missing_fields)}"}
    end
  end

  defp validate_user_id(user_id) when is_binary(user_id) do
    if String.length(user_id) > 0 do
      :ok
    else
      {:error, "Invalid user ID"}
    end
  end

  defp validate_user_id(_), do: {:error, "User ID must be a string"}

  defp parse_date_range(nil), do: {:ok, nil, nil}

  defp parse_date_range(%{start: start_date, end: end_date}) when is_struct(start_date, Date) and is_struct(end_date, Date) do
    {:ok, start_date, end_date}
  end

  defp parse_date_range(%{"start" => start_string, "end" => end_string}) do
    with {:ok, start_date} <- Date.from_iso8601(start_string),
         {:ok, end_date} <- Date.from_iso8601(end_string) do
      {:ok, start_date, end_date}
    else
      {:error, _} -> {:error, "Invalid date format. Use YYYY-MM-DD"}
    end
  end

  defp parse_date_range(_), do: {:error, "Invalid date range format"}

  defp get_user(user_id) do
    case Ash.get(User, user_id, domain: RivaAsh.Accounts) do
      {:ok, user} -> {:ok, user}
      {:error, _reason} -> {:error, "User not found"}
    end
  end

  defp count_total_users do
    User
    |> Ash.read(domain: RivaAsh.Accounts)
    |> case do
      {:ok, users} -> length(users)
      {:error, _reason} -> 0
    end
  end

  defp count_active_users do
    User
    |> Ash.Query.filter(expr(is_nil(archived_at)))
    |> Ash.read(domain: RivaAsh.Accounts)
    |> case do
      {:ok, users} -> length(users)
      {:error, _reason} -> 0
    end
  end

  defp count_new_users_this_month do
    now = DateTime.utc_now()
    start_of_month = DateTime.new!(now.year, now.month, 1, 0, 0, 0)

    User
    |> Ash.Query.filter(expr(inserted_at >= ^start_of_month))
    |> Ash.read(domain: RivaAsh.Accounts)
    |> case do
      {:ok, users} -> length(users)
      {:error, _reason} -> 0
    end
  end

  defp calculate_user_growth_rate do
    now = DateTime.utc_now()
    start_of_month = DateTime.new!(now.year, now.month, 1, 0, 0, 0)
    start_of_previous_month = DateTime.add(start_of_month, -1, :month)

    with {:ok, current_month_users} <-
           User
           |> Ash.Query.filter(expr(inserted_at >= ^start_of_month))
           |> Ash.read(domain: RivaAsh.Accounts),
         {:ok, previous_month_users} <-
           User
           |> Ash.Query.filter(expr(inserted_at >= ^start_of_previous_month and inserted_at < ^start_of_month))
           |> Ash.read(domain: RivaAsh.Accounts) do
      current_count = length(current_month_users)
      previous_count = length(previous_month_users)

      if previous_count > 0 do
        :math.log(current_count / previous_count) * 100
      else
        if current_count > 0, do: 100.0, else: 0.0
      end
    else
      _ -> 0.0
    end
  end

  defp get_user_activity_stats do
    now = DateTime.utc_now()
    thirty_days_ago = DateTime.add(now, -30, :day)

    with {:ok, recent_users} <-
           User
           |> Ash.Query.filter(expr(last_login >= ^thirty_days_ago))
           |> Ash.read(domain: RivaAsh.Accounts),
         {:ok, total_sessions} <-
           # Assuming there's a Session resource that tracks user sessions
           RivaAsh.Auth.Session
           |> Ash.Query.filter(expr(created_at >= ^thirty_days_ago))
           |> Ash.read(domain: RivaAsh.Auth),
         {:ok, total_actions} <-
           # Assuming there's an ActivityLog resource for user actions
           RivaAsh.Activity.ActivityLog
           |> Ash.Query.filter(expr(created_at >= ^thirty_days_ago))
           |> Ash.read(domain: RivaAsh.Activity) do
      %{
        active_users_30d: length(recent_users),
        total_sessions_30d: length(total_sessions),
        total_actions_30d: length(total_actions),
        avg_actions_per_user: if(length(recent_users) > 0, do: length(total_actions) / length(recent_users), else: 0)
      }
    else
      _ -> %{}
    end
  end

  defp get_user_demographics do
    with {:ok, users} <- Ash.read(User, domain: RivaAsh.Accounts) do
      total_users = length(users)

      # Role distribution
      role_counts = users
      |> Enum.group_by(&Map.get(&1, :role, "user"))
      |> Enum.map(fn {role, user_list} -> {role, length(user_list)} end)
      |> Map.new()

      # Registration trend by month
      registration_by_month = users
      |> Enum.group_by(fn user ->
        case user.inserted_at do
          %DateTime{year: year, month: month} -> {year, month}
          %NaiveDateTime{year: year, month: month} -> {year, month}
          _ -> nil
        end
      end)
      |> Enum.reject(fn {key, _} -> is_nil(key) end)
      |> Enum.map(fn {{year, month}, user_list} -> {{year, month}, length(user_list)} end)
      |> Map.new()

      %{
        total_users: total_users,
        role_distribution: role_counts,
        registration_by_month: registration_by_month,
        generated_at: DateTime.utc_now()
      }
    else
      _ -> %{}
    end
  end

  defp get_system_performance_metrics do
    now = DateTime.utc_now()
    thirty_days_ago = DateTime.add(now, -30, :day)
    
    # Get database performance metrics
    db_metrics = %{
      total_queries: get_db_query_count(thirty_days_ago),
      avg_query_time: get_avg_query_time(thirty_days_ago),
      slow_queries: get_slow_query_count(thirty_days_ago)
    }
    
    # Get system resource metrics
    resource_metrics = %{
      active_users: count_active_users(),
      total_sessions: get_total_sessions(thirty_days_ago),
      avg_session_duration: get_avg_session_duration(thirty_days_ago),
      error_rate: get_error_rate(thirty_days_ago)
    }
    
    %{
      database: db_metrics,
      resources: resource_metrics,
      generated_at: DateTime.utc_now()
    }
  end
  
  defp get_db_query_count(_since_date) do
    # This would query database query logs
    0
  end
  
  defp get_avg_query_time(_since_date) do
    # This would calculate average query execution time
    0.0
  end
  
  defp get_slow_query_count(_since_date) do
    # This would count slow queries
    0
  end
  
  defp get_total_sessions(_since_date) do
    case RivaAsh.Auth.Session
         |> Ash.Query.filter(expr(created_at >= ^_since_date))
         |> Ash.read(domain: RivaAsh.Auth) do
      {:ok, sessions} -> length(sessions)
      {:error, _reason} -> 0
    end
  end
  
  defp get_avg_session_duration(_since_date) do
    case RivaAsh.Auth.Session
         |> Ash.Query.filter(expr(created_at >= ^_since_date))
         |> Ash.read(domain: RivaAsh.Auth) do
      {:ok, sessions} ->
        total_duration = sessions
        |> Enum.map(fn session ->
          case session do
            %{created_at: created, last_accessed: last} when not is_nil(last) ->
              DateTime.diff(last, created)
            %{created_at: created, last_accessed: nil} ->
              DateTime.diff(DateTime.utc_now(), created)
            _ -> 0
          end
        end)
        |> Enum.sum()
        
        if length(sessions) > 0 do
          total_duration / length(sessions)
        else
          0
        end
      {:error, _reason} -> 0
    end
  end
  
  defp get_error_rate(_since_date) do
    # This would calculate error rate from logs
    0.0
  end

  defp calculate_user_statistics(user, start_date, end_date) do
    %{
      user_id: user.id,
      period: %{start: start_date, end: end_date},
      total_logins: count_user_logins(user.id, start_date, end_date),
      total_actions: count_user_actions(user.id, start_date, end_date),
      session_duration: calculate_average_session_duration(user.id, start_date, end_date),
      most_used_features: get_most_used_features(user.id, start_date, end_date),
      last_activity: get_last_user_activity(user.id, start_date, end_date),
      generated_at: DateTime.utc_now()
    }
  end

  defp count_user_logins(user_id, start_date, end_date) do
    case RivaAsh.Auth.Session
         |> Ash.Query.filter(expr(user_id == ^user_id and created_at >= ^start_date and created_at <= ^end_date))
         |> Ash.read(domain: RivaAsh.Auth) do
      {:ok, sessions} -> length(sessions)
      {:error, _reason} -> 0
    end
  end

  defp count_user_actions(user_id, start_date, end_date) do
    case RivaAsh.Activity.ActivityLog
         |> Ash.Query.filter(expr(user_id == ^user_id and created_at >= ^start_date and created_at <= ^end_date))
         |> Ash.read(domain: RivaAsh.Activity) do
      {:ok, actions} -> length(actions)
      {:error, _reason} -> 0
    end
  end

  defp calculate_average_session_duration(user_id, start_date, end_date) do
    case RivaAsh.Auth.Session
         |> Ash.Query.filter(expr(user_id == ^user_id and created_at >= ^start_date and created_at <= ^end_date))
         |> Ash.read(domain: RivaAsh.Auth) do
      {:ok, sessions} ->
        total_duration = sessions
        |> Enum.map(fn session ->
          case session do
            %{created_at: created, last_accessed: last} when not is_nil(last) ->
              DateTime.diff(last, created)
            %{created_at: created, last_accessed: nil} ->
              DateTime.diff(DateTime.utc_now(), created)
            _ -> 0
          end
        end)
        |> Enum.sum()
        
        if length(sessions) > 0 do
          total_duration / length(sessions)
        else
          0
        end
      {:error, _reason} -> 0
    end
  end

  defp get_most_used_features(user_id, start_date, end_date) do
    case RivaAsh.Activity.ActivityLog
         |> Ash.Query.filter(expr(user_id == ^user_id and created_at >= ^start_date and created_at <= ^end_date))
         |> Ash.read(domain: RivaAsh.Activity) do
      {:ok, actions} ->
        actions
        |> Enum.group_by(&Map.get(&1, :action_type))
        |> Enum.map(fn {action_type, action_list} -> {action_type, length(action_list)} end)
        |> Enum.sort_by(fn {_action_type, count} -> -count end)
        |> Enum.take(5)
        |> Enum.map(fn {action_type, _count} -> action_type end)
      {:error, _reason} -> []
    end
  end

  defp get_last_user_activity(user_id, _start_date, _end_date) do
    case RivaAsh.Activity.ActivityLog
         |> Ash.Query.filter(expr(user_id == ^user_id))
         |> Ash.Query.sort(desc: :created_at)
         |> Ash.read(domain: RivaAsh.Activity, limit: 1) do
      {:ok, [activity | _]} -> activity
      {:ok, []} -> nil
      {:error, _reason} -> nil
    end
  end

  defp extract_user_preferences(user) do
    case Map.get(user, :preferences) do
      nil -> %{}
      preferences when is_map(preferences) -> preferences
      _ -> %{}
    end
  end

  defp validate_preferences(preferences) do
    valid_keys = [:theme, :language, :notifications, :privacy, :marketing]
    invalid_keys = Map.keys(preferences) -- valid_keys
    
    case length(invalid_keys) do
      0 -> :ok
      _ -> {:error, "Invalid preference keys: #{inspect(invalid_keys)}"}
    end
  end

  defp apply_user_preferences(user, preferences) do
    case Ash.update(user, %{preferences: preferences, updated_at: DateTime.utc_now()}, domain: RivaAsh.Accounts) do
      {:ok, updated_user} -> updated_user
      {:error, _reason} -> user
    end
  end

  defp query_user_activities(user_id, limit, offset, activity_type) do
    query = RivaAsh.Activity.ActivityLog
            |> Ash.Query.filter(expr(user_id == ^user_id))
            |> Ash.Query.sort(desc: :created_at)
            |> Ash.Query.limit(limit)
            |> Ash.Query.offset(offset)
    
    query = if activity_type do
      Ash.Query.filter(query, expr(action_type == ^activity_type))
    else
      query
    end
    
    case Ash.read(query, domain: RivaAsh.Activity) do
      {:ok, activities} -> activities
      {:error, _reason} -> []
    end
  end
end
