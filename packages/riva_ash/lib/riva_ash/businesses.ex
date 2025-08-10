defmodule RivaAsh.Businesses do
  @moduledoc """
  Business management and operations module.
  
  This module provides functionality for:
  - Business management
  - Business operations
  - Business analytics
  - Business settings
  - Business configuration
  """

  require Logger

  @type business :: map()
  @type business_params :: map()
  @type result :: {:ok, any()} | {:error, String.t()}

  @doc """
  Creates a new business.
  
  ## Parameters
    - business_params: Parameters for creating the business
    - owner_id: ID of the business owner
  
  ## Returns
    - {:ok, business} - Successfully created business
    - {:error, reason} - Creation failed
  """
  @spec create_business(business_params(), String.t()) :: {:ok, business()} | {:error, String.t()}
  def create_business(business_params, owner_id) when is_binary(owner_id) do
    Logger.info("Creating business for owner: #{owner_id}")
    
    with {:ok, validated_params} <- validate_business_params(business_params),
         business_data <- prepare_business_data(validated_params, owner_id),
         {:ok, business} <- save_business(business_data) do
      {:ok, business}
    else
      {:error, reason} -> {:error, format_error(reason)}
    end
  end

  @doc """
  Updates an existing business.
  
  ## Parameters
    - business_id: ID of the business to update
    - update_params: Parameters for updating the business
    - actor: The user performing the update
  
  ## Returns
    - {:ok, business} - Successfully updated business
    - {:error, reason} - Update failed
  """
  @spec update_business(String.t(), business_params(), map()) :: {:ok, business()} | {:error, String.t()}
  def update_business(business_id, update_params, actor) when is_binary(business_id) do
    Logger.info("Updating business: #{business_id}")
    
    with {:ok, business} <- get_business(business_id),
         :ok <- validate_business_ownership(business, actor),
         :ok <- validate_business_update(business, update_params),
         updated_business <- merge_business_data(business, update_params),
         {:ok, saved_business} <- save_business(updated_business) do
      {:ok, saved_business}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Gets business by ID.
  
  ## Parameters
    - business_id: ID of the business to retrieve
  
  ## Returns
    - {:ok, business} - Successfully found business
    - {:error, reason} - Business not found or error
  """
  @spec get_business(String.t()) :: {:ok, business()} | {:error, String.t()}
  def get_business(business_id) when is_binary(business_id) do
    Logger.debug("Getting business: #{business_id}")
    
    # This would typically fetch the business from the database
    # For now, return empty business as stub implementation
    {:ok, %{
      id: business_id,
      name: "Sample Business",
      description: "A sample business",
      business_type: "restaurant",
      owner_id: "owner-123",
      status: "active",
      settings: %{},
      created_at: DateTime.utc_now(),
      updated_at: DateTime.utc_now()
    }}
  end

  @doc """
  Gets businesses for a user.
  
  ## Parameters
    - user_id: ID of the user
    - opts: Options for filtering and pagination
  
  ## Returns
    - {:ok, businesses} - List of businesses
    - {:error, reason} - Failed to get businesses
  """
  @spec get_user_businesses(String.t(), keyword()) :: {:ok, [business()]} | {:error, String.t()}
  def get_user_businesses(user_id, opts \\ []) when is_binary(user_id) do
    Logger.debug("Getting businesses for user: #{user_id}")
    
    try do
      filters = Keyword.get(opts, :filters, %{})
      limit = Keyword.get(opts, :limit, 20)
      offset = Keyword.get(opts, :offset, 0)
      
      businesses = query_user_businesses(user_id, filters, limit, offset)
      
      {:ok, businesses}
    rescue
      error ->
        Logger.error("Error getting user businesses: #{inspect(error)}")
        {:error, "Failed to retrieve user businesses"}
    end
  end

  @doc """
  Gets business statistics.
  
  ## Parameters
    - business_id: ID of the business
    - date_range: Date range for statistics (optional)
  
  ## Returns
    - {:ok, stats} - Business statistics
    - {:error, reason} - Failed to get statistics
  """
  @spec get_business_stats(String.t(), map() | nil) :: {:ok, map()} | {:error, String.t()}
  def get_business_stats(business_id, date_range \\ nil) when is_binary(business_id) do
    Logger.debug("Getting business stats for business: #{business_id}")
    
    try do
      with {:ok, business} <- get_business(business_id),
           {:ok, start_date, end_date} <- parse_date_range(date_range),
           stats <- calculate_business_statistics(business, start_date, end_date) do
        {:ok, stats}
      else
        {:error, reason} -> {:error, reason}
      end
    rescue
      error ->
        Logger.error("Error getting business stats: #{inspect(error)}")
        {:error, "Failed to retrieve business statistics"}
    end
  end

  @doc """
  Validates business parameters.
  
  ## Parameters
    - params: Parameters to validate
  
  ## Returns
    - {:ok, validated_params} - Validated parameters
    - {:error, reason} - Validation failed
  """
  @spec validate_business_params(map()) :: {:ok, map()} | {:error, String.t()}
  def validate_business_params(params) do
    Logger.debug("Validating business parameters")
    
    required_fields = [:name, :business_type, :owner_id]
    
    case validate_required_fields(params, required_fields) do
      :ok ->
        with :ok <- validate_business_name(params),
             :ok <- validate_business_type(params),
             :ok <- validate_business_category(params) do
          {:ok, params}
        else
          {:error, reason} -> {:error, reason}
        end
      
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Updates business settings.
  
  ## Parameters
    - business_id: ID of the business
    - settings: New settings to apply
    - actor: The user performing the update
  
  ## Returns
    - {:ok, business} - Updated business
    - {:error, reason} - Failed to update settings
  """
  @spec update_business_settings(String.t(), map(), map()) :: {:ok, business()} | {:error, String.t()}
  def update_business_settings(business_id, settings, actor) when is_binary(business_id) do
    Logger.info("Updating business settings for business: #{business_id}")
    
    try do
      with {:ok, business} <- get_business(business_id),
           :ok <- validate_business_ownership(business, actor),
           :ok <- validate_settings(settings),
           updated_business <- apply_business_settings(business, settings) do
        {:ok, updated_business}
      else
        {:error, reason} -> {:error, reason}
      end
    rescue
      error ->
        Logger.error("Error updating business settings: #{inspect(error)}")
        {:error, "Failed to update business settings"}
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

  defp validate_business_name(params) do
    case Map.get(params, :name) do
      name when is_binary(name) and byte_size(name) > 2 -> :ok
      _ -> {:error, "Business name must be at least 3 characters long"}
    end
  end

  defp validate_business_type(params) do
    case Map.get(params, :business_type) do
      type when type in ["restaurant", "hotel", "spa", "fitness", "retail", "service", "other"] -> :ok
      _ -> {:error, "Invalid business type"}
    end
  end

  defp validate_business_category(params) do
    case Map.get(params, :category) do
      category when is_binary(category) and byte_size(category) > 0 -> :ok
      nil -> :ok
      _ -> {:error, "Invalid business category"}
    end
  end

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

  defp prepare_business_data(params, owner_id) do
    %{
      name: Map.get(params, :name),
      description: Map.get(params, :description, ""),
      business_type: Map.get(params, :business_type),
      category: Map.get(params, :category),
      email: Map.get(params, :email),
      phone: Map.get(params, :phone),
      address: Map.get(params, :address, %{}),
      owner_id: owner_id,
      status: "active",
      settings: Map.get(params, :settings, %{}),
      created_at: DateTime.utc_now(),
      updated_at: DateTime.utc_now()
    }
  end

  defp save_business(business_data) do
    # This would typically save the business to the database
    # For now, return the business data as stub implementation
    {:ok, business_data}
  end

  defp validate_business_ownership(business, actor) do
    if business.owner_id == actor.id do
      :ok
    else
      {:error, "You don't have permission to modify this business"}
    end
  end

  defp validate_business_update(_business, _update_params) do
    # This would typically validate business update
    # For now, return :ok as stub implementation
    :ok
  end

  defp merge_business_data(business, update_params) do
    %{
      business
      | name: Map.get(update_params, :name, business.name),
        description: Map.get(update_params, :description, business.description),
        business_type: Map.get(update_params, :business_type, business.business_type),
        category: Map.get(update_params, :category, business.category),
        email: Map.get(update_params, :email, business.email),
        phone: Map.get(update_params, :phone, business.phone),
        address: Map.get(update_params, :address, business.address),
        settings: Map.get(update_params, :settings, business.settings),
        updated_at: DateTime.utc_now()
    }
  end

  defp calculate_business_statistics(business, start_date, end_date) do
    %{
      business_id: business.id,
      business_name: business.name,
      period: %{start: start_date, end: end_date},
      total_revenue: calculate_total_revenue(business, start_date, end_date),
      total_reservations: calculate_total_reservations(business, start_date, end_date),
      average_reservation_value: calculate_average_reservation_value(business, start_date, end_date),
      customer_satisfaction: calculate_customer_satisfaction(business, start_date, end_date),
      popular_items: get_popular_items(business, start_date, end_date),
      monthly_trends: get_monthly_trends(business, start_date, end_date),
      generated_at: DateTime.utc_now()
    }
  end

  defp calculate_total_revenue(_business, _start_date, _end_date) do
    # This would typically calculate total revenue
    # For now, return 0.0 as stub implementation
    0.0
  end

  defp calculate_total_reservations(_business, _start_date, _end_date) do
    # This would typically calculate total reservations
    # For now, return 0 as stub implementation
    0
  end

  defp calculate_average_reservation_value(_business, _start_date, _end_date) do
    # This would typically calculate average reservation value
    # For now, return 0.0 as stub implementation
    0.0
  end

  defp calculate_customer_satisfaction(_business, _start_date, _end_date) do
    # This would typically calculate customer satisfaction
    # For now, return 0.0 as stub implementation
    0.0
  end

  defp get_popular_items(_business, _start_date, _end_date) do
    # This would typically get popular items
    # For now, return empty list as stub implementation
    []
  end

  defp get_monthly_trends(_business, _start_date, _end_date) do
    # This would typically get monthly trends
    # For now, return empty map as stub implementation
    %{}
  end

  defp validate_settings(_settings) do
    # This would typically validate settings
    # For now, return :ok as stub implementation
    :ok
  end

  defp apply_business_settings(business, settings) do
    # This would typically apply business settings
    # For now, return the business as stub implementation
    %{business | settings: settings, updated_at: DateTime.utc_now()}
  end

  defp query_user_businesses(_user_id, _filters, _limit, _offset) do
    # This would typically query user businesses
    # For now, return empty list as stub implementation
    []
  end

  defp format_error(error) do
    case error do
      %{errors: errors} when is_list(errors) ->
        errors
        |> Enum.map(&format_error_message/1)
        |> Enum.join(", ")
      
      error when is_binary(error) ->
        error
      
      _ ->
        "An unknown error occurred"
    end
  end

  defp format_error_message({field, {message, _opts}}) do
    "#{field} #{message}"
  end

  defp format_error_message(%{message: message, field: field}) when is_atom(field) do
    "#{field} #{message}"
  end

  defp format_error_message(%{message: message}) do
    message
  end

  defp format_error_message(error) do
    inspect(error)
  end
end