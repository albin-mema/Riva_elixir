defmodule RivaAsh.Layout.LayoutService do
  @moduledoc """
  Service module for managing Layouts business logic.
  This module encapsulates all business logic related to layouts,
  keeping it separate from the LiveView UI concerns.
  """

  require Logger

  alias RivaAsh.Repo
  alias RivaAsh.Resources.Layout
  alias RivaAsh.Resources.Business
  alias Ash.Query

  @doc """
  Get all layouts for the current user with pagination.
  """
  def get_user_layouts(user) do
    try do
      query = 
        Layout
        |> Query.filter(business_id: user.business_id)
        |> Query.sort(:name)
        |> Query.load([:business])

      case Ash.read(query, page: [limit: 20, offset: 0]) do
        {:ok, layouts} ->
          # Get total count for pagination
          count_query = 
            Layout
            |> Query.filter(business_id: user.business_id)
            |> Query.aggregate(:count, :id)

          case Ash.read(count_query) do
            {:ok, [%{count: total_count}]} ->
              meta = %{
                total_count: total_count,
                page_size: 20,
                current_page: 1,
                total_pages: ceil(total_count / 20)
              }
              {:ok, {layouts, meta}}
            {:error, reason} ->
              Logger.error("Failed to count layouts: #{inspect(reason)}")
              {:error, reason}
          end
        
        {:error, reason} ->
          Logger.error("Failed to get user layouts: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in get_user_layouts: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Create a new layout.
  """
  def create_layout(attrs, user) do
    try do
      case Ash.create(Layout, 
           attributes: Map.merge(attrs, %{
             business_id: user.business_id,
             status: :active
           }),
           authorize?: true) do
        {:ok, layout} ->
          {:ok, layout}
        {:error, reason} ->
          Logger.error("Failed to create layout: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in create_layout: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Update an existing layout.
  """
  def update_layout(id, attrs, user) do
    try do
      case Ash.get(Layout, id, authorize?: true) do
        {:ok, layout} ->
          if layout.business_id == user.business_id do
            case Ash.update(layout, attributes: attrs, authorize?: true) do
              {:ok, updated_layout} ->
                {:ok, updated_layout}
              {:error, reason} ->
                Logger.error("Failed to update layout: #{inspect(reason)}")
                {:error, reason}
            end
          else
            {:error, :forbidden}
          end
        
        {:error, reason} ->
          Logger.error("Failed to get layout for update: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in update_layout: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Delete a layout.
  """
  def delete_layout(id, user) do
    try do
      case Ash.get(Layout, id, authorize?: true) do
        {:ok, layout} ->
          if layout.business_id == user.business_id do
            case Ash.destroy(layout, authorize?: true) do
              {:ok, deleted_layout} ->
                {:ok, deleted_layout}
              {:error, reason} ->
                Logger.error("Failed to delete layout: #{inspect(reason)}")
                {:error, reason}
            end
          else
            {:error, :forbidden}
          end
        
        {:error, reason} ->
          Logger.error("Failed to get layout for deletion: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in delete_layout: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Activate a layout.
  """
  def activate_layout(id, user) do
    try do
      case Ash.get(Layout, id, authorize?: true) do
        {:ok, layout} ->
          if layout.business_id == user.business_id do
            case Ash.update(layout, attributes: %{status: :active}, authorize?: true) do
              {:ok, updated_layout} ->
                {:ok, updated_layout}
              {:error, reason} ->
                Logger.error("Failed to activate layout: #{inspect(reason)}")
                {:error, reason}
            end
          else
            {:error, :forbidden}
          end
        
        {:error, reason} ->
          Logger.error("Failed to get layout for activation: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in activate_layout: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Deactivate a layout.
  """
  def deactivate_layout(id, user) do
    try do
      case Ash.get(Layout, id, authorize?: true) do
        {:ok, layout} ->
          if layout.business_id == user.business_id do
            case Ash.update(layout, attributes: %{status: :inactive}, authorize?: true) do
              {:ok, updated_layout} ->
                {:ok, updated_layout}
              {:error, reason} ->
                Logger.error("Failed to deactivate layout: #{inspect(reason)}")
                {:error, reason}
            end
          else
            {:error, :forbidden}
          end
        
        {:error, reason} ->
          Logger.error("Failed to get layout for deactivation: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in deactivate_layout: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Get a single layout by ID.
  """
  def get_layout(id, user) do
    try do
      case Ash.get(Layout, id, authorize?: true) do
        {:ok, layout} ->
          if layout.business_id == user.business_id do
            {:ok, layout}
          else
            {:error, :forbidden}
          end
        
        {:error, reason} ->
          Logger.error("Failed to get layout: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in get_layout: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Validate layout attributes.
  """
  def validate_layout_attrs(attrs) do
    _errors = []

    # Validate name
    name_errors = 
      case attrs do
        %{name: name} when is_binary(name) and byte_size(name) > 0 ->
          []
        _ ->
          ["Name is required"]
      end

    # Validate business_id
    business_id_errors = 
      case attrs do
        %{business_id: business_id} when is_binary(business_id) and byte_size(business_id) > 0 ->
          []
        _ ->
          ["Business ID is required"]
      end

    # Validate width
    width_errors = 
      case attrs do
        %{width: width} when is_integer(width) and width > 0 ->
          []
        %{width: width} when is_integer(width) ->
          ["Width must be greater than 0"]
        _ ->
          ["Width is required and must be a positive integer"]
      end

    # Validate height
    height_errors = 
      case attrs do
        %{height: height} when is_integer(height) and height > 0 ->
          []
        %{height: height} when is_integer(height) ->
          ["Height must be greater than 0"]
        _ ->
          ["Height is required and must be a positive integer"]
      end

    # Validate description (optional but if provided, should be reasonable length)
    description_errors = 
      case attrs do
        %{description: description} when is_binary(description) and byte_size(description) <= 2000 ->
          []
        %{description: description} when is_binary(description) ->
          ["Description must be less than 2000 characters"]
        _ ->
          []
      end

    all_errors = name_errors ++ business_id_errors ++ width_errors ++ height_errors ++ description_errors

    case all_errors do
      [] -> {:ok, attrs}
      errors -> {:error, errors}
    end
  end

  @doc """
  Check if a layout name is already taken in the business.
  """
  def name_taken?(name, business_id) do
    try do
      query = 
        Layout
        |> Query.filter(business_id: business_id, name: name)
        |> Query.limit(1)

      case Ash.read(query) do
        {:ok, []} -> false
        {:ok, [_ | _]} -> true
        {:error, reason} ->
          Logger.error("Failed to check name uniqueness: #{inspect(reason)}")
          false
      end
    rescue
      error ->
        Logger.error("Unexpected error in name_taken?: #{inspect(error)}")
        false
    end
  end

  @doc """
  Get layout statistics for a business.
  """
  def get_layout_stats(business_id) do
    try do
      # Get total count
      total_count_query = 
        Layout
        |> Query.filter(business_id: business_id)
        |> Query.aggregate(:count, :id)

      # Get active count
      active_count_query = 
        Layout
        |> Query.filter(business_id: business_id, status: :active)
        |> Query.aggregate(:count, :id)

      # Get inactive count
      inactive_count_query = 
        Layout
        |> Query.filter(business_id: business_id, status: :inactive)
        |> Query.aggregate(:count, :id)

      # Get average dimensions
      avg_width_query = 
        Layout
        |> Query.filter(business_id: business_id)
        |> Query.aggregate(:avg, :width)

      avg_height_query = 
        Layout
        |> Query.filter(business_id: business_id)
        |> Query.aggregate(:avg, :height)

      with {:ok, [%{count: total_count}]} <- Ash.read(total_count_query),
           {:ok, [%{count: active_count}]} <- Ash.read(active_count_query),
           {:ok, [%{count: inactive_count}]} <- Ash.read(inactive_count_query),
           {:ok, [%{avg: avg_width}]} <- Ash.read(avg_width_query),
           {:ok, [%{avg: avg_height}]} <- Ash.read(avg_height_query) do
        
        stats = %{
          total_count: total_count,
          active_count: active_count,
          inactive_count: inactive_count,
          avg_width: round(avg_width || 0),
          avg_height: round(avg_height || 0)
        }
        
        {:ok, stats}
      else
        {:error, reason} ->
          Logger.error("Failed to get layout stats: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in get_layout_stats: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Check if layout dimensions are valid.
  """
  def valid_dimensions?(width, height) do
    is_integer(width) and is_integer(height) and width > 0 and height > 0
  end

  @doc """
  Check if layout dimensions exceed maximum allowed size.
  """
  def within_max_dimensions?(width, height) do
    max_width = Application.get_env(:riva_ash, :max_layout_width, 1000)
    max_height = Application.get_env(:riva_ash, :max_layout_height, 1000)
    
    width <= max_width and height <= max_height
  end

  @doc """
  Check if layout dimensions are within minimum allowed size.
  """
  def within_min_dimensions?(width, height) do
    min_width = Application.get_env(:riva_ash, :min_layout_width, 10)
    min_height = Application.get_env(:riva_ash, :min_layout_height, 10)
    
    width >= min_width and height >= min_height
  end
end