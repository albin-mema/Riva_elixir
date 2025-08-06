defmodule RivaAsh.ItemType.TypeService do
  @moduledoc """
  Service module for managing Item Types business logic.
  This module encapsulates all business logic related to item types,
  keeping it separate from the LiveView UI concerns.
  """

  require Logger

  alias RivaAsh.Repo
  alias RivaAsh.Resources.ItemType
  alias RivaAsh.Resources.Business
  alias Ash.Query

  @doc """
  Get all item types for the current user with pagination.
  """
  def get_user_types(user) do
    query =
      ItemType
      |> Query.filter(business_id: user.business_id)
      |> Query.sort(:name)
      |> Query.load([:business, :item_count])

    try do
      case Ash.read(query, page: [limit: 20, offset: 0]) do
        {:ok, item_types} ->
          # Get total count for pagination
          count_query =
            ItemType
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

              {:ok, {item_types, meta}}

            {:error, reason} ->
              Logger.error("Failed to count item types: #{inspect(reason)}")
              {:error, reason}
          end

        {:error, reason} ->
          Logger.error("Failed to get user item types: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in get_user_types: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Create a new item type.
  """
  def create_type(attrs, user) do
    try do
      case Ash.create(ItemType,
             attributes:
               Map.merge(attrs, %{
                 business_id: user.business_id,
                 status: :active
               }),
             authorize?: true
           ) do
        {:ok, item_type} ->
          {:ok, item_type}

        {:error, reason} ->
          Logger.error("Failed to create item type: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in create_type: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Update an existing item type.
  """
  def update_type(id, attrs, user) do
    try do
      case Ash.get(ItemType, id, authorize?: true) do
        {:ok, item_type} ->
          if item_type.business_id == user.business_id do
            case Ash.update(item_type, attributes: attrs, authorize?: true) do
              {:ok, updated_item_type} ->
                {:ok, updated_item_type}

              {:error, reason} ->
                Logger.error("Failed to update item type: #{inspect(reason)}")
                {:error, reason}
            end
          else
            {:error, :forbidden}
          end

        {:error, reason} ->
          Logger.error("Failed to get item type for update: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in update_type: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Delete an item type.
  """
  def delete_type(id, user) do
    try do
      case Ash.get(ItemType, id, authorize?: true) do
        {:ok, item_type} ->
          if item_type.business_id == user.business_id do
            case Ash.destroy(item_type, authorize?: true) do
              {:ok, deleted_item_type} ->
                {:ok, deleted_item_type}

              {:error, reason} ->
                Logger.error("Failed to delete item type: #{inspect(reason)}")
                {:error, reason}
            end
          else
            {:error, :forbidden}
          end

        {:error, reason} ->
          Logger.error("Failed to get item type for deletion: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in delete_type: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Activate an item type.
  """
  def activate_type(id, user) do
    try do
      case Ash.get(ItemType, id, authorize?: true) do
        {:ok, item_type} ->
          if item_type.business_id == user.business_id do
            case Ash.update(item_type, attributes: %{status: :active}, authorize?: true) do
              {:ok, updated_item_type} ->
                {:ok, updated_item_type}

              {:error, reason} ->
                Logger.error("Failed to activate item type: #{inspect(reason)}")
                {:error, reason}
            end
          else
            {:error, :forbidden}
          end

        {:error, reason} ->
          Logger.error("Failed to get item type for activation: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in activate_type: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Deactivate an item type.
  """
  def deactivate_type(id, user) do
    try do
      case Ash.get(ItemType, id, authorize?: true) do
        {:ok, item_type} ->
          if item_type.business_id == user.business_id do
            case Ash.update(item_type, attributes: %{status: :inactive}, authorize?: true) do
              {:ok, updated_item_type} ->
                {:ok, updated_item_type}

              {:error, reason} ->
                Logger.error("Failed to deactivate item type: #{inspect(reason)}")
                {:error, reason}
            end
          else
            {:error, :forbidden}
          end

        {:error, reason} ->
          Logger.error("Failed to get item type for deactivation: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in deactivate_type: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Get a single item type by ID.
  """
  def get_type(id, user) do
    try do
      case Ash.get(ItemType, id, authorize?: true) do
        {:ok, item_type} ->
          if item_type.business_id == user.business_id do
            {:ok, item_type}
          else
            {:error, :forbidden}
          end

        {:error, reason} ->
          Logger.error("Failed to get item type: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in get_type: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Validate item type attributes.
  """
  def validate_type_attrs(attrs) do
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

    all_errors = name_errors ++ business_id_errors ++ description_errors

    case all_errors do
      [] -> {:ok, attrs}
      errors -> {:error, errors}
    end
  end

  @doc """
  Check if an item type name is already taken in the business.
  """
  def name_taken?(name, business_id) do
    query =
      ItemType
      |> Query.filter(business_id: business_id, name: name)
      |> Query.limit(1)

    try do
      case Ash.read(query) do
        {:ok, []} ->
          false

        {:ok, [_ | _]} ->
          true

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
  Get item type statistics for a business.
  """
  def get_type_stats(business_id) do
    # Get total count
    total_count_query =
      ItemType
      |> Query.filter(business_id: business_id)
      |> Query.aggregate(:count, :id)

    # Get active count
    active_count_query =
      ItemType
      |> Query.filter(business_id: business_id, status: :active)
      |> Query.aggregate(:count, :id)

    # Get inactive count
    inactive_count_query =
      ItemType
      |> Query.filter(business_id: business_id, status: :inactive)
      |> Query.aggregate(:count, :id)

    try do
      with {:ok, [%{count: total_count}]} <- Ash.read(total_count_query),
           {:ok, [%{count: active_count}]} <- Ash.read(active_count_query),
           {:ok, [%{count: inactive_count}]} <- Ash.read(inactive_count_query) do
        stats = %{
          total_count: total_count,
          active_count: active_count,
          inactive_count: inactive_count
        }

        {:ok, stats}
      else
        {:error, reason} ->
          Logger.error("Failed to get type stats: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in get_type_stats: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end
end
