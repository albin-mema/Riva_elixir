defmodule RivaAsh.Item.ItemService do
  @moduledoc """
  Service for handling item operations.
  Separates business logic from LiveView concerns.
  """

  alias RivaAsh.Resources.{Item, Business}

  @doc """
  Gets all items for a specific user.

  ## Parameters
  - user: The user to get items for

  ## Returns
  {:ok, {[Item.t()], meta}} | {:error, reason}
  """
  @spec get_user_items(User.t()) :: {:ok, {[Item.t()], map()}} | {:error, term()}
  def get_user_items(user) do
    with {:ok, businesses} <- get_user_businesses(user),
         business_ids <- Enum.map(businesses, & &1.id),
         {:ok, items} <- get_items_for_businesses(user, business_ids) do
      meta = %{
        total: length(items),
        page: 1,
        page_size: length(items)
      }

      {:ok, {items, meta}}
    end
  end

  @doc """
  Gets a specific item by ID for a user.

  ## Parameters
  - item_id: ID of the item to retrieve
  - user: The user requesting the item

  ## Returns
  {:ok, Item.t()} | {:error, reason}
  """
  @spec get_item(String.t(), User.t()) :: {:ok, Item.t()} | {:error, term()}
  def get_item(item_id, user) do
    try do
      case Item.read!(id: item_id, actor: user) do
        [item] -> {:ok, item}
        [] -> {:error, :not_found}
        _ -> {:error, :multiple_found}
      end
    rescue
      error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
        {:error, error}
    end
  end

  @doc """
  Creates a new item.

  ## Parameters
  - item_params: Map of item parameters
  - user: The user creating the item

  ## Returns
  {:ok, Item.t()} | {:error, reason}
  """
  @spec create_item(map(), User.t()) :: {:ok, Item.t()} | {:error, term()}
  def create_item(item_params, user) do
    with :ok <- validate_item_params(item_params),
         {:ok, item} <- Item.create!(item_params, actor: user),
         do: {:ok, item}
  end

  @doc """
  Updates an existing item.

  ## Parameters
  - item_id: ID of the item to update
  - update_params: Map of update parameters
  - user: The user performing the update

  ## Returns
  {:ok, Item.t()} | {:error, reason}
  """
  @spec update_item(String.t(), map(), User.t()) :: {:ok, Item.t()} | {:error, term()}
  def update_item(item_id, update_params, user) do
    with {:ok, item} <- get_item(item_id, user),
         :ok <- validate_update_params(item, update_params),
         {:ok, updated_item} <- Item.update!(item, update_params, actor: user),
         do: {:ok, updated_item}
  end

  @doc """
  Deletes an item.

  ## Parameters
  - item_id: ID of the item to delete
  - user: The user performing the deletion

  ## Returns
  {:ok, Item.t()} | {:error, reason}
  """
  @spec delete_item(String.t(), User.t()) :: {:ok, Item.t()} | {:error, term()}
  def delete_item(item_id, user) do
    with {:ok, item} <- get_item(item_id, user),
         :ok <- validate_deletion(item),
         {:ok, deleted_item} <- Item.destroy!(item, actor: user),
         do: {:ok, deleted_item}
  end

  @doc """
  Gets the current status of an item.

  ## Parameters
  - item: The item to check status for

  ## Returns
  :available | :occupied | :maintenance | :hold
  """
  @spec get_item_status(Item.t()) :: atom()
  def get_item_status(item) do
    # This would check current reservations, holds, maintenance schedules, etc.
    # For now, we'll implement a basic version that could be extended

    case check_maintenance_status(item) do
      :maintenance -> :maintenance
      _available -> check_reservation_status(item)
    end
  end

  @doc """
  Searches for items based on criteria.

  ## Parameters
  - search_params: Map containing search criteria
  - user: The user performing the search

  ## Returns
  {:ok, {[Item.t()], meta}} | {:error, reason}
  """
  @spec search_items(map(), User.t()) :: {:ok, {[Item.t()], map()}} | {:error, term()}
  def search_items(search_params, user) do
    with {:ok, businesses} <- get_user_businesses(user),
         business_ids <- Enum.map(businesses, & &1.id),
         {:ok, items} <- search_items_for_businesses(search_params, user, business_ids) do
      meta = %{
        total: length(items),
        page: 1,
        page_size: length(items)
      }

      {:ok, {items, meta}}
    end
  end

  # Private helper functions

  defp get_user_businesses(user) do
    Business.read!(actor: user)
  rescue
    error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
      {:error, error}
  end

  defp get_items_for_businesses(user, business_ids) do
    Item.read!(
      actor: user,
      filter: [section: [plot: [business_id: [in: business_ids]]]],
      sort: [name: :asc]
    )
  rescue
    error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
      {:error, error}
  end

  defp search_items_for_businesses(search_params, user, business_ids) do
    base_query =
      Item
      |> Query.filter(section: [plot: [business_id: [in: business_ids]]])
      |> Query.sort(name: :asc)

    query =
      case Map.get(search_params, :search_term) do
        nil ->
          base_query

        term ->
          base_query
          |> Query.or([
            Query.filter(name: [contains: term]),
            Query.filter(description: [contains: term])
          ])
      end

    Ash.read!(query, actor: user)
  rescue
    error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
      {:error, error}
  end

  defp validate_item_params(params) do
    required_fields = [:name, :business_id, :item_type_id]

    missing_fields = required_fields |> Enum.filter(&(!Map.has_key?(params, &1)))

    case missing_fields do
      [] -> :ok
      _ -> {:error, {:missing_fields, missing_fields}}
    end
  end

  defp validate_update_params(_item, update_params) do
    # Don't allow changing business or item type as it would affect relationships
    restricted_fields = [:business_id, :item_type_id]

    case Enum.any?(restricted_fields, &Map.has_key?(update_params, &1)) do
      true -> {:error, :cannot_change_restricted_fields}
      false -> :ok
    end
  end

  defp validate_deletion(item) do
    # Check if item has active reservations or holds
    case has_active_reservations_or_holds(item) do
      true -> {:error, :cannot_delete_item_with_active_bookings}
      false -> :ok
    end
  end

  defp check_maintenance_status(_item) do
    # Check if item is in maintenance
    # This would query maintenance schedules, etc.
    :available
  end

  defp check_reservation_status(_item) do
    # Check if item has active reservations
    # This would query reservations, holds, etc.
    :available
  end

  defp has_active_reservations_or_holds(_item) do
    # This would check for active reservations and holds
    # For now, return false as a placeholder
    false
  end
end
