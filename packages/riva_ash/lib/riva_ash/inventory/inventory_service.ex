defmodule RivaAsh.Inventory.InventoryService do
  @moduledoc """
  Service for handling inventory management operations.
  Separates business logic from LiveView concerns.
  """

  alias RivaAsh.Resources.{Business, Item, ItemType}

  @doc """
  Gets businesses for a specific user.

  ## Parameters
  - user: The user to get businesses for

  ## Returns
  {:ok, [Business.t()]} | {:error, reason}
  """
  @spec get_user_businesses(User.t()) :: {:ok, [Business.t()]} | {:error, term()}
  def get_user_businesses(user) do
    Business.read!(actor: user)
  rescue
    error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
      {:error, error}
  end

  @doc """
  Gets items for a user's businesses.

  ## Parameters
  - user: The user to get items for
  - business_ids: List of business IDs to filter by

  ## Returns
  {:ok, [Item.t()]} | {:error, reason}
  """
  @spec get_user_items(User.t(), [String.t()]) :: {:ok, [Item.t()]} | {:error, term()}
  def get_user_items(user, business_ids) do
    Item.read!(
      actor: user,
      filter: [section: [plot: [business_id: [in: business_ids]]]]
    )
  rescue
    error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
      {:error, error}
  end

  @doc """
  Gets item types for a user's businesses.

  ## Parameters
  - user: The user to get item types for
  - business_ids: List of business IDs to filter by

  ## Returns
  {:ok, [ItemType.t()]} | {:error, reason}
  """
  @spec get_user_item_types(User.t(), [String.t()]) :: {:ok, [ItemType.t()]} | {:error, term()}
  def get_user_item_types(user, business_ids) do
    ItemType.read!(
      actor: user,
      filter: [business_id: [in: business_ids]]
    )
  rescue
    error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
      {:error, error}
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
  Counts items by status.

  ## Parameters
  - items: List of items to count
  - status: The status to count (:available, :occupied, :maintenance, :hold)

  ## Returns
  integer()
  """
  @spec count_items_by_status([Item.t()], atom()) :: integer()
  def count_items_by_status(items, status) do
    Enum.count(items, &(get_item_status(&1) == status))
  end

  # Private helper functions

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
end
