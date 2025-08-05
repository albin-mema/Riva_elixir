defmodule RivaAsh.Hold.HoldService do
  @moduledoc """
  Service for handling item hold operations.
  Separates business logic from LiveView concerns.
  """
  
  alias RivaAsh.Resources.ItemHold
  alias Ash.Query

  @doc """
  Gets all holds for a specific user.
  
  ## Parameters
  - user: The user to get holds for
  
  ## Returns
  {:ok, {[ItemHold.t()], meta}} | {:error, reason}
  """
  @spec get_user_holds(User.t()) :: {:ok, {[ItemHold.t()], map()}} | {:error, term()}
  def get_user_holds(user) do
    try do
      case ItemHold.read!(actor: user, sort: [start_time: :desc]) do
        item_holds ->
          meta = %{
            total: length(item_holds),
            page: 1,
            page_size: length(item_holds)
          }
          {:ok, {item_holds, meta}}
      end
    rescue
      error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
        {:error, error}
    end
  end

  @doc """
  Cancels an existing item hold.
  
  ## Parameters
  - hold_id: ID of the hold to cancel
  - user: The user performing the cancellation
  
  ## Returns
  {:ok, ItemHold.t()} | {:error, reason}
  """
  @spec cancel_hold(String.t(), User.t()) :: {:ok, ItemHold.t()} | {:error, term()}
  def cancel_hold(hold_id, user) do
    with {:ok, item_hold} <- get_hold_for_user(hold_id, user),
         :ok <- validate_cancellation(item_hold),
         {:ok, updated_hold} <- update_hold_status(item_hold, :cancelled, user) do
      {:ok, updated_hold}
    end
  end

  @doc """
  Deletes an item hold.
  
  ## Parameters
  - hold_id: ID of the hold to delete
  - user: The user performing the deletion
  
  ## Returns
  {:ok, ItemHold.t()} | {:error, reason}
  """
  @spec delete_hold(String.t(), User.t()) :: {:ok, ItemHold.t()} | {:error, term()}
  def delete_hold(hold_id, user) do
    with {:ok, item_hold} <- get_hold_for_user(hold_id, user),
         :ok <- validate_deletion(item_hold),
         {:ok, deleted_hold} <- ItemHold.destroy!(item_hold, actor: user) do
      {:ok, deleted_hold}
    end
  end

  @doc """
  Creates a new item hold.
  
  ## Parameters
  - hold_params: Map of hold parameters
  - user: The user creating the hold
  
  ## Returns
  {:ok, ItemHold.t()} | {:error, reason}
  """
  @spec create_hold(map(), User.t()) :: {:ok, ItemHold.t()} | {:error, term()}
  def create_hold(hold_params, user) do
    with :ok <- validate_hold_params(hold_params),
         {:ok, item_hold} <- ItemHold.create!(hold_params, actor: user) do
      {:ok, item_hold}
    end
  end

  @doc """
  Updates an existing item hold.
  
  ## Parameters
  - hold_id: ID of the hold to update
  - update_params: Map of update parameters
  - user: The user performing the update
  
  ## Returns
  {:ok, ItemHold.t()} | {:error, reason}
  """
  @spec update_hold(String.t(), map(), User.t()) :: {:ok, ItemHold.t()} | {:error, term()}
  def update_hold(hold_id, update_params, user) do
    with {:ok, item_hold} <- get_hold_for_user(hold_id, user),
         :ok <- validate_update(item_hold, update_params),
         {:ok, updated_hold} <- ItemHold.update!(item_hold, update_params, actor: user) do
      {:ok, updated_hold}
    end
  end

  @doc """
  Checks if an item is available for holding during a specific time period.
  
  ## Parameters
  - item_id: ID of the item to check
  - start_time: Start time of the hold period
  - end_time: End time of the hold period
  
  ## Returns
  {:ok, boolean()} | {:error, reason}
  """
  @spec check_item_availability(String.t(), DateTime.t(), DateTime.t()) :: {:ok, boolean()} | {:error, term()}
  def check_item_availability(item_id, start_time, end_time) do
    try do
      case ItemHold.read!(
             filter: [
               item_id: [eq: item_id],
               status: [eq: :active],
               or: [
                 [and: [
                   start_time: [lte: end_time],
                   end_time: [gte: start_time]
                 ]],
                 [and: [
                   start_time: [lte: end_time],
                   end_time: [is_nil: true]
                 ]]
               ]
             ]
           ) do
        [] -> {:ok, true}
        _holds -> {:ok, false}
      end
    rescue
      error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
        {:error, error}
    end
  end

  # Private helper functions

  defp get_hold_for_user(hold_id, user) do
    try do
      case ItemHold.read!(id: hold_id, actor: user) do
        [item_hold] -> {:ok, item_hold}
        [] -> {:error, :not_found}
        _ -> {:error, :multiple_found}
      end
    rescue
      error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
        {:error, error}
    end
  end

  defp validate_cancellation(item_hold) do
    case item_hold.status do
      :active -> :ok
      :cancelled -> {:error, :already_cancelled}
      :expired -> {:error, :already_expired}
      _ -> {:error, :cannot_cancel}
    end
  end

  defp validate_deletion(item_hold) do
    case item_hold.status do
      :active -> {:error, :cannot_delete_active_hold}
      _ -> :ok
    end
  end

  defp validate_hold_params(params) do
    required_fields = [:item_id, :client_id, :start_time]
    
    missing_fields = required_fields |> Enum.filter(&(!Map.has_key?(params, &1)))
    
    case missing_fields do
      [] -> :ok
      _ -> {:error, {:missing_fields, missing_fields}}
    end
  end

  defp validate_update(item_hold, update_params) do
    case item_hold.status do
      :active -> :ok
      :cancelled -> {:error, :cannot_cancelled_hold}
      :expired -> {:error, :cannot_expired_hold}
      _ -> {:error, :cannot_update_hold}
    end
  end

  defp update_hold_status(item_hold, status, user) do
    update_params = %{status: status, updated_at: DateTime.utc_now()}
    ItemHold.update!(item_hold, update_params, actor: user)
  rescue
    error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
      {:error, error}
  end
end