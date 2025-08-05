defmodule RivaAsh.Position.PositionService do
  @moduledoc """
  Service for handling item position operations.
  Separates business logic from LiveView concerns.
  """
  
  alias RivaAsh.Resources.ItemPosition
  alias Ash.Query

  @doc """
  Gets all item positions for a specific user.
  
  ## Parameters
  - user: The user to get positions for
  
  ## Returns
  {:ok, {[ItemPosition.t()], meta}} | {:error, reason}
  """
  @spec get_user_positions(User.t()) :: {:ok, {[ItemPosition.t()], map()}} | {:error, term()}
  def get_user_positions(user) do
    try do
      case ItemPosition.read!(actor: user, sort: [inserted_at: :desc]) do
        item_positions ->
          meta = %{
            total: length(item_positions),
            page: 1,
            page_size: length(item_positions)
          }
          {:ok, {item_positions, meta}}
      end
    rescue
      error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
        {:error, error}
    end
  end

  @doc """
  Gets a specific item position by ID for a user.
  
  ## Parameters
  - position_id: ID of the position to retrieve
  - user: The user requesting the position
  
  ## Returns
  {:ok, ItemPosition.t()} | {:error, reason}
  """
  @spec get_position(String.t(), User.t()) :: {:ok, ItemPosition.t()} | {:error, term()}
  def get_position(position_id, user) do
    try do
      case ItemPosition.read!(id: position_id, actor: user) do
        [position] -> {:ok, position}
        [] -> {:error, :not_found}
        _ -> {:error, :multiple_found}
      end
    rescue
      error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
        {:error, error}
    end
  end

  @doc """
  Creates a new item position.
  
  ## Parameters
  - position_params: Map of position parameters
  - user: The user creating the position
  
  ## Returns
  {:ok, ItemPosition.t()} | {:error, reason}
  """
  @spec create_position(map(), User.t()) :: {:ok, ItemPosition.t()} | {:error, term()}
  def create_position(position_params, user) do
    with :ok <- validate_position_params(position_params),
         {:ok, position} <- ItemPosition.create!(position_params, actor: user) do
      {:ok, position}
    end
  end

  @doc """
  Updates an existing item position.
  
  ## Parameters
  - position_id: ID of the position to update
  - update_params: Map of update parameters
  - user: The user performing the update
  
  ## Returns
  {:ok, ItemPosition.t()} | {:error, reason}
  """
  @spec update_position(String.t(), map(), User.t()) :: {:ok, ItemPosition.t()} | {:error, term()}
  def update_position(position_id, update_params, user) do
    with {:ok, position} <- get_position(position_id, user),
         :ok <- validate_update_params(position, update_params),
         {:ok, updated_position} <- ItemPosition.update!(position, update_params, actor: user) do
      {:ok, updated_position}
    end
  end

  @doc """
  Deletes an item position.
  
  ## Parameters
  - position_id: ID of the position to delete
  - user: The user performing the deletion
  
  ## Returns
  {:ok, ItemPosition.t()} | {:error, reason}
  """
  @spec delete_position(String.t(), User.t()) :: {:ok, ItemPosition.t()} | {:error, term()}
  def delete_position(position_id, user) do
    with {:ok, position} <- get_position(position_id, user),
         :ok <- validate_deletion(position),
         {:ok, deleted_position} <- ItemPosition.destroy!(position, actor: user) do
      {:ok, deleted_position}
    end
  end

  @doc """
  Activates an item position.
  
  ## Parameters
  - position_id: ID of the position to activate
  - user: The user performing the activation
  
  ## Returns
  {:ok, ItemPosition.t()} | {:error, reason}
  """
  @spec activate_position(String.t(), User.t()) :: {:ok, ItemPosition.t()} | {:error, term()}
  def activate_position(position_id, user) do
    with {:ok, position} <- get_position(position_id, user),
         :ok <- validate_activation(position),
         {:ok, updated_position} <- update_position_status(position, :active, user) do
      {:ok, updated_position}
    end
  end

  @doc """
  Deactivates an item position.
  
  ## Parameters
  - position_id: ID of the position to deactivate
  - user: The user performing the deactivation
  
  ## Returns
  {:ok, ItemPosition.t()} | {:error, reason}
  """
  @spec deactivate_position(String.t(), User.t()) :: {:ok, ItemPosition.t()} | {:error, term()}
  def deactivate_position(position_id, user) do
    with {:ok, position} <- get_position(position_id, user),
         :ok <- validate_deactivation(position),
         {:ok, updated_position} <- update_position_status(position, :inactive, user) do
      {:ok, updated_position}
    end
  end

  @doc """
  Validates if coordinates are within layout bounds.
  
  ## Parameters
  - x_coord: X coordinate
  - y_coord: Y coordinate
  - layout_id: Layout ID to check against
  - user: The user performing the validation
  
  ## Returns
  {:ok, boolean()} | {:error, reason}
  """
  @spec validate_coordinates(integer(), integer(), String.t(), User.t()) :: {:ok, boolean()} | {:error, term()}
  def validate_coordinates(x_coord, y_coord, layout_id, user) do
    # This would check if the coordinates are within the layout's bounds
    # For now, we'll implement a basic validation
    case x_coord >= 0 and y_coord >= 0 do
      true -> {:ok, true}
      false -> {:error, :invalid_coordinates}
    end
  end

  @doc """
  Checks for overlapping positions in a layout.
  
  ## Parameters
  - position_id: ID of the position to check (can be nil for new positions)
  - layout_id: Layout ID to check within
  - x_coord: X coordinate
  - y_coord: Y coordinate
  - width: Width of the position
  - height: Height of the position
  - user: The user performing the check
  
  ## Returns
  {:ok, boolean()} | {:error, reason}
  """
  @spec check_overlap(String.t() | nil, String.t(), integer(), integer(), integer() | nil, integer() | nil, User.t()) :: {:ok, boolean()} | {:error, term()}
  def check_overlap(position_id, layout_id, x_coord, y_coord, width, height, user) do
    # This would check for overlapping positions in the same layout
    # For now, we'll implement a basic placeholder
    {:ok, false}
  end

  # Private helper functions

  defp validate_position_params(params) do
    required_fields = [:item_id, :layout_id, :x_coord, :y_coord]
    
    missing_fields = required_fields |> Enum.filter(&(!Map.has_key?(params, &1)))
    
    case missing_fields do
      [] -> :ok
      _ -> {:error, {:missing_fields, missing_fields}}
    end
  end

  defp validate_update_params(position, update_params) do
    # Don't allow changing item or layout as it would affect relationships
    restricted_fields = [:item_id, :layout_id]
    
    case Enum.any?(restricted_fields, &Map.has_key?(update_params, &1)) do
      true -> {:error, :cannot_change_restricted_fields}
      false -> :ok
    end
  end

  defp validate_deletion(position) do
    # Check if position has active items or reservations
    case has_active_associations(position) do
      true -> {:error, :cannot_delete_position_with_active_associations}
      false -> :ok
    end
  end

  defp validate_activation(position) do
    case position.status do
      :inactive -> :ok
      :active -> {:error, :already_active}
      _ -> {:error, :cannot_activate_position}
    end
  end

  defp validate_deactivation(position) do
    case position.status do
      :active -> :ok
      :inactive -> {:error, :already_inactive}
      _ -> {:error, :cannot_deactivate_position}
    end
  end

  defp update_position_status(position, status, user) do
    update_params = %{status: status, updated_at: DateTime.utc_now()}
    ItemPosition.update!(position, update_params, actor: user)
  rescue
    error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
      {:error, error}
  end

  defp has_active_associations(position) do
    # This would check for active items, reservations, etc. associated with this position
    # For now, return false as a placeholder
    false
  end
end