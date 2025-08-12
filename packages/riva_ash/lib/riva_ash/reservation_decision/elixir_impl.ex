defmodule RivaAsh.ReservationDecision.ElixirImpl do
  @behaviour RivaAsh.ReservationDecision

  @impl true
  def check_availability(item_id, start_dt, end_dt) do
    case RivaAsh.Availability.check_availability(item_id, start_dt, end_dt) do
      {:ok, :available} -> {:ok, {:available, get_capacity(item_id)}}
      {:ok, {:partial, n}} -> {:ok, {:partial, n}}
      {:error, reason} -> {:ok, {:unavailable, to_string(reason)}}
    end
  end

  defp get_capacity(item_id) do
    case RivaAsh.Resources.Item.by_id(item_id) do
      {:ok, item} -> item.capacity || 1
      _ -> 1
    end
  end
end

