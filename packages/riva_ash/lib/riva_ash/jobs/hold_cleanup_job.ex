defmodule RivaAsh.Jobs.HoldCleanupJob do
  @moduledoc """
  Background job to automatically clean up expired item holds.

  This job runs periodically to:
  1. Find expired active holds
  2. Mark them as inactive
  3. Log cleanup activities
  4. Optionally notify relevant parties

  Can be scheduled to run every few minutes to ensure timely cleanup.
  """

  use GenServer
  require Logger

  import Ash.Expr

  alias RivaAsh.Resources.ItemHold
  alias RivaAsh.Domain

  @cleanup_interval_ms 5 * 60 * 1000  # 5 minutes

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    # Schedule the first cleanup
    schedule_cleanup()
    {:ok, %{}}
  end

  @impl true
  def handle_info(:cleanup_expired_holds, state) do
    cleanup_expired_holds()
    schedule_cleanup()
    {:noreply, state}
  end

  @doc """
  Manually trigger cleanup of expired holds.
  Returns the number of holds that were cleaned up.
  """
  def cleanup_now do
    GenServer.call(__MODULE__, :cleanup_now)
  end

  @impl true
  def handle_call(:cleanup_now, _from, state) do
    count = cleanup_expired_holds()
    {:reply, count, state}
  end

  defp schedule_cleanup do
    Process.send_after(self(), :cleanup_expired_holds, @cleanup_interval_ms)
  end

  defp cleanup_expired_holds do
    Logger.info("Starting cleanup of expired item holds")

    try do
      # Find all active holds that have expired
      now = DateTime.utc_now()
      expired_holds = ItemHold
      |> Ash.Query.filter(expr(is_active == true and expires_at < ^now))
      |> Ash.read!(domain: Domain)

      count = length(expired_holds)

      if count > 0 do
        Logger.info("Found #{count} expired holds to clean up")

        # Deactivate each expired hold
        Enum.each(expired_holds, fn hold ->
          case deactivate_hold(hold) do
            {:ok, _updated_hold} ->
              Logger.debug("Deactivated expired hold #{hold.id}")
            {:error, error} ->
              Logger.error("Failed to deactivate hold #{hold.id}: #{inspect(error)}")
          end
        end)

        Logger.info("Completed cleanup of #{count} expired holds")
      else
        Logger.debug("No expired holds found")
      end

      count
    rescue
      error ->
        Logger.error("Error during hold cleanup: #{inspect(error)}")
        0
    end
  end

  defp deactivate_hold(hold) do
    hold
    |> Ash.Changeset.for_update(:update, %{
      is_active: false,
      updated_at: DateTime.utc_now()
    })
    |> Ash.update(domain: Domain)
  end

  @doc """
  Get statistics about hold cleanup operations.
  """
  def get_stats do
    active_holds = ItemHold
    |> Ash.Query.filter(expr(is_active == true))
    |> Ash.count!(domain: Domain)

    now = DateTime.utc_now()
    expired_holds = ItemHold
    |> Ash.Query.filter(expr(is_active == true and expires_at < ^now))
    |> Ash.count!(domain: Domain)

    total_holds = ItemHold
    |> Ash.count!(domain: Domain)

    %{
      total_holds: total_holds,
      active_holds: active_holds,
      expired_holds_pending_cleanup: expired_holds,
      last_cleanup: DateTime.utc_now()
    }
  end
end
