defmodule RivaAsh.Jobs.HoldCleanupJob do
  @moduledoc """
  Background job to automatically clean up expired item holds.

  This job runs periodically to:
  1. Find expired active holds
  2. Mark them as inactive
  3. Log cleanup activities
  4. Track cleanup statistics
  5. Handle errors gracefully

  Can be scheduled to run every few minutes to ensure timely cleanup.
  Includes comprehensive error handling and monitoring capabilities.
  """

  use GenServer
  require Logger

  import Ash.Expr

  alias RivaAsh.Resources.ItemHold
  alias RivaAsh.Domain

  @cleanup_interval_ms 5 * 60 * 1000  # 5 minutes
  @max_retries 3
  @retry_delay_ms 30 * 1000  # 30 seconds

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    Logger.info("Starting HoldCleanupJob")
    # Schedule the first cleanup
    schedule_cleanup()
    {:ok, %{
      cleanup_count: 0,
      error_count: 0,
      last_cleanup: nil,
      last_error: nil
    }}
  end

  @impl true
  def handle_info(:cleanup_expired_holds, state) do
    {count, new_state} = perform_cleanup_with_retry(state)
    schedule_cleanup()
    {:noreply, new_state}
  end

  @impl true
  def handle_info({:retry_cleanup, attempt}, state) do
    Logger.info("Retrying hold cleanup, attempt #{attempt}")
    {count, new_state} = perform_cleanup_with_retry(state, attempt)
    {:noreply, new_state}
  end

  @doc """
  Manually trigger cleanup of expired holds.
  Returns the number of holds that were cleaned up.
  """
  def cleanup_now do
    GenServer.call(__MODULE__, :cleanup_now, 30_000)  # 30 second timeout
  end

  @impl true
  def handle_call(:cleanup_now, _from, state) do
    {count, new_state} = perform_cleanup_with_retry(state)
    {:reply, count, new_state}
  end

  @impl true
  def handle_call(:get_state, _from, state) do
    {:reply, state, state}
  end

  defp schedule_cleanup do
    Process.send_after(self(), :cleanup_expired_holds, @cleanup_interval_ms)
  end

  defp perform_cleanup_with_retry(state, attempt \\ 1) do
    try do
      count = cleanup_expired_holds()
      new_state = %{state |
        cleanup_count: state.cleanup_count + count,
        last_cleanup: DateTime.utc_now()
      }
      {count, new_state}
    rescue
      error ->
        Logger.error("Hold cleanup failed on attempt #{attempt}: #{inspect(error)}")

        new_state = %{state |
          error_count: state.error_count + 1,
          last_error: {DateTime.utc_now(), error}
        }

        if attempt < @max_retries do
          Logger.info("Scheduling retry #{attempt + 1} in #{@retry_delay_ms}ms")
          Process.send_after(self(), {:retry_cleanup, attempt + 1}, @retry_delay_ms)
        else
          Logger.error("Hold cleanup failed after #{@max_retries} attempts, giving up")
        end

        {0, new_state}
    end
  end

  defp cleanup_expired_holds do
    Logger.info("Starting cleanup of expired item holds")

    # Find all active holds that have expired
    now = DateTime.utc_now()

    case ItemHold
         |> Ash.Query.filter(expr(is_active == true and expires_at < ^now))
         |> Ash.read(domain: Domain) do
      {:ok, expired_holds} ->
        count = length(expired_holds)

        if count > 0 do
          Logger.info("Found #{count} expired holds to clean up")

          # Process holds in batches to avoid overwhelming the system
          batch_size = 50

          expired_holds
          |> Enum.chunk_every(batch_size)
          |> Enum.with_index()
          |> Enum.each(fn {batch, batch_index} ->
            Logger.debug("Processing batch #{batch_index + 1} of #{ceil(count / batch_size)}")
            process_hold_batch(batch)
          end)

          Logger.info("Completed cleanup of #{count} expired holds")
        else
          Logger.debug("No expired holds found")
        end

        count
      {:error, error} ->
        Logger.error("Failed to query expired holds: #{inspect(error)}")
        raise "Database query failed: #{inspect(error)}"
    end
  end

  defp process_hold_batch(holds) do
    # Process holds individually for now (bulk operations can be added later)
    Enum.each(holds, &deactivate_hold_individually/1)
  end

  defp deactivate_hold_individually(hold) do
    case deactivate_hold(hold) do
      {:ok, _updated_hold} ->
        Logger.debug("Deactivated expired hold #{hold.id}")
      {:error, error} ->
        Logger.error("Failed to deactivate hold #{hold.id}: #{inspect(error)}")
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
  Get comprehensive statistics about hold cleanup operations.
  """
  def get_stats do
    with {:ok, active_holds} <- count_active_holds(),
         {:ok, expired_holds} <- count_expired_holds(),
         {:ok, total_holds} <- count_total_holds(),
         {:ok, job_state} <- get_job_state() do
      %{
        total_holds: total_holds,
        active_holds: active_holds,
        expired_holds_pending_cleanup: expired_holds,
        cleanup_stats: %{
          total_cleanups: job_state.cleanup_count,
          total_errors: job_state.error_count,
          last_cleanup: job_state.last_cleanup,
          last_error: job_state.last_error
        },
        timestamp: DateTime.utc_now()
      }
    else
      {:error, reason} ->
        Logger.error("Failed to get hold cleanup stats: #{inspect(reason)}")
        %{error: "Failed to retrieve statistics", reason: reason}
    end
  end

  defp count_active_holds do
    try do
      count = ItemHold
              |> Ash.Query.filter(expr(is_active == true))
              |> Ash.count!(domain: Domain)
      {:ok, count}
    rescue
      e -> {:error, e}
    end
  end

  defp count_expired_holds do
    try do
      now = DateTime.utc_now()
      count = ItemHold
              |> Ash.Query.filter(expr(is_active == true and expires_at < ^now))
              |> Ash.count!(domain: Domain)
      {:ok, count}
    rescue
      e -> {:error, e}
    end
  end

  defp count_total_holds do
    try do
      count = ItemHold |> Ash.count!(domain: Domain)
      {:ok, count}
    rescue
      e -> {:error, e}
    end
  end

  defp get_job_state do
    try do
      state = GenServer.call(__MODULE__, :get_state, 5_000)
      {:ok, state}
    rescue
      e -> {:error, e}
    end
  end

  @doc """
  Health check for the cleanup job.
  Returns :ok if healthy, {:error, reason} if not.
  """
  def health_check do
    try do
      case Process.whereis(__MODULE__) do
        nil -> {:error, "HoldCleanupJob process not running"}
        pid when is_pid(pid) ->
          if Process.alive?(pid) do
            # Check if the job is responsive
            case GenServer.call(__MODULE__, :get_state, 5_000) do
              %{} -> :ok
              _ -> {:error, "Job not responding properly"}
            end
          else
            {:error, "HoldCleanupJob process not alive"}
          end
      end
    rescue
      e -> {:error, "Health check failed: #{inspect(e)}"}
    end
  end
end
