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
  require Ash.Query

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
    case perform_cleanup_with_retry(state) do
      {count, new_state} ->
        schedule_cleanup()
        {:noreply, new_state}
      _ ->
        schedule_cleanup()
        {:noreply, state}
    end
  end

  @impl true
  def handle_info({:retry_cleanup, attempt}, state) do
    Logger.info("Retrying hold cleanup, attempt #{attempt}")
    case perform_cleanup_with_retry(state, attempt) do
      {count, new_state} -> {:noreply, new_state}
      _ -> {:noreply, state}
    end
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
    case perform_cleanup_with_retry(state) do
      {count, new_state} -> {:reply, count, new_state}
      _ -> {:reply, 0, state}
    end
  end

  @impl true
  def handle_call(:get_state, _from, state) do
    {:reply, state, state}
  end

  defp schedule_cleanup do
    Process.send_after(self(), :cleanup_expired_holds, @cleanup_interval_ms)
  end

  defp perform_cleanup_with_retry(state, attempt \\ 1) do
    case cleanup_expired_holds() do
      {:ok, count} ->
        new_state = %{state |
          cleanup_count: state.cleanup_count + count,
          last_cleanup: Timex.now()
        }
        {count, new_state}
      {:error, error} ->
        Logger.error("Hold cleanup failed on attempt #{attempt}: #{inspect(error)}")

        new_state = %{state |
          error_count: state.error_count + 1,
          last_error: {Timex.now(), error}
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

    now = Timex.now()
    case ItemHold
         |> Ash.Query.filter(expr(is_active == true and expires_at < ^now))
         |> Ash.read(domain: Domain) do
      {:ok, expired_holds} ->
        count = length(expired_holds)
        if count >= 0 do
          if count > 0 do
            Logger.info("Found #{count} expired holds to clean up")
            process_expired_holds(expired_holds, count)
          else
            Logger.debug("No expired holds found")
          end
          {:ok, count}
        else
          {:error, :invalid_count}
        end
      {:error, error} -> {:error, error}
    end
  end

  defp process_expired_holds(expired_holds, count) do
    batch_size = 50

    expired_holds
    |> Enum.chunk_every(batch_size)
    |> Enum.with_index()
    |> Enum.each(fn {batch, batch_index} ->
      Logger.debug("Processing batch #{batch_index + 1} of #{ceil(count / batch_size)}")
      process_hold_batch(batch)
    end)

    Logger.info("Completed cleanup of #{count} expired holds")
  end

  defp process_hold_batch(holds) do
    results = Enum.map(holds, &deactivate_hold_safely/1)
    errors = Enum.filter(results, &match?({:error, _}, &1))

    if Enum.empty?(errors) do
      :ok
    else
      Logger.error("Batch processing failed: #{inspect(errors)}")
    end
  end

  defp deactivate_hold_safely(hold) do
    case deactivate_hold(hold) do
      {:ok, _updated_hold} ->
        Logger.debug("Deactivated expired hold #{hold.id}")
        {:ok, hold.id}
      {:error, error} -> {:error, error}
    end
  end

  defp deactivate_hold(hold) do
    if hold do
      hold
      |> Ash.Changeset.for_update(:update, %{
        is_active: false,
        updated_at: Timex.now()
      })
      |> Ash.update(domain: Domain)
    else
      {:error, :hold_required}
    end
  end

  @doc """
  Get comprehensive statistics about hold cleanup operations.
  """
  def get_stats do
    with {:ok, active_holds} <- count_active_holds(),
         {:ok, expired_holds} <- count_expired_holds(),
         {:ok, total_holds} <- count_total_holds(),
         {:ok, job_state} <- get_job_state() do
      {:ok, %{
        total_holds: total_holds,
        active_holds: active_holds,
        expired_holds_pending_cleanup: expired_holds,
        cleanup_stats: %{
          total_cleanups: job_state.cleanup_count,
          total_errors: job_state.error_count,
          last_cleanup: job_state.last_cleanup,
          last_error: job_state.last_error
        },
        timestamp: Timex.now()
      }}
    else
      {:error, reason} ->
        Logger.error("Failed to get hold cleanup stats: #{inspect(reason)}")
        {:error, %{error: "Failed to retrieve statistics", reason: reason}}
    end
  end

  defp count_active_holds do
    ItemHold
    |> Ash.Query.filter(expr(is_active == true))
    |> Ash.count(domain: Domain)
  end

  defp count_expired_holds do
    now = Timex.now()
    ItemHold
    |> Ash.Query.filter(expr(is_active == true and expires_at < ^now))
    |> Ash.count(domain: Domain)
  end

  defp count_total_holds do
    ItemHold
    |> Ash.count(domain: Domain)
  end

  defp get_job_state do
    try do
      state = GenServer.call(__MODULE__, :get_state, 5_000)
      {:ok, state}
    rescue
      error -> {:error, error}
    end
  end

  @doc """
  Health check for the cleanup job.
  Returns :ok if healthy, {:error, reason} if not.
  """
  def health_check do
    case Process.whereis(__MODULE__) do
      nil -> {:error, "HoldCleanupJob process not running"}
      pid ->
        if Process.alive?(pid) do
          try do
            state = GenServer.call(__MODULE__, :get_state, 5_000)
            if is_map(state) do
              :ok
            else
              {:error, "Job not responding properly"}
            end
          rescue
            error -> {:error, "Failed to get job state: #{inspect(error)}"}
          end
        else
          {:error, "HoldCleanupJob process not alive"}
        end
    end
  end
end
