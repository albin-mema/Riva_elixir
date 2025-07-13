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
  import OK, only: [success: 1, failure: 1, ~>>: 2, for: 1, required: 2, map_all: 2, check: 3]

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
    perform_cleanup_with_retry(state)
    ~> fn {count, new_state} ->
      schedule_cleanup()
      {:noreply, new_state}
    end
    |> case do
      {:ok, result} -> result
      {:error, _} -> 
        schedule_cleanup()
        {:noreply, state}
    end
  end

  @impl true
  def handle_info({:retry_cleanup, attempt}, state) do
    Logger.info("Retrying hold cleanup, attempt #{attempt}")
    perform_cleanup_with_retry(state, attempt)
    ~> fn {count, new_state} -> {:noreply, new_state} end
    |> case do
      {:ok, result} -> result
      {:error, _} -> {:noreply, state}
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
    perform_cleanup_with_retry(state)
    ~> fn {count, new_state} -> {:reply, count, new_state} end
    |> case do
      {:ok, result} -> result
      {:error, _} -> {:reply, 0, state}
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
    OK.for do
      count <- cleanup_expired_holds()
      new_state = %{state |
        cleanup_count: state.cleanup_count + count,
        last_cleanup: DateTime.utc_now()
      }
    after
      {count, new_state}
    else
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

    OK.for do
      now = DateTime.utc_now()
      expired_holds <- ItemHold
                      |> Ash.Query.filter(expr(is_active == true and expires_at < ^now))
                      |> Ash.read(domain: Domain)
      count = length(expired_holds)
      _ <- OK.check(count >= 0, &(&1 >= 0), :invalid_count)
    after
      if count > 0 do
        Logger.info("Found #{count} expired holds to clean up")
        process_expired_holds(expired_holds, count)
      else
        Logger.debug("No expired holds found")
      end
      count
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
    holds
    |> OK.map_all(&deactivate_hold_safely/1)
    |> case do
      {:ok, _results} -> :ok
      {:error, errors} -> 
        Logger.error("Batch processing failed: #{inspect(errors)}")
    end
  end

  defp deactivate_hold_safely(hold) do
    deactivate_hold(hold)
    ~> fn _updated_hold ->
      Logger.debug("Deactivated expired hold #{hold.id}")
      hold.id
    end
  end

  defp deactivate_hold(hold) do
    OK.required(hold, :hold_required)
    ~>> fn validated_hold ->
      validated_hold
      |> Ash.Changeset.for_update(:update, %{
        is_active: false,
        updated_at: DateTime.utc_now()
      })
      |> Ash.update(domain: Domain)
    end
  end

  @doc """
  Get comprehensive statistics about hold cleanup operations.
  """
  def get_stats do
    OK.for do
      active_holds <- count_active_holds()
      expired_holds <- count_expired_holds()
      total_holds <- count_total_holds()
      job_state <- get_job_state()
    after
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
      reason ->
        Logger.error("Failed to get hold cleanup stats: #{inspect(reason)}")
        %{error: "Failed to retrieve statistics", reason: reason}
    end
  end

  defp count_active_holds do
    ItemHold
    |> Ash.Query.filter(expr(is_active == true))
    |> Ash.count(domain: Domain)
    ~> fn count -> count end
  end

  defp count_expired_holds do
    now = DateTime.utc_now()
    ItemHold
    |> Ash.Query.filter(expr(is_active == true and expires_at < ^now))
    |> Ash.count(domain: Domain)
    ~> fn count -> count end
  end

  defp count_total_holds do
    ItemHold
    |> Ash.count(domain: Domain)
    ~> fn count -> count end
  end

  defp get_job_state do
    OK.for do
      state <- OK.wrap(GenServer.call(__MODULE__, :get_state, 5_000))
    after
      state
    end
  end

  @doc """
  Health check for the cleanup job.
  Returns :ok if healthy, {:error, reason} if not.
  """
  def health_check do
    OK.for do
      pid <- Process.whereis(__MODULE__) |> OK.required("HoldCleanupJob process not running")
      _ <- OK.check(pid, &Process.alive?/1, "HoldCleanupJob process not alive")
      state <- GenServer.call(__MODULE__, :get_state, 5_000) |> OK.wrap()
      _ <- OK.check(state, &is_map/1, "Job not responding properly")
    after
      :ok
    end
  end
end
