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

  # 5 minutes
  @cleanup_interval_ms 5 * 60 * 1000
  @max_retries 3
  # 30 seconds
  @retry_delay_ms 30 * 1000

  @type state :: %{
          cleanup_count: non_neg_integer(),
          error_count: non_neg_integer(),
          last_cleanup: DateTime.t() | nil,
          last_error: {DateTime.t(), term()} | nil
        }

  @type cleanup_result :: {:ok, non_neg_integer()} | {:error, term()}
  @type stats_result :: {:ok, map()} | {:error, map()}

  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  @spec init(keyword()) :: {:ok, state()}
  def init(_opts) do
    Logger.info("Starting HoldCleanupJob")
    schedule_cleanup()

    {:ok,
     %{
       cleanup_count: 0,
       error_count: 0,
       last_cleanup: nil,
       last_error: nil
     }}
  end

  @impl true
  @spec handle_info(:cleanup_expired_holds, state()) :: {:noreply, state()}
  def handle_info(:cleanup_expired_holds, state) do
    state
    |> perform_cleanup_with_retry()
    |> handle_cleanup_result()
  end

  @impl true
  @spec handle_info({:retry_cleanup, pos_integer()}, state()) :: {:noreply, state()}
  def handle_info({:retry_cleanup, attempt}, state) do
    Logger.info("Retrying hold cleanup, attempt #{attempt}")

    state
    |> perform_cleanup_with_retry(attempt)
    |> handle_retry_result()
  end

  @impl true
  @spec handle_call(:cleanup_now, {pid(), term()}, state()) :: {:reply, non_neg_integer(), state()}
  def handle_call(:cleanup_now, _from, state) do
    state
    |> perform_cleanup_with_retry()
    |> handle_immediate_cleanup_result()
  end

  @impl true
  @spec handle_call(:get_state, {pid(), term()}, state()) :: {:reply, state(), state()}
  def handle_call(:get_state, _from, state) do
    {:reply, state, state}
  end

  # Public API

  @doc """
  Manually trigger cleanup of expired holds.
  Returns the number of holds that were cleaned up.
  """
  @spec cleanup_now() :: non_neg_integer()
  def cleanup_now do
    # 30 second timeout
    GenServer.call(__MODULE__, :cleanup_now, 30_000)
  end

  @doc """
  Get comprehensive statistics about hold cleanup operations.
  """
  @spec get_stats() :: stats_result()
  def get_stats do
    with {:ok, active_holds} <- count_active_holds(),
         {:ok, expired_holds} <- count_expired_holds(),
         {:ok, total_holds} <- count_total_holds(),
         {:ok, job_state} <- get_job_state() do
      build_stats_response(active_holds, expired_holds, total_holds, job_state)
    else
      {:error, reason} ->
        Logger.error("Failed to get hold cleanup stats: #{inspect(reason)}")
        {:error, %{error: "Failed to retrieve statistics", reason: reason}}
    end
  end

  @spec build_stats_response(non_neg_integer(), non_neg_integer(), non_neg_integer(), state()) :: stats_result()
  defp build_stats_response(active_holds, expired_holds, total_holds, job_state) do
    {:ok,
     %{
       total_holds: total_holds,
       active_holds: active_holds,
       expired_holds_pending_cleanup: expired_holds,
       cleanup_stats: extract_cleanup_stats(job_state),
       timestamp: DateTime.utc_now()
     }}
  end

  @spec extract_cleanup_stats(state()) :: map()
  defp extract_cleanup_stats(job_state) do
    %{
      total_cleanups: job_state.cleanup_count,
      total_errors: job_state.error_count,
      last_cleanup: job_state.last_cleanup,
      last_error: job_state.last_error
    }
  end

  @doc """
  Health check for the cleanup job.
  Returns :ok if healthy, {:error, reason} if not.
  """
  @spec health_check() :: :ok | {:error, String.t()}
  def health_check do
    case Process.whereis(__MODULE__) do
      nil ->
        {:error, "HoldCleanupJob process not running"}

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

  # Private helper functions

  @spec schedule_cleanup() :: :ok
  defp schedule_cleanup do
    Process.send_after(self(), :cleanup_expired_holds, @cleanup_interval_ms)
  end

  @spec perform_cleanup_with_retry(state(), pos_integer()) :: {non_neg_integer(), state()}
  defp perform_cleanup_with_retry(state, attempt \\ 1) do
    case cleanup_expired_holds() do
      {:ok, count} ->
        new_state = update_cleanup_success(state, count)
        {count, new_state}

      {:error, error} ->
        new_state = update_cleanup_failure(state, error, attempt)
        {0, new_state}
    end
  end

  @spec handle_cleanup_result({non_neg_integer(), state()}) :: {:noreply, state()}
  defp handle_cleanup_result({_count, new_state}) do
    new_state
    |> schedule_cleanup()
    |> respond_with_no_reply()
  end

  @spec handle_retry_result({non_neg_integer(), state()}) :: {:noreply, state()}
  defp handle_retry_result({_count, new_state}) do
    {:noreply, new_state}
  end

  @spec handle_immediate_cleanup_result({non_neg_integer(), state()}) :: {:reply, non_neg_integer(), state()}
  defp handle_immediate_cleanup_result({count, new_state}) do
    {:reply, count, new_state}
  end

  # Helper functions for consistent response handling
  @spec schedule_cleanup(state()) :: state()
  defp schedule_cleanup(state) do
    Process.send_after(self(), :cleanup_expired_holds, @cleanup_interval_ms)
    state
  end

  @spec respond_with_no_reply(state()) :: {:noreply, state()}
  defp respond_with_no_reply(state) do
    {:noreply, state}
  end

  @spec update_cleanup_success(state(), non_neg_integer()) :: state()
  defp update_cleanup_success(state, count) do
    state
    |> increment_cleanup_count(count)
    |> set_last_cleanup()
  end

  @spec update_cleanup_failure(state(), term(), pos_integer()) :: state()
  defp update_cleanup_failure(state, error, attempt) do
    state
    |> log_cleanup_failure(error, attempt)
    |> increment_error_count()
    |> set_last_error(error)
    |> handle_retry_scheduling(attempt)
  end

  # Helper functions for cleanup success handling
  @spec increment_cleanup_count(state(), non_neg_integer()) :: state()
  defp increment_cleanup_count(state, count) do
    %{state | cleanup_count: state.cleanup_count + count}
  end

  @spec set_last_cleanup(state()) :: state()
  defp set_last_cleanup(state) do
    %{state | last_cleanup: DateTime.utc_now()}
  end

  # Helper functions for cleanup failure handling
  @spec log_cleanup_failure(state(), term(), pos_integer()) :: state()
  defp log_cleanup_failure(state, error, attempt) do
    Logger.error("Hold cleanup failed on attempt #{attempt}: #{inspect(error)}")
    state
  end

  @spec increment_error_count(state()) :: state()
  defp increment_error_count(state) do
    %{state | error_count: state.error_count + 1}
  end

  @spec set_last_error(state(), term()) :: state()
  defp set_last_error(state, error) do
    %{state | last_error: {DateTime.utc_now(), error}}
  end

  @spec handle_retry_scheduling(state(), pos_integer()) :: state()
  defp handle_retry_scheduling(state, attempt) when attempt < @max_retries do
    Logger.info("Scheduling retry #{attempt + 1} in #{@retry_delay_ms}ms")
    Process.send_after(self(), {:retry_cleanup, attempt + 1}, @retry_delay_ms)
    state
  end

  defp handle_retry_scheduling(state, _attempt) do
    Logger.error("Hold cleanup failed after #{@max_retries} attempts, giving up")
    state
  end

  @spec cleanup_expired_holds() :: cleanup_result()
  defp cleanup_expired_holds do
    Logger.info("Starting cleanup of expired item holds")

    now = DateTime.utc_now()

    with {:ok, expired_holds} <- find_expired_holds(now),
         count <- length(expired_holds),
         :ok <- process_expired_holds(expired_holds, count) do
      {:ok, count}
    end
  end

  @spec find_expired_holds(DateTime.t()) :: {:ok, [map()]} | {:error, term()}
  defp find_expired_holds(now) do
    ItemHold
    |> Ash.Query.filter(expr(is_active == true and expires_at < ^now))
    |> Ash.read(domain: Domain)
  end

  @spec process_expired_holds([map()], non_neg_integer()) :: :ok | {:error, term()}
  defp process_expired_holds(expired_holds, count) when count > 0 do
    Logger.info("Found #{count} expired holds to clean up")

    expired_holds
    |> process_holds_in_batches()
  end

  defp process_expired_holds(_expired_holds, _count) do
    Logger.debug("No expired holds found")
    :ok
  end

  @spec process_holds_in_batches([map()]) :: :ok
  defp process_holds_in_batches(expired_holds) do
    expired_holds
    |> calculate_batch_config()
    |> process_batches()
  end

  @spec calculate_batch_config([map()]) :: {list(), non_neg_integer(), non_neg_integer()}
  defp calculate_batch_config(expired_holds) do
    batch_size = 50
    count = length(expired_holds)
    batches = Enum.chunk_every(expired_holds, batch_size)
    {batches, count, batch_size}
  end

  @spec process_batches({list(), non_neg_integer(), non_neg_integer()}) :: :ok
  defp process_batches({batches, count, batch_size}) do
    batches
    |> Enum.with_index()
    |> Enum.each(fn {batch, batch_index} ->
      Logger.debug("Processing batch #{batch_index + 1} of #{ceil(count / batch_size)}")
      process_hold_batch(batch)
    end)

    Logger.info("Completed cleanup of #{count} expired holds")
    :ok
  end

  @spec process_hold_batch([map()]) :: :ok
  defp process_hold_batch(holds) do
    holds
    |> Enum.map(&deactivate_hold_safely/1)
    |> process_batch_results()
  end

  @spec process_batch_results([{atom(), term()}]) :: :ok
  defp process_batch_results(results) do
    results
    |> extract_errors()
    |> handle_batch_errors()
  end

  @spec extract_errors([{atom(), term()}]) :: [term()]
  defp extract_errors(results) do
    Enum.filter(results, &match?({:error, _unmatched}, &1))
  end

  @spec handle_batch_errors([term()]) :: :ok
  defp handle_batch_errors([]), do: :ok
  defp handle_batch_errors(errors), do: Logger.error("Batch processing failed: #{inspect(errors)}")

  @spec deactivate_hold_safely(map()) :: {:ok, String.t()} | {:error, term()}
  defp deactivate_hold_safely(hold) do
    case deactivate_hold(hold) do
      {:ok, _updated_hold} ->
        Logger.debug("Deactivated expired hold #{hold.id}")
        {:ok, hold.id}

      {:error, error} ->
        {:error, error}
    end
  end

  @spec deactivate_hold(map() | nil) :: {:ok, map()} | {:error, term()}
  defp deactivate_hold(hold) do
    with true <- validate_hold_present(hold),
         changeset <- prepare_hold_changeset(hold),
         {:ok, updated_hold} <- Ash.update(changeset, domain: Domain) do
      {:ok, updated_hold}
    end
  end

  @spec validate_hold_present(map() | nil) :: true | {:error, :hold_required}
  defp validate_hold_present(nil), do: {:error, :hold_required}
  defp validate_hold_present(_hold), do: true

  @spec prepare_hold_changeset(map()) :: Ash.Changeset.t()
  defp prepare_hold_changeset(hold) do
    hold
    |> Ash.Changeset.for_update(:update, build_update_changeset())
  end

  @spec build_update_changeset() :: map()
  defp build_update_changeset do
    %{
      is_active: false,
      updated_at: DateTime.utc_now()
    }
  end

  @spec count_active_holds() :: {:ok, non_neg_integer()} | {:error, term()}
  defp count_active_holds do
    ItemHold
    |> Ash.Query.filter(expr(is_active == true))
    |> Ash.count(domain: Domain)
  end

  @spec count_expired_holds() :: {:ok, non_neg_integer()} | {:error, term()}
  defp count_expired_holds do
    now = DateTime.utc_now()

    ItemHold
    |> Ash.Query.filter(expr(is_active == true and expires_at < ^now))
    |> Ash.count(domain: Domain)
  end

  @spec count_total_holds() :: {:ok, non_neg_integer()} | {:error, term()}
  defp count_total_holds do
    ItemHold
    |> Ash.count(domain: Domain)
  end

  @spec get_job_state() :: {:ok, state()} | {:error, term()}
  defp get_job_state do
    try do
      state = GenServer.call(__MODULE__, :get_state, 5_000)
      {:ok, state}
    rescue
      error -> {:error, error}
    end
  end
end
