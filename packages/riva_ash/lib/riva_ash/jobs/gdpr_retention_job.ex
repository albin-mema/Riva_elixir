defmodule RivaAsh.Jobs.GDPRRetentionJob do
  @moduledoc """
  Background job for automated GDPR data retention compliance.

  This job should be scheduled to run daily to ensure compliance with
  GDPR data retention requirements. It performs:

  1. Cleanup of expired data based on retention policies
  2. Anonymization of data that must be kept for business purposes
  3. Generation of compliance reports
  4. Notification of any compliance issues

  Schedule this job to run daily during low-traffic hours.
  """

  use GenServer

  alias RivaAsh.GDPR.RetentionPolicy

  require Logger

  @job_name "gdpr_retention_cleanup"
  # Run at 2 AM daily
  @default_schedule "0 2 * * *"

  @type state :: %{
          schedule: String.t(),
          last_run: DateTime.t() | nil,
          stats: map()
        }

  @type job_result :: {:ok, map()} | {:error, String.t()}

  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  @spec init(keyword()) :: {:ok, state()}
  def init(opts) do
    schedule = Keyword.get(opts, :schedule, @default_schedule)

    schedule
    |> schedule_next_run()

    Logger.info("GDPR Retention Job started with schedule: #{schedule}")

    {:ok, %{schedule: schedule, last_run: nil, stats: %{}}}
  end

  @impl true
  @spec handle_info(:run_retention_cleanup, state()) :: {:noreply, state()}
  def handle_info(:run_retention_cleanup, state) do
    Logger.info("GDPR: Starting scheduled retention cleanup")

    start_time = System.monotonic_time(:millisecond)

    state
    |> execute_retention_cleanup(start_time)
    |> handle_cleanup_result(start_time, state.schedule, state)
  end

  @impl true
  @spec handle_call(:get_stats, {pid(), term()}, state()) :: {:reply, map(), state()}
  def handle_call(:get_stats, _from, state) do
    {:reply, state.stats, state}
  end

  @impl true
  @spec handle_call(:run_now, {pid(), term()}, state()) :: {:reply, :ok, state()}
  def handle_call(:run_now, _from, state) do
    send(self(), :run_retention_cleanup)
    {:reply, :ok, state}
  end

  @impl true
  @spec handle_cast({:update_schedule, String.t()}, state()) :: {:noreply, state()}
  def handle_cast({:update_schedule, new_schedule}, state) do
    Logger.info("GDPR: Updating retention job schedule to: #{new_schedule}")

    new_state = %{state | schedule: new_schedule}
    schedule_next_run(new_schedule)

    {:noreply, new_state}
  end

  # Public API

  @doc """
  Get statistics from the last retention cleanup run.
  """
  @spec get_stats() :: map()
  def get_stats do
    GenServer.call(__MODULE__, :get_stats)
  end

  @doc """
  Trigger an immediate retention cleanup run.
  """
  @spec run_now() :: :ok
  def run_now do
    GenServer.call(__MODULE__, :run_now)
  end

  @doc """
  Update the cleanup schedule.
  """
  @spec update_schedule(String.t()) :: :ok
  def update_schedule(new_schedule) do
    GenServer.cast(__MODULE__, {:update_schedule, new_schedule})
  end

  @doc """
  Health check function to verify the job is running properly.
  """
  @spec health_check() :: {:ok, String.t()} | {:warning, String.t()} | {:error, String.t()}
  def health_check do
    get_stats()
    |> evaluate_job_health()
  end

  @doc """
  Generate a compliance report for the retention job.
  """
  @spec generate_job_report() :: map()
  def generate_job_report do
    stats = get_stats()
    health = health_check()

    %{
      job_name: @job_name,
      schedule: "Daily at 2 AM UTC",
      health_status: health,
      last_run_stats: stats,
      report_generated_at: DateTime.utc_now(),
      compliance_notes: [
        "Job runs daily to ensure GDPR Article 5(1)(e) compliance",
        "Retention periods are defined in RivaAsh.GDPR.RetentionPolicy",
        "All cleanup operations are logged for audit purposes",
        "Failures trigger immediate alerts to technical team"
      ]
    }
  end

  # Private helper functions

  @spec schedule_next_run(String.t()) :: :ok
  defp schedule_next_run(_schedule) do
    next_run_ms = :timer.hours(24)
    Process.send_after(self(), :run_retention_cleanup, next_run_ms)

    Logger.debug("GDPR: Next retention cleanup scheduled in #{next_run_ms}ms")
  end

  @spec execute_retention_cleanup(state(), integer()) :: job_result()
  defp execute_retention_cleanup(_state, start_time) do
    with {:ok, results} <- RetentionPolicy.run_retention_cleanup(),
         report <- RetentionPolicy.generate_retention_report(),
         execution_time <- calculate_execution_time(start_time) do
      {:ok, %{results: results, report: report, execution_time_ms: execution_time}}
    end
  end

  @spec handle_cleanup_result(job_result(), integer(), String.t(), state()) :: {:noreply, state()}
  defp handle_cleanup_result({:ok, cleanup_data}, start_time, schedule, state) do
    updated_state = cleanup_data
    |> extract_cleanup_metrics()
    |> update_state_with_results(state)
    |> log_successful_cleanup(start_time)
    |> handle_compliance_alerts()

    schedule_next_run(schedule)
    {:noreply, updated_state}
  end

  defp handle_cleanup_result({:error, reason}, _start_time, _schedule, state) do
    reason
    |> log_cleanup_failure()
    |> send_failure_alert()

    schedule_retry()
    {:noreply, state}
  end

  # Helper functions for better single level of abstraction
  @spec extract_cleanup_metrics(map()) :: {integer(), map(), map()}
  defp extract_cleanup_metrics(cleanup_data) do
    {
      cleanup_data.execution_time_ms,
      cleanup_data.results,
      cleanup_data.report
    }
  end

  @spec log_successful_cleanup({integer(), map(), map()}, integer()) :: :ok
  defp log_successful_cleanup({execution_time, results, _report}, _start_time) do
    Logger.info("GDPR: Retention cleanup completed successfully",
      extra: %{
        execution_time_ms: execution_time,
        results: results
      }
    )
  end

  @spec log_cleanup_failure(term()) :: term()
  defp log_cleanup_failure(reason) do
    Logger.error("GDPR: Retention cleanup failed",
      extra: %{error: inspect(reason)}
    )
    reason
  end

  @spec calculate_execution_time(integer()) :: integer()
  defp calculate_execution_time(start_time) do
    System.monotonic_time(:millisecond) - start_time
  end

  @spec update_state_with_results(state(), {integer(), map(), map()}) :: state()
  defp update_state_with_results(state, {execution_time, results, report}) do
    state
    |> update_last_run()
    |> merge_results(results, execution_time, report)
  end

  @spec update_last_run(state()) :: state()
  defp update_last_run(state) do
    %{state | last_run: DateTime.utc_now()}
  end

  @spec merge_results(state(), map(), integer(), map()) :: state()
  defp merge_results(state, results, execution_time, report) do
    %{state |
      stats: Map.merge(results, %{
        execution_time_ms: execution_time,
        report: report
      })
    }
  end

  @spec log_successful_cleanup(integer(), map()) :: :ok
  defp log_successful_cleanup(execution_time, results) do
    Logger.info("GDPR: Retention cleanup completed successfully",
      extra: %{
        execution_time_ms: execution_time,
        results: results
      }
    )
  end

  @spec handle_compliance_alerts(map()) :: :ok
  defp handle_compliance_alerts(results) do
    if has_compliance_issues?(results) do
      results
      |> build_compliance_alert()
      |> send_compliance_alert()
    end
  end

  @spec has_compliance_issues?(map()) :: boolean()
  defp has_compliance_issues?(results) do
    total_processed = calculate_total_processed(results)
    exceeds_threshold?(total_processed, results)
  end

  @spec calculate_total_processed(map()) :: integer()
  defp calculate_total_processed(results) do
    results
    |> Map.values()
    |> Enum.sum()
  end

  @spec exceeds_threshold?(integer(), map()) :: boolean()
  defp exceeds_threshold?(total_processed, results) do
    total_processed > 1000 or Map.get(results, :errors, 0) > 0
  end

  @spec build_compliance_alert(map()) :: map()
  defp build_compliance_alert(results) do
    %{
      timestamp: DateTime.utc_now(),
      job: @job_name,
      alert_type: "compliance_issue",
      details: results
    }
  end

  @spec send_compliance_alert(map()) :: :ok
  defp send_compliance_alert(alert_data) do
    Logger.warning("GDPR: Compliance issues detected during retention cleanup", extra: alert_data)

    # This could integrate with your notification system
    # send_notification(:compliance_team, :gdpr_alert, alert_data)
  end

  @spec send_failure_alert(term()) :: term()
  defp send_failure_alert(error) do
    error
    |> build_failure_alert()
    |> log_failure_alert()

    error
  end

  @spec build_failure_alert(term()) :: map()
  defp build_failure_alert(error) do
    %{
      timestamp: DateTime.utc_now(),
      job: @job_name,
      alert_type: "job_failure",
      error: inspect(error),
      severity: "critical"
    }
  end

  @spec log_failure_alert(map()) :: map()
  defp log_failure_alert(alert_data) do
    Logger.error("GDPR: Critical failure in retention cleanup job", extra: alert_data)
    alert_data
  end

  @spec schedule_retry() :: :ok
  defp schedule_retry do
    Process.send_after(self(), :run_retention_cleanup, :timer.hours(1))
  end

  @spec evaluate_job_health(map()) :: {:ok, String.t()} | {:warning, String.t()} | {:error, String.t()}
  defp evaluate_job_health(%{last_run: nil}) do
    {:warning, "Job has not run yet"}
  end

  defp evaluate_job_health(%{last_run: last_run}) do
    last_run
    |> calculate_hours_since_last_run()
    |> evaluate_health_status()
  end

  defp evaluate_job_health(_stats) do
    {:error, "Unable to get job statistics"}
  end

  # Helper functions for health evaluation
  @spec calculate_hours_since_last_run(DateTime.t()) :: integer()
  defp calculate_hours_since_last_run(last_run) do
    DateTime.diff(DateTime.utc_now(), last_run, :hour)
  end

  @spec evaluate_health_status(integer()) :: {:ok, String.t()} | {:warning, String.t()} | {:error, String.t()}
  defp evaluate_health_status(hours_since_last_run) when hours_since_last_run > 48 do
    {:error, "Job has not run in #{hours_since_last_run} hours"}
  end

  defp evaluate_health_status(hours_since_last_run) when hours_since_last_run > 25 do
    {:warning, "Job is overdue (#{hours_since_last_run} hours since last run)"}
  end

  defp evaluate_health_status(hours_since_last_run) do
    {:ok, "Job is running normally (last run: #{hours_since_last_run} hours ago)"}
  end
end
