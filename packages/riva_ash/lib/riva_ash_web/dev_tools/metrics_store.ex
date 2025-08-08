alias RivaAshWeb.DevTools, as: DevTools

defmodule RivaAshWeb.DevTools.MetricsStore do
  @moduledoc """
  Persistent storage for devtools metrics and telemetry data.

  Provides a comprehensive metrics collection and analysis system for
  monitoring application performance, debugging issues, and gaining
  insights into system behavior.

  Features:
  - Time-series data storage with ETS for high performance
  - Efficient querying and aggregation capabilities
  - Configurable data retention policies
  - Real-time subscriptions via Phoenix PubSub
  - Comprehensive telemetry event handling
  - Performance trend analysis
  - Slow query detection
  - Authorization failure tracking

  Uses functional programming patterns with proper error handling,
  type safety specifications, and follows GenServer best practices.
  """

  use GenServer

  alias Phoenix.PubSub

  @table_name :devtools_metrics
  @pubsub_topic "devtools:metrics"
  @default_retention_hours 24
  @default_cleanup_interval :timer.minutes(10)
  @default_query_limit 1000
  @default_slow_query_threshold 100
  @default_result_limit 50

  @type metric_type ::
          :query
          | :policy_evaluation
          | :http_request
          | :authorization_failure
          | :reactor_step
          | :action
          | :ash
          | :reactor
          | :phoenix
          | :riva_ash
  @type metric_data :: map()
  @type metric_metadata :: map()
  @type metric :: %{
          id: String.t(),
          type: metric_type(),
          data: metric_data(),
          metadata: metric_metadata(),
          timestamp: DateTime.t()
        }
  @type timeframe :: :minute | :hour | :day | :week
  @type query_opts :: [since: DateTime.t(), limit: pos_integer()]
  @type subscriber_ref :: reference()
  @type subscriber_types :: :all | [metric_type()]

  defstruct [
    :table,
    :retention_policy,
    :subscribers,
    :cleanup_interval
  ]

  ## Client API

  @doc """
  Starts the MetricsStore GenServer.

  ## Options
    - `:retention_hours` - Number of hours to retain metrics (default: 24)
    - `:cleanup_interval` - Cleanup interval in milliseconds (default: 10 minutes)
  """
  @spec start_link(Keyword.t()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Stores a metric with the given type, data, and optional metadata.

  ## Parameters
    - `type` - Type of the metric
    - `data` - Metric data as a map
    - `metadata` - Optional metadata as a map

  ## Returns
    - `:ok` - Metric stored successfully
  """
  @spec store_metric(metric_type(), metric_data(), metric_metadata()) :: :ok
  def store_metric(type, data, metadata \\ %{}) do
    metric = create_metric(type, data, metadata)
    GenServer.cast(__MODULE__, {:store_metric, metric})
  end

  @doc """
  Queries metrics by type with optional filtering options.

  ## Parameters
    - `type` - Type of metrics to query
    - `opts` - Query options (since, limit)

  ## Returns
    - List of matching metrics
  """
  @spec query_metrics(metric_type(), query_opts()) :: [metric()]
  def query_metrics(type, opts \\ []) do
    GenServer.call(__MODULE__, {:query_metrics, type, opts})
  end

  @doc """
  Gets a metrics summary for the specified timeframe.

  ## Parameters
    - `timeframe` - Time period for summary (default: :hour)

  ## Returns
    - Map with summary statistics
  """
  @spec get_metrics_summary(timeframe()) :: map()
  def get_metrics_summary(timeframe \\ :hour) do
    GenServer.call(__MODULE__, {:get_summary, timeframe})
  end

  @doc """
  Subscribes to real-time metric updates.

  ## Parameters
    - `types` - Types of metrics to subscribe to (default: :all)

  ## Returns
    - `:ok` - Subscription successful
  """
  @spec subscribe_to_metrics(subscriber_types()) :: :ok
  def subscribe_to_metrics(types \\ :all) do
    PubSub.subscribe(RivaAsh.PubSub, @pubsub_topic)
    GenServer.cast(__MODULE__, {:subscribe, self(), types})
  end

  @doc """
  Gets slow queries exceeding the specified threshold.

  ## Parameters
    - `threshold_ms` - Duration threshold in milliseconds (default: 100)
    - `limit` - Maximum number of results (default: 50)

  ## Returns
    - List of slow queries sorted by duration (descending)
  """
  @spec get_slow_queries(pos_integer(), pos_integer()) :: [metric()]
  def get_slow_queries(threshold_ms \\ @default_slow_query_threshold, limit \\ @default_result_limit) do
    GenServer.call(__MODULE__, {:get_slow_queries, threshold_ms, limit})
  end

  @doc """
  Gets recent authorization failures.

  ## Parameters
    - `limit` - Maximum number of results (default: 50)

  ## Returns
    - List of authorization failures (newest first)
  """
  @spec get_authorization_failures(pos_integer()) :: [metric()]
  def get_authorization_failures(limit \\ @default_result_limit) do
    GenServer.call(__MODULE__, {:get_authorization_failures, limit})
  end

  @doc """
  Gets performance trends for the specified timeframe.

  ## Parameters
    - `timeframe` - Time period for analysis (default: :hour)

  ## Returns
    - List of performance data points by time interval
  """
  @spec get_performance_trends(timeframe()) :: [map()]
  def get_performance_trends(timeframe \\ :hour) do
    GenServer.call(__MODULE__, {:get_performance_trends, timeframe})
  end

  ## Server Implementation

  @impl true
  def init(opts) do
    table = :ets.new(@table_name, [:ordered_set, :public, :named_table])

    # Set up telemetry handlers
    attach_telemetry_handlers()

    # Schedule cleanup
    schedule_cleanup()

    state = %__MODULE__{
      table: table,
      retention_policy: Keyword.get(opts, :retention_hours, @default_retention_hours),
      subscribers: %{},
      cleanup_interval: Keyword.get(opts, :cleanup_interval, @default_cleanup_interval)
    }

    {:ok, state}
  end

  @impl true
  def handle_cast({:store_metric, metric}, state) do
    :ets.insert(state.table, {metric.timestamp, metric})

    # Notify subscribers
    broadcast_metric(metric, state)

    {:noreply, state}
  end

  def handle_cast({:subscribe, pid, types}, state) do
    ref = Process.monitor(pid)
    subscribers = Map.put(state.subscribers, ref, {pid, types})

    {:noreply, %{state | subscribers: subscribers}}
  end

  @impl true
  def handle_call({:query_metrics, type, opts}, _from, state) do
    since = Keyword.get(opts, :since, DateTime.add(DateTime.utc_now(), -3600))
    # Cap at reasonable limit
    limit = min(Keyword.get(opts, :limit, @default_query_limit), 10_000)

    metrics =
      state.table
      |> :ets.select([
        {{:"$1", :"$2"}, [{:andalso, {:>=, :"$1", since}, {:==, {:map_get, :type, :"$2"}, type}}], [:"$2"]}
      ])
      |> Enum.take(limit)
      |> Enum.reverse()

    {:reply, metrics, state}
  end

  def handle_call({:get_summary, timeframe}, _from, state) do
    since = get_timeframe_start(timeframe)

    summary = calculate_summary(state.table, since)

    {:reply, summary, state}
  end

  def handle_call({:get_slow_queries, threshold_ms, limit}, _from, state) do
    # Last hour
    since = DateTime.add(DateTime.utc_now(), -3600)

    slow_queries =
      state.table
      |> :ets.select([
        {{:"$1", :"$2"},
         [
           {:andalso, {:>=, :"$1", since}, {:==, {:map_get, :type, :"$2"}, :query},
            {:>=, {:map_get, :duration, {:map_get, :data, :"$2"}}, threshold_ms}}
         ], [:"$2"]}
      ])
      |> Enum.take(limit)
      |> Enum.sort_by(fn metric -> metric.data.duration end, :desc)

    {:reply, slow_queries, state}
  end

  def handle_call({:get_authorization_failures, limit}, _from, state) do
    # Last hour
    since = DateTime.add(DateTime.utc_now(), -3600)

    failures =
      state.table
      |> :ets.select([
        {{:"$1", :"$2"}, [{:andalso, {:>=, :"$1", since}, {:==, {:map_get, :type, :"$2"}, :authorization_failure}}],
         [:"$2"]}
      ])
      |> Enum.take(limit)
      |> Enum.reverse()

    {:reply, failures, state}
  end

  def handle_call({:get_performance_trends, timeframe}, _from, state) do
    since = get_timeframe_start(timeframe)

    trends = calculate_performance_trends(state.table, since)

    {:reply, trends, state}
  end

  @impl true
  def handle_info(:cleanup, state) do
    cleanup_old_metrics(state)
    schedule_cleanup(state)
    {:noreply, state}
  end

  def handle_info({:DOWN, ref, :process, _pid, _reason}, state) do
    subscribers = Map.delete(state.subscribers, ref)
    {:noreply, %{state | subscribers: subscribers}}
  end

  ## Private Functions

  defp create_metric(type, data, metadata) do
    %{
      id: generate_id(),
      type: type,
      data: data,
      metadata: metadata,
      timestamp: DateTime.utc_now()
    }
  end

  defp attach_telemetry_handlers do
    events = [
      [:devtools, :ash, :query],
      [:devtools, :ash, :action],
      [:devtools, :ash, :policy, :evaluation],
      [:devtools, :reactor, :step],
      [:devtools, :reactor, :execution],
      [:phoenix, :endpoint, :stop],
      [:riva_ash, :authorization, :denied],
      [:riva_ash, :reservation, :created]
    ]

    :telemetry.attach_many(
      "devtools-metrics-store",
      events,
      &handle_telemetry_event/4,
      %{}
    )
  rescue
    error ->
      # Log error but don't crash the server
      IO.puts("Warning: Failed to attach telemetry handlers: #{inspect(error)}")
      :ok
  end

  defp handle_telemetry_event([:devtools, :ash, :query], measurements, metadata, _config) do
    store_metric(
      :query,
      %{
        resource: metadata.resource,
        action: metadata.action,
        duration: measurements.duration && System.convert_time_unit(measurements.duration, :native, :millisecond),
        actor_role: metadata.actor_role
      },
      metadata
    )
  end

  defp handle_telemetry_event([:devtools, :ash, :policy, :evaluation], measurements, metadata, _config) do
    store_metric(
      :policy_evaluation,
      %{
        resource: metadata.resource,
        action: metadata.action,
        result: metadata.result,
        duration: measurements.duration && System.convert_time_unit(measurements.duration, :native, :millisecond),
        actor_role: metadata.actor_role
      },
      metadata
    )
  end

  defp handle_telemetry_event([:phoenix, :endpoint, :stop], measurements, metadata, _config) do
    store_metric(
      :http_request,
      %{
        method: metadata.conn.method,
        path: metadata.conn.request_path,
        status: metadata.conn.status,
        duration: measurements.duration && System.convert_time_unit(measurements.duration, :native, :millisecond)
      },
      metadata
    )
  end

  defp handle_telemetry_event([:riva_ash, :authorization, :denied], measurements, metadata, _config) do
    store_metric(
      :authorization_failure,
      %{
        resource: metadata.resource,
        action: metadata.action,
        reason: metadata.reason,
        actor_role: metadata.actor_role
      },
      metadata
    )
  end

  defp handle_telemetry_event([:devtools, :reactor, :step], measurements, metadata, _config) do
    store_metric(
      :reactor_step,
      %{
        reactor: metadata.reactor,
        step: metadata.step,
        status: metadata.status,
        duration: measurements.duration && System.convert_time_unit(measurements.duration, :native, :millisecond)
      },
      metadata
    )
  end

  defp handle_telemetry_event(_event, _measurements, _metadata, _config) do
    :ok
  end

  defp broadcast_metric(metric, state) do
    PubSub.broadcast(RivaAsh.PubSub, @pubsub_topic, {:new_metric, metric})

    # Notify specific subscribers
    for {ref, {pid, types}} <- state.subscribers do
      if should_notify_metric(metric, types) do
        send(pid, {:new_metric, metric})
      end
    end
  rescue
    error ->
      IO.puts("Warning: Failed to broadcast metric: #{inspect(error)}")
      :ok
  end

  defp should_notify_metric(_metric, :all), do: true
  defp should_notify_metric(metric, types) when is_list(types), do: metric.type in types

  defp generate_id do
    :crypto.strong_rand_bytes(16) |> Base.encode64(padding: false)
  end

  defp schedule_cleanup, do: schedule_cleanup(%__MODULE__{cleanup_interval: @default_cleanup_interval})

  defp schedule_cleanup(state) do
    Process.send_after(self(), :cleanup, state.cleanup_interval)
  end

  defp cleanup_old_metrics(state) do
    cutoff = DateTime.add(DateTime.utc_now(), -state.retention_policy * 3600)

    # Delete old metrics
    deleted_count =
      :ets.select_delete(state.table, [
        {{:"$1", :"$2"}, [{:<, :"$1", cutoff}], [true]}
      ])

    if deleted_count > 0 do
      IO.puts("MetricsStore: Cleaned up #{deleted_count} old metrics")
    end
  end

  defp get_timeframe_start(:minute), do: DateTime.add(DateTime.utc_now(), -60)
  defp get_timeframe_start(:hour), do: DateTime.add(DateTime.utc_now(), -3600)
  defp get_timeframe_start(:day), do: DateTime.add(DateTime.utc_now(), -86_400)
  defp get_timeframe_start(:week), do: DateTime.add(DateTime.utc_now(), -604_800)

  defp calculate_summary(table, since) do
    metrics =
      :ets.select(table, [
        {{:"$1", :"$2"}, [{:>=, :"$1", since}], [:"$2"]}
      ])

    %{
      total_requests: count_by_type(metrics, :http_request),
      total_queries: count_by_type(metrics, :query),
      avg_response_time: avg_duration_by_type(metrics, :http_request),
      avg_query_time: avg_duration_by_type(metrics, :query),
      authorization_failures: count_by_type(metrics, :authorization_failure),
      slow_queries: count_slow_queries(metrics, 100),
      policy_evaluations: count_by_type(metrics, :policy_evaluation),
      data_points: length(metrics),
      time_range: %{
        start: since,
        end: DateTime.utc_now()
      }
    }
  end

  defp calculate_performance_trends(table, since) do
    metrics =
      :ets.select(table, [
        {{:"$1", :"$2"}, [{:>=, :"$1", since}], [:"$2"]}
      ])

    # Group by 5-minute intervals
    interval_ms = 5 * 60 * 1000

    metrics
    |> Enum.group_by(fn metric ->
      timestamp_ms = DateTime.to_unix(metric.timestamp, :millisecond)
      div(timestamp_ms, interval_ms) * interval_ms
    end)
    |> Enum.map(fn {interval, interval_metrics} ->
      %{
        timestamp: DateTime.from_unix!(interval, :millisecond),
        avg_response_time: avg_duration_by_type(interval_metrics, :http_request),
        request_count: count_by_type(interval_metrics, :http_request),
        query_count: count_by_type(interval_metrics, :query),
        error_count: count_by_type(interval_metrics, :authorization_failure),
        slow_query_count: count_slow_queries(interval_metrics, 100),
        policy_evaluation_count: count_by_type(interval_metrics, :policy_evaluation)
      }
    end)
    |> Enum.sort_by(& &1.timestamp)
  end

  defp count_by_type(metrics, type) do
    Enum.count(metrics, &(&1.type == type))
  end

  defp avg_duration_by_type(metrics, type) do
    durations =
      metrics
      |> Enum.filter(&(&1.type == type))
      |> Enum.map(& &1.data.duration)
      |> Enum.filter(&is_number/1)

    case durations do
      [] -> 0
      durations -> Enum.sum(durations) / length(durations)
    end
  end

  defp count_slow_queries(metrics, threshold_ms) do
    metrics
    |> Enum.filter(fn metric ->
      metric.type == :query &&
        is_number(metric.data.duration) &&
        metric.data.duration > threshold_ms
    end)
    |> length()
  end
end
