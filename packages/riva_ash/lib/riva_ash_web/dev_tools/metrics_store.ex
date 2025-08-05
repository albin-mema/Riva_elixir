defmodule RivaAshWeb.DevTools.MetricsStore do
  @moduledoc """
  Persistent storage for devtools metrics and telemetry data.
  
  Features:
  - Time-series data storage
  - Efficient querying and aggregation
  - Data retention policies
  - Real-time subscriptions
  """
  use GenServer
  
  alias Phoenix.PubSub

  @table_name :devtools_metrics
  @pubsub_topic "devtools:metrics"

  defstruct [
    :table,
    :retention_policy,
    :subscribers
  ]

  ## Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def store_metric(type, data, metadata \\ %{}) do
    metric = %{
      id: generate_id(),
      type: type,
      data: data,
      metadata: metadata,
      timestamp: DateTime.utc_now()
    }
    
    GenServer.cast(__MODULE__, {:store_metric, metric})
  end

  def query_metrics(type, opts \\ []) do
    GenServer.call(__MODULE__, {:query_metrics, type, opts})
  end

  def get_metrics_summary(timeframe \\ :hour) do
    GenServer.call(__MODULE__, {:get_summary, timeframe})
  end

  def subscribe_to_metrics(types \\ :all) do
    PubSub.subscribe(RivaAsh.PubSub, @pubsub_topic)
    GenServer.cast(__MODULE__, {:subscribe, self(), types})
  end

  def get_slow_queries(threshold_ms \\ 100, limit \\ 50) do
    GenServer.call(__MODULE__, {:get_slow_queries, threshold_ms, limit})
  end

  def get_authorization_failures(limit \\ 50) do
    GenServer.call(__MODULE__, {:get_authorization_failures, limit})
  end

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
      retention_policy: Keyword.get(opts, :retention_hours, 24),
      subscribers: %{}
    }
    
    {:ok, state}
  end

  @impl true
  def handle_cast({:store_metric, metric}, state) do
    :ets.insert(state.table, {metric.timestamp, metric})
    
    # Notify subscribers
    broadcast_metric(metric)
    
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
    limit = Keyword.get(opts, :limit, 1000)
    
    metrics = 
      state.table
      |> :ets.select([
        {{:"$1", :"$2"}, 
         [{:andalso, {:>=, :"$1", since}, {:==, {:map_get, :type, :"$2"}, type}}], 
         [:"$2"]}
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
    since = DateTime.add(DateTime.utc_now(), -3600) # Last hour
    
    slow_queries = 
      state.table
      |> :ets.select([
        {{:"$1", :"$2"}, 
         [{:andalso, 
           {:>=, :"$1", since}, 
           {:==, {:map_get, :type, :"$2"}, :query},
           {:>=, {:map_get, :duration, {:map_get, :data, :"$2"}}, threshold_ms}}], 
         [:"$2"]}
      ])
      |> Enum.take(limit)
      |> Enum.sort_by(fn metric -> metric.data.duration end, :desc)
    
    {:reply, slow_queries, state}
  end

  def handle_call({:get_authorization_failures, limit}, _from, state) do
    since = DateTime.add(DateTime.utc_now(), -3600) # Last hour
    
    failures = 
      state.table
      |> :ets.select([
        {{:"$1", :"$2"}, 
         [{:andalso, 
           {:>=, :"$1", since}, 
           {:==, {:map_get, :type, :"$2"}, :authorization_failure}}], 
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
    schedule_cleanup()
    {:noreply, state}
  end

  def handle_info({:DOWN, ref, :process, _pid, _reason}, state) do
    subscribers = Map.delete(state.subscribers, ref)
    {:noreply, %{state | subscribers: subscribers}}
  end

  ## Private Functions

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
  end

  defp handle_telemetry_event([:devtools, :ash, :query], measurements, metadata, _config) do
    store_metric(:query, %{
      resource: metadata.resource,
      action: metadata.action,
      duration: measurements.duration && System.convert_time_unit(measurements.duration, :native, :millisecond),
      actor_role: metadata.actor_role
    }, metadata)
  end

  defp handle_telemetry_event([:devtools, :ash, :policy, :evaluation], measurements, metadata, _config) do
    store_metric(:policy_evaluation, %{
      resource: metadata.resource,
      action: metadata.action,
      result: metadata.result,
      duration: measurements.duration && System.convert_time_unit(measurements.duration, :native, :millisecond),
      actor_role: metadata.actor_role
    }, metadata)
  end

  defp handle_telemetry_event([:phoenix, :endpoint, :stop], measurements, metadata, _config) do
    store_metric(:http_request, %{
      method: metadata.conn.method,
      path: metadata.conn.request_path,
      status: metadata.conn.status,
      duration: measurements.duration && System.convert_time_unit(measurements.duration, :native, :millisecond)
    }, metadata)
  end

  defp handle_telemetry_event([:riva_ash, :authorization, :denied], measurements, metadata, _config) do
    store_metric(:authorization_failure, %{
      resource: metadata.resource,
      action: metadata.action,
      reason: metadata.reason,
      actor_role: metadata.actor_role
    }, metadata)
  end

  defp handle_telemetry_event([:devtools, :reactor, :step], measurements, metadata, _config) do
    store_metric(:reactor_step, %{
      reactor: metadata.reactor,
      step: metadata.step,
      status: metadata.status,
      duration: measurements.duration && System.convert_time_unit(measurements.duration, :native, :millisecond)
    }, metadata)
  end

  defp handle_telemetry_event(_event, _measurements, _metadata, _config) do
    :ok
  end

  defp broadcast_metric(metric) do
    PubSub.broadcast(RivaAsh.PubSub, @pubsub_topic, {:new_metric, metric})
  end

  defp generate_id do
    :crypto.strong_rand_bytes(16) |> Base.encode64(padding: false)
  end

  defp schedule_cleanup do
    Process.send_after(self(), :cleanup, :timer.minutes(10))
  end

  defp cleanup_old_metrics(state) do
    cutoff = DateTime.add(DateTime.utc_now(), -state.retention_policy * 3600)
    
    # Delete old metrics
    :ets.select_delete(state.table, [
      {{:"$1", :"$2"}, [{:<, :"$1", cutoff}], [true]}
    ])
  end

  defp get_timeframe_start(:minute), do: DateTime.add(DateTime.utc_now(), -60)
  defp get_timeframe_start(:hour), do: DateTime.add(DateTime.utc_now(), -3600)
  defp get_timeframe_start(:day), do: DateTime.add(DateTime.utc_now(), -86400)
  defp get_timeframe_start(:week), do: DateTime.add(DateTime.utc_now(), -604800)

  defp calculate_summary(table, since) do
    metrics = :ets.select(table, [
      {{:"$1", :"$2"}, [{:>=, :"$1", since}], [:"$2"]}
    ])

    %{
      total_requests: count_by_type(metrics, :http_request),
      total_queries: count_by_type(metrics, :query),
      avg_response_time: avg_duration_by_type(metrics, :http_request),
      avg_query_time: avg_duration_by_type(metrics, :query),
      authorization_failures: count_by_type(metrics, :authorization_failure),
      slow_queries: count_slow_queries(metrics, 100),
      policy_evaluations: count_by_type(metrics, :policy_evaluation)
    }
  end

  defp calculate_performance_trends(table, since) do
    metrics = :ets.select(table, [
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
        error_count: count_by_type(interval_metrics, :authorization_failure)
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
      |> Enum.map(&(&1.data.duration))
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
