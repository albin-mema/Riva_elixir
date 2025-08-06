defmodule RivaAshWeb.Telemetry do
  @moduledoc """
  Telemetry supervisor and metrics collection for Riva Ash web interface.

  Handles metrics collection, reporting, and custom telemetry events for
  Phoenix, Ash, Reactor, and business-specific operations. This module
  provides comprehensive monitoring capabilities for application performance,
  user behavior, and system health.

  The telemetry system includes:
  - Phoenix endpoint and LiveView metrics
  - Ash resource and action tracking
  - Reactor execution monitoring
  - Database performance metrics
  - VM and system resource monitoring
  - Custom business logic events
  """

  use Supervisor
  import Telemetry.Metrics

  @type supervisor_arg :: any()
  @type metric_name :: atom()
  @type metric_value :: number()
  @type metric_metadata :: map()
  @type event_name :: list()
  @type measurements :: map()
  @type config :: map()
  @type actor :: map() | nil
  @type result :: :ok | {:error, atom()}

  @doc """
  Starts the telemetry supervisor.

  Initializes the telemetry system with periodic metrics collection
  and custom event handlers for comprehensive application monitoring.
  """
  @spec start_link(supervisor_arg()) :: Supervisor.on_start()
  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end

  @impl true
  def init(_arg) do
    children = [
      # Telemetry poller will execute the given period measurements
      # every 10_000ms. Learn more here: https://hexdocs.pm/telemetry_metrics
      {:telemetry_poller, measurements: periodic_measurements(), period: 10_000}
      # Add reporters as children of your supervision tree.
      # {Telemetry.Metrics.ConsoleReporter, metrics: metrics()}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  @doc """
  Returns the complete metrics configuration for the application.

  Defines all metrics collected by the telemetry system including
  Phoenix framework metrics, Ash resource metrics, Reactor metrics,
  database performance metrics, VM metrics, and custom business metrics.
  """
  @spec metrics() :: list()
  def metrics do
    [
      # Phoenix Metrics
      summary("phoenix.endpoint.start.system_time",
        unit: {:native, :millisecond}
      ),
      summary("phoenix.endpoint.stop.duration",
        unit: {:native, :millisecond},
        tags: [:route, :method, :status]
      ),
      summary("phoenix.router_dispatch.start.system_time",
        tags: [:route],
        unit: {:native, :millisecond}
      ),
      summary("phoenix.router_dispatch.exception.duration",
        tags: [:route],
        unit: {:native, :millisecond}
      ),
      summary("phoenix.router_dispatch.stop.duration",
        tags: [:route],
        unit: {:native, :millisecond}
      ),
      summary("phoenix.socket_connected.duration",
        unit: {:native, :millisecond}
      ),
      summary("phoenix.channel_joined.duration",
        unit: {:native, :millisecond}
      ),
      summary("phoenix.channel_handled_in.duration",
        tags: [:event],
        unit: {:native, :millisecond}
      ),

      # LiveView Metrics
      summary("phoenix.live_view.mount.stop.duration",
        unit: {:native, :millisecond},
        tags: [:view, :connected?]
      ),
      summary("phoenix.live_view.handle_event.stop.duration",
        unit: {:native, :millisecond},
        tags: [:view, :event]
      ),
      summary("phoenix.live_view.handle_info.stop.duration",
        unit: {:native, :millisecond},
        tags: [:view]
      ),

      # Ash Metrics
      summary("ash.query.stop.duration",
        unit: {:native, :millisecond},
        tags: [:resource, :action, :actor_role]
      ),
      summary("ash.action.stop.duration",
        unit: {:native, :millisecond},
        tags: [:resource, :action, :type, :actor_role]
      ),
      counter("ash.policy.evaluation.stop.count",
        tags: [:resource, :action, :result, :actor_role]
      ),
      summary("ash.policy.evaluation.stop.duration",
        unit: {:native, :millisecond},
        tags: [:resource, :action, :result]
      ),

      # Reactor Metrics
      summary("reactor.step.stop.duration",
        unit: {:native, :millisecond},
        tags: [:reactor, :step, :status]
      ),
      counter("reactor.execution.stop.count",
        tags: [:reactor, :status]
      ),
      summary("reactor.execution.stop.duration",
        unit: {:native, :millisecond},
        tags: [:reactor, :status]
      ),

      # Database Metrics
      summary("riva_ash.repo.query.total_time",
        unit: {:native, :millisecond},
        tags: [:source, :command],
        description: "The sum of the other measurements"
      ),
      summary("riva_ash.repo.query.decode_time",
        unit: {:native, :millisecond},
        tags: [:source, :command],
        description: "The time spent decoding the data received from the database"
      ),
      summary("riva_ash.repo.query.query_time",
        unit: {:native, :millisecond},
        tags: [:source, :command],
        description: "The time spent executing the query"
      ),
      summary("riva_ash.repo.query.queue_time",
        unit: {:native, :millisecond},
        tags: [:source, :command],
        description: "The time spent waiting for a database connection"
      ),
      summary("riva_ash.repo.query.idle_time",
        unit: {:native, :millisecond},
        tags: [:source, :command],
        description: "The time the connection spent waiting before being checked out for the query"
      ),

      # VM Metrics
      last_value("vm.memory.total", unit: {:byte, :kilobyte}),
      last_value("vm.total_run_queue_lengths.total"),
      last_value("vm.total_run_queue_lengths.cpu"),
      last_value("vm.total_run_queue_lengths.io"),

      # Custom Business Metrics
      counter("riva_ash.reservation.created.count",
        tags: [:business_id, :item_type, :client_type]
      ),
      counter("riva_ash.authorization.denied.count",
        tags: [:resource, :action, :reason, :actor_role]
      ),
      summary("riva_ash.business_setup.duration",
        unit: {:native, :millisecond},
        tags: [:step_count, :with_errors]
      )
    ]
  end

  @doc """
  Returns periodic measurement functions for the telemetry poller.

  Defines functions that will be executed periodically to collect
  system and application metrics that aren't event-driven.
  """
  @spec periodic_measurements() :: list()
  defp periodic_measurements do
    [
      # A module, function and arguments to be invoked periodically.
      # This function must call :telemetry.execute/3 and a metric must be added above.
      {__MODULE__, :dispatch_vm_metrics, []},
      {__MODULE__, :dispatch_application_metrics, []}
    ]
  end

  @doc """
  Dispatches VM metrics for system monitoring.

  Collects and normalizes virtual machine metrics including memory usage
  and run queue lengths. Handles different Erlang/OTP versions gracefully.
  """
  @spec dispatch_vm_metrics() :: :ok
  def dispatch_vm_metrics do
    memory = normalize_memory_metrics()
    total_run_queue_lengths = normalize_run_queue_metrics()

    :telemetry.execute([:vm, :memory], memory, %{})
    :telemetry.execute([:vm, :total_run_queue_lengths], total_run_queue_lengths, %{})
  end

  @doc """
  Dispatches application-specific metrics.

  Collects custom application metrics including active connections,
  business counts, reservation counts, and cache performance.
  """
  @spec dispatch_application_metrics() :: :ok
  def dispatch_application_metrics do
    # Custom application metrics
    :telemetry.execute(
      [:riva_ash, :application, :info],
      %{
        active_connections: get_active_connections(),
        total_businesses: count_businesses(),
        total_reservations: count_reservations(),
        cache_hit_rate: get_cache_hit_rate()
      }
    )
  end

  # Helper functions for custom metrics
  @spec get_active_connections() :: non_neg_integer()
  defp get_active_connections do
    # Get actual connection pool stats
    case :ets.info(:riva_ash_repo_pool) do
      :undefined -> 0
      info -> Keyword.get(info, :size, 0)
    end
  rescue
    _ -> 0
  end

  @spec count_businesses() :: non_neg_integer()
  defp count_businesses do
    try do
      case RivaAsh.Resources.Business.read(domain: RivaAsh.Domain) do
        {:ok, businesses} -> length(businesses)
        _ -> 0
      end
    rescue
      _ -> 0
    end
  end

  @spec count_reservations() :: non_neg_integer()
  defp count_reservations do
    try do
      case RivaAsh.Resources.Reservation.read(domain: RivaAsh.Domain) do
        {:ok, reservations} -> length(reservations)
        _ -> 0
      end
    rescue
      _ -> 0
    end
  end

  @spec get_cache_hit_rate() :: float()
  defp get_cache_hit_rate do
    # Implement cache hit rate calculation if you have caching
    0.0
  end

  @doc """
  Attaches custom telemetry handlers for development tools.

  Sets up event handlers that enrich telemetry data with additional
  context and metadata for debugging and development purposes.
  """
  @spec attach_devtools_handlers() :: :ok | {:error, :already_attached}
  def attach_devtools_handlers do
    events = [
      [:ash, :policy, :evaluation, :start],
      [:ash, :policy, :evaluation, :stop],
      [:ash, :query, :start],
      [:ash, :query, :stop],
      [:ash, :action, :start],
      [:ash, :action, :stop],
      [:reactor, :step, :start],
      [:reactor, :step, :stop],
      [:reactor, :execution, :start],
      [:reactor, :execution, :stop],
      [:riva_ash, :authorization, :denied],
      [:riva_ash, :reservation, :created],
      [:riva_ash, :business_setup, :start],
      [:riva_ash, :business_setup, :stop]
    ]

    :telemetry.attach_many(
      "devtools-telemetry-handler",
      events,
      &__MODULE__.handle_event/4,
      %{}
    )
  end

  @doc """
  Handles telemetry events with enrichment and transformation.

  Processes various telemetry events to enrich them with additional
  metadata and context for better observability and debugging.
  """
  @spec handle_event(event_name(), measurements(), metric_metadata(), config()) :: :ok
  def handle_event([:ash, :policy, :evaluation, :stop], measurements, metadata, _config) do
    # Enrich with actor role information
    actor_role = get_actor_role(metadata[:actor])

    enriched_metadata = Map.put(metadata, :actor_role, actor_role)

    # Re-emit with enriched metadata for devtools
    :telemetry.execute(
      [:devtools, :ash, :policy, :evaluation],
      measurements,
      enriched_metadata
    )
  end

  def handle_event([:ash, :query, :stop], measurements, metadata, _config) do
    actor_role = get_actor_role(metadata[:actor])

    enriched_metadata = Map.put(metadata, :actor_role, actor_role)

    :telemetry.execute(
      [:devtools, :ash, :query],
      measurements,
      enriched_metadata
    )
  end

  def handle_event([:ash, :action, :stop], measurements, metadata, _config) do
    actor_role = get_actor_role(metadata[:actor])

    enriched_metadata = Map.put(metadata, :actor_role, actor_role)

    :telemetry.execute(
      [:devtools, :ash, :action],
      measurements,
      enriched_metadata
    )
  end

  def handle_event([:reactor, :step, :stop], measurements, metadata, _config) do
    :telemetry.execute(
      [:devtools, :reactor, :step],
      measurements,
      metadata
    )
  end

  def handle_event([:reactor, :execution, :stop], measurements, metadata, _config) do
    :telemetry.execute(
      [:devtools, :reactor, :execution],
      measurements,
      metadata
    )
  end

  def handle_event(_event, _measurements, _metadata, _config) do
    :ok
  end

  @doc """
  Extracts actor role from actor metadata.

  Safely extracts the role information from various actor types
  for authorization and auditing purposes.
  """
  @spec get_actor_role(actor()) :: atom()
  defp get_actor_role(nil), do: :anonymous
  defp get_actor_role(%{role: role}), do: role
  defp get_actor_role(%{__struct__: RivaAsh.Accounts.User, role: role}), do: role
  defp get_actor_role(%{__struct__: RivaAsh.Resources.Employee, role: role}), do: role
  defp get_actor_role(_), do: :unknown

  # Helper functions for normalizing metrics
  @spec normalize_memory_metrics() :: map()
  defp normalize_memory_metrics do
    case :erlang.memory() do
      mem when is_list(mem) -> Map.new(mem)
      mem when is_map(mem) -> mem
      _ -> %{}
    end
  end

  @spec normalize_run_queue_metrics() :: map()
  defp normalize_run_queue_metrics do
    case :erlang.statistics(:total_run_queue_lengths) do
      %{total: _t} = m ->
        m

      {cpu, io, total} when is_integer(cpu) and is_integer(io) and is_integer(total) ->
        %{cpu: cpu, io: io, total: total}

      _ ->
        %{}
    end
  end

  @doc """
  Emits custom telemetry events for authorization denials.

  Tracks authorization failures for security monitoring and
  access pattern analysis.
  """
  @spec emit_authorization_denied(String.t(), String.t(), String.t(), actor()) :: :ok
  def emit_authorization_denied(resource, action, reason, actor) do
    :telemetry.execute(
      [:riva_ash, :authorization, :denied],
      %{count: 1},
      %{
        resource: resource,
        action: action,
        reason: reason,
        actor_role: get_actor_role(actor)
      }
    )
  end

  @doc """
  Emits custom telemetry events for reservation creation.

  Tracks reservation creation for business analytics and
  performance monitoring.
  """
  @spec emit_reservation_created(map(), String.t(), String.t(), String.t()) :: :ok
  def emit_reservation_created(reservation, business_id, item_type, client_type) do
    :telemetry.execute(
      [:riva_ash, :reservation, :created],
      %{count: 1, amount: reservation.total_amount || 0},
      %{
        business_id: business_id,
        item_type: item_type,
        client_type: client_type,
        reservation_id: reservation.id
      }
    )
  end

  @doc """
  Emits custom telemetry events for business setup start.

  Tracks the beginning of business setup workflows for
  process monitoring and user experience analysis.
  """
  @spec emit_business_setup_start(map()) :: :ok
  def emit_business_setup_start(inputs) do
    :telemetry.execute(
      [:riva_ash, :business_setup, :start],
      %{system_time: System.system_time()},
      %{inputs: inputs}
    )
  end

  @doc """
  Emits custom telemetry events for business setup completion.

  Tracks the completion of business setup workflows with
  performance metrics and error tracking.
  """
  @spec emit_business_setup_stop(map(), integer(), list()) :: :ok
  def emit_business_setup_stop(result, step_count, errors) do
    :telemetry.execute(
      [:riva_ash, :business_setup, :stop],
      %{
        duration: System.system_time() - result.start_time,
        step_count: step_count
      },
      %{
        status: if(errors == [], do: :success, else: :error),
        with_errors: errors != [],
        error_count: length(errors)
      }
    )
  end
end
