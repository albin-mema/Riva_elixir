defmodule RivaAshWeb.Telemetry do
  use Supervisor
  import Telemetry.Metrics

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
        description:
          "The time the connection spent waiting before being checked out for the query"
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

  defp periodic_measurements do
    [
      # A module, function and arguments to be invoked periodically.
      # This function must call :telemetry.execute/3 and a metric must be added above.
      {__MODULE__, :dispatch_vm_metrics, []},
      {__MODULE__, :dispatch_application_metrics, []}
    ]
  end

  # In tests, telemetry_poller invokes this regularly. Some measurements may return
  # unexpected shapes (e.g., maps or tuples) depending on Erlang/OTP version and flags.
  # Normalize them into maps to avoid function_clause errors seen in tests.
  def dispatch_vm_metrics do
    memory =
      case :erlang.memory() do
        mem when is_list(mem) -> Map.new(mem)
        mem when is_map(mem) -> mem
        _ -> %{}
      end

    total_run_queue_lengths =
      case :erlang.statistics(:total_run_queue_lengths) do
        %{total: _t} = m -> m
        {cpu, io, total} when is_integer(cpu) and is_integer(io) and is_integer(total) ->
          %{cpu: cpu, io: io, total: total}
        _ ->
          %{}
      end

    :telemetry.execute([:vm, :memory], memory, %{})
    :telemetry.execute([:vm, :total_run_queue_lengths], total_run_queue_lengths, %{})
  end

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
  defp get_active_connections do
    # Get actual connection pool stats
    case :ets.info(:riva_ash_repo_pool) do
      :undefined -> 0
      info -> Keyword.get(info, :size, 0)
    end
  rescue
    _ -> 0
  end

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

  defp get_cache_hit_rate do
    # Implement cache hit rate calculation if you have caching
    0.0
  end

  @doc """
  Attach custom telemetry handlers for devtools.
  """
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

  defp get_actor_role(nil), do: :anonymous
  defp get_actor_role(%{role: role}), do: role
  defp get_actor_role(%{__struct__: RivaAsh.Accounts.User, role: role}), do: role
  defp get_actor_role(%{__struct__: RivaAsh.Resources.Employee, role: role}), do: role
  defp get_actor_role(_), do: :unknown

  @doc """
  Custom telemetry events for business logic
  """
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

  def emit_business_setup_start(inputs) do
    :telemetry.execute(
      [:riva_ash, :business_setup, :start],
      %{system_time: System.system_time()},
      %{inputs: inputs}
    )
  end

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
