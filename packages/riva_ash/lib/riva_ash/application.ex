defmodule RivaAsh.Application do
  @moduledoc false

  use Application
  require Logger

  @impl true
  @spec start(Application.start_type(), term()) :: {:ok, pid()} | {:error, term()}
  def start(_type, _args) do
    with :ok <- initialize_application(),
         children <- build_supervision_tree(),
         {:ok, pid} <- start_supervisor(children) do
      log_startup_success(children)
      {:ok, pid}
    else
      {:error, reason} ->
        log_startup_failure(reason)
        {:error, reason}
    end
  end

  @impl true
  @spec start_phase(atom(), Application.start_type(), term()) :: :ok | {:error, term()}
  def start_phase(phase, _start_type, _phase_args) do
    case handle_start_phase(phase) do
      :ok -> :ok
      {:error, reason} ->
        Logger.error("Start phase #{inspect(phase)} failed: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @impl true
  @spec config_change(map(), map(), [atom()]) :: :ok
  def config_change(changed, _new, removed) do
    with :ok <- handle_endpoint_config_change(changed, removed),
         :ok <- validate_configuration() do
      :ok
    else
      {:error, reason} ->
        Logger.error("Configuration change failed: #{inspect(reason)}")
        :ok
    end
  end

  # Private helper functions following single level of abstraction principle

  @spec initialize_application() :: :ok | {:error, term()}
  defp initialize_application do
    with :ok <- configure_ash_sat_solver(),
         :ok <- log_sat_solver_configuration(),
         :ok <- initialize_telemetry() do
      :ok
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @spec build_supervision_tree() :: [Supervisor.child_spec() | map() | {module(), term()}]
  defp build_supervision_tree do
    base_children()
    |> maybe_add_database_child()
    |> add_business_process_supervisors()
  end

  @spec start_supervisor([term()]) :: {:ok, pid()} | {:error, term()}
  defp start_supervisor(children) do
    opts = [
      strategy: :one_for_one,
      name: RivaAsh.Supervisor,
      max_restarts: 3,
      max_seconds: 10
    ]
    
    Supervisor.start_link(children, opts)
  end

  @spec log_startup_success([term()]) :: :ok
  defp log_startup_success(children) do
    child_count = length(children)
    Logger.info("Application started successfully with #{child_count} children")
  end

  @spec log_startup_failure(term()) :: :ok
  defp log_startup_failure(reason) do
    Logger.error("Application failed to start: #{inspect(reason)}")
  end

  @spec handle_start_phase(atom()) :: :ok | {:error, term()}
  defp handle_start_phase(:migrate) do
    Logger.info("Running database migrations...")
    case safe_migrate() do
      :ok ->
        Logger.info("Database migrations completed successfully")
        :ok
      {:error, reason} ->
        Logger.error("Database migration failed: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp handle_start_phase(_phase), do: :ok

  @spec handle_endpoint_config_change(map(), [atom()]) :: :ok
  defp handle_endpoint_config_change(changed, removed) do
    RivaAshWeb.Endpoint.config_change(changed, removed)
  end

  @spec validate_configuration() :: :ok | {:error, term()}
  defp validate_configuration do
    # Add configuration validation logic here
    :ok
  end

  # Core configuration functions

  @spec configure_ash_sat_solver() :: :ok | {:error, term()}
  defp configure_ash_sat_solver do
    try do
      Application.put_env(:ash, :sat_solver, {SimpleSat, []})
      :ok
    rescue
      error -> {:error, "Failed to configure SAT solver: #{inspect(error)}"}
    end
  end

  @spec log_sat_solver_configuration() :: :ok
  defp log_sat_solver_configuration do
    sat_solver = Application.get_env(:ash, :sat_solver, :not_configured)
    Logger.info("Configured SAT solver: #{inspect(sat_solver)}")
    :ok
  end

  @spec initialize_telemetry() :: :ok
  defp initialize_telemetry do
    # Initialize telemetry metrics and event handlers
    :ok
  end

  @spec safe_migrate() :: :ok | {:error, term()}
  defp safe_migrate do
    try do
      RivaAsh.Release.migrate()
      :ok
    rescue
      error -> {:error, error}
    end
  end

  # Child specification builders

  @spec base_children() :: [Supervisor.child_spec() | map() | {module(), term()}]
  defp base_children do
    [
      RivaAshWeb.Telemetry,
      {Phoenix.PubSub, name: RivaAsh.PubSub},
      RivaAshWeb.Endpoint,
      TwMerge.Cache,
      {Finch, name: RivaAsh.Finch},
      RivaAsh.Jobs.HoldCleanupJob,
      RivaAsh.Accounts.RateLimiter,
      {DNSCluster, query: Application.get_env(:riva_ash, :dns_cluster_query) || :ignore}
    ]
  end

  @spec maybe_add_database_child([term()]) :: [term()]
  defp maybe_add_database_child(children) do
    if skip_database?() do
      children
    else
      children ++ [RivaAsh.Repo]
    end
  end

  @spec add_business_process_supervisors([term()]) :: [term()]
  defp add_business_process_supervisors(children) do
    children ++ [RivaAsh.BusinessProcessSupervisor]
  end

  @spec skip_database?() :: boolean()
  defp skip_database? do
    Application.get_env(:riva_ash, :skip_database, false) or
    System.get_env("SKIP_DB") == "true"
  end
end
