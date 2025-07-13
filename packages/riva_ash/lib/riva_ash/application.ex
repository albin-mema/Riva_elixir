defmodule RivaAsh.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application
  import OK, only: [success: 1, failure: 1, ~>>: 2, for: 1]

  @impl true
  def start(_type, _args) do
    # Configure Ash to use SimpleSat
    Application.put_env(:ash, :sat_solver, {SimpleSat, []})

    # Log the SAT solver configuration for debugging
    sat_solver = Application.get_env(:ash, :sat_solver, :not_configured)
    require Logger
    Logger.info("Configured SAT solver: #{inspect(sat_solver)}")

    # Define the children to be supervised
    children = [
      # Start the Ecto repository
      RivaAsh.Repo,
      # Start the Telemetry supervisor
      RivaAshWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: RivaAsh.PubSub},
      # Start the Endpoint (http/https)
      RivaAshWeb.Endpoint,
      # Start TwMerge.Cache
      TwMerge.Cache,
      # Start Finch for HTTP clients
      {Finch, name: RivaAsh.Finch},
      # Start background jobs
      RivaAsh.Jobs.HoldCleanupJob,
      # DNS cluster for distributed nodes
      {DNSCluster, query: Application.get_env(:riva_ash, :dns_cluster_query) || :ignore}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: RivaAsh.Supervisor]
    Supervisor.start_link(children, opts)
    ~> fn pid -> pid end
  end

  # Handle application start phases
  @impl true
  def start_phase(:migrate, _start_type, _phase_args) do
    # Run database migrations during the migrate phase
    # This is useful for production deployments
    require Logger
    Logger.info("Running database migrations...")

    # Only run migrations if the repo is available
    OK.for do
      result <- OK.wrap(RivaAsh.Release.migrate())
    after
      Logger.info("Database migrations completed successfully")
      :ok
    else
      error ->
        Logger.error("Database migration failed: #{inspect(error)}")
        # Don't fail the application startup if migrations fail
        # This allows the app to start even if migrations have issues
        :ok
    end
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    RivaAshWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
