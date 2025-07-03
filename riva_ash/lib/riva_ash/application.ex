defmodule RivaAsh.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    # Configure Ash to use SimpleSat
    Application.put_env(:ash, :sat_solver, {SimpleSat, []})

    # Log the SAT solver configuration for debugging
    sat_solver = Application.get_env(:ash, :sat_solver, :not_configured)
    require Logger
    Logger.info("Configured SAT solver: #{inspect(sat_solver)}")

    children = [
      RivaAsh.Repo,
      {DNSCluster, query: Application.get_env(:riva_ash, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: RivaAsh.PubSub},
      {Finch, name: RivaAsh.Finch},
      RivaAshWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: RivaAsh.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    RivaAshWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
