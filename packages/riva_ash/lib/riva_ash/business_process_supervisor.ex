defmodule RivaAsh.BusinessProcessSupervisor do
  @moduledoc """
  Supervisor for business process workers and background jobs.
  
  This supervisor manages the lifecycle of business-critical processes
  that need to be restarted independently of the main application.
  """

  use Supervisor
  require Logger

  @doc """
  Starts the business process supervisor.
  """
  @spec start_link(keyword()) :: Supervisor.on_start()
  def start_link(opts) do
    name = Keyword.get(opts, :name, __MODULE__)
    Supervisor.start_link(__MODULE__, opts, name: name)
  end

  @impl true
  @spec init(keyword()) :: {:ok, {Supervisor.sup_flags(), [Supervisor.child_spec()]}} | :ignore
  def init(_opts) do
    children = [
      # Reservation management processes
      RivaAsh.ReservationManager,
      RivaAsh.AvailabilityChecker,
      
      # Payment processing
      RivaAsh.PaymentProcessor,
      
      # Notification services
      RivaAsh.NotificationService,
      
      # Background job processors
      RivaAsh.JobProcessor,
      
      # Cache management
      RivaAsh.CacheManager
    ]

    Logger.info("Initializing business process supervisor with #{length(children)} children")

    # Use rest_for_one strategy: if a child fails, restart it and all children that started after it
    opts = [
      strategy: :rest_for_one,
      max_restarts: 5,
      max_seconds: 10,
      intensity: 1
    ]

    Supervisor.init(children, opts)
  end
end