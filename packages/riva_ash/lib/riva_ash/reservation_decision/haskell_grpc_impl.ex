defmodule RivaAsh.ReservationDecision.HaskellGrpcImpl do
  @behaviour RivaAsh.ReservationDecision

  require Logger

  @impl true
  def check_availability(item_id, start_dt, end_dt) do
    # Placeholder: actual gRPC client call to be implemented once service is running
    Logger.warning("Haskell gRPC client not yet implemented; falling back to Elixir implementation")
    RivaAsh.ReservationDecision.ElixirImpl.check_availability(item_id, start_dt, end_dt)
  end
end
