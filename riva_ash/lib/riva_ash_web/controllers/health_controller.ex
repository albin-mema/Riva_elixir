defmodule RivaAshWeb.HealthController do
  use Phoenix.Controller, formats: [:json]

  def check(conn, _params) do
    # Basic health check - verify database connectivity
    case RivaAsh.Repo.query("SELECT 1") do
      {:ok, _result} ->
        conn
        |> put_status(:ok)
        |> json(%{
          status: "healthy",
          timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
          database: "connected",
          service: "riva_ash_api"
        })

      {:error, _error} ->
        conn
        |> put_status(:service_unavailable)
        |> json(%{
          status: "unhealthy",
          timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
          database: "disconnected",
          service: "riva_ash_api"
        })
    end
  end
end