defmodule RivaAshWeb.HealthController do
  @moduledoc """
  Controller for handling health check endpoints.

  Provides endpoints to check the health status of the application,
  including database connectivity and service status.
  """

  use Phoenix.Controller, formats: [:json]

  alias Plug.Conn
  alias RivaAsh.ErrorHelpers

  @type health_response :: %{
          status: String.t(),
          timestamp: String.t(),
          database: String.t(),
          service: String.t()
        }

  @doc """
  Health check endpoint.

  Returns the health status of the application including database connectivity.

  ## Responses
    * 200 - Application is healthy
      ```elixir
      %{
        "status" => "healthy",
        "timestamp" => "2023-01-01T00:00:00Z",
        "database" => "connected",
        "service" => "riva_ash_api"
      }
      ```

    * 503 - Service unavailable (database disconnected)
      ```elixir
      %{
        "status" => "unhealthy",
        "timestamp" => "2023-01-01T00:00:00Z",
        "database" => "disconnected",
        "service" => "riva_ash_api"
      }
      ```
  """
  @spec check(Conn.t(), map()) :: Conn.t()
  def check(conn, _params) do
    # Verify database connectivity
    case RivaAsh.Repo.query("SELECT 1") do
      {:ok, _result} ->
        send_health_response(conn, :ok, "healthy", "connected")

      {:error, _error} ->
        send_health_response(conn, :service_unavailable, "unhealthy", "disconnected")
    end
  end

  @spec send_health_response(Conn.t(), atom(), String.t(), String.t()) :: Conn.t()
  defp send_health_response(conn, status, health_status, db_status) do
    response = %{
      status: health_status,
      timestamp: Timex.now() |> DateTime.to_iso8601(),
      database: db_status,
      service: "riva_ash_api"
    }

    conn
    |> put_status(status)
    |> json(response)
  end
end
