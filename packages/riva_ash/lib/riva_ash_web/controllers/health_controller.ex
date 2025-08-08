alias RivaAsh.Repo, as: Repo

defmodule RivaAshWeb.HealthController do
  @moduledoc """
  Controller for handling health check endpoints.

  Provides endpoints to check the health status of the application,
  including database connectivity and service status.

  Uses functional programming patterns with proper error handling and
  type safety specifications.
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
    case verify_database_connectivity() do
      {:ok, _result} ->
        send_healthy_response(conn)

      {:error, _reason} ->
        send_unhealthy_response(conn)
    end
  end

  @doc """
  Enhanced health check endpoint with detailed metrics.

  Returns comprehensive health status including database connectivity,
  service metrics, and system information.

  ## Responses
    * 200 - Application is healthy with detailed metrics
    * 503 - Service unavailable with detailed error information
  """
  @spec check_detailed(Conn.t(), map()) :: Conn.t()
  def check_detailed(conn, _params) do
    case {check_database_status(), collect_service_metrics()} do
      {{:ok, db_status}, {:ok, service_metrics}} ->
        send_detailed_healthy_response(conn, db_status, service_metrics)

      {:error, reason} ->
        send_detailed_unhealthy_response(conn, reason)
    end
  end

  # Private helper functions

  defp verify_database_connectivity do
    case RivaAsh.Repo.query("SELECT 1") do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, reason}
    end
  end

  defp check_database_status do
    case verify_database_connectivity() do
      {:ok, _result} -> {:ok, "connected"}
      {:error, _reason} -> {:ok, "disconnected"}
    end
  end

  defp collect_service_metrics do
    metrics = %{
      uptime: calculate_uptime(),
      memory_usage: get_memory_usage(),
      process_count: get_process_count()
    }

    {:ok, metrics}
  end

  defp calculate_uptime do
    case :application.get_start_time(:riva_ash) do
      {:ok, start_time} ->
        seconds = :erlang.system_time(:second) - start_time
        format_duration(seconds)

      :undefined ->
        "unknown"
    end
  end

  defp get_memory_usage do
    {memory, _unmatched} = :erlang.memory()
    format_bytes(memory)
  end

  defp get_process_count do
    length(:erlang.processes())
  end

  defp send_healthy_response(conn) do
    send_health_response(conn, :ok, "healthy", "connected")
  end

  defp send_unhealthy_response(conn) do
    send_health_response(conn, :service_unavailable, "unhealthy", "disconnected")
  end

  defp send_detailed_healthy_response(conn, db_status, service_metrics) do
    response = %{
      status: "healthy",
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
      database: db_status,
      service: "riva_ash_api",
      metrics: service_metrics
    }

    conn
    |> put_status(:ok)
    |> json(response)
  end

  defp send_detailed_unhealthy_response(conn, reason) do
    response = %{
      status: "unhealthy",
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
      database: "disconnected",
      service: "riva_ash_api",
      error: "Database connection failed: #{inspect(reason)}"
    }

    conn
    |> put_status(:service_unavailable)
    |> json(response)
  end

  @spec send_health_response(Conn.t(), atom(), String.t(), String.t()) :: Conn.t()
  defp send_health_response(conn, status, health_status, db_status) do
    response = %{
      status: health_status,
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
      database: db_status,
      service: "riva_ash_api"
    }

    conn
    |> put_status(status)
    |> json(response)
  end

  # Utility functions

  defp format_duration(seconds) when seconds < 60 do
    "#{seconds}s"
  end

  defp format_duration(seconds) when seconds < 3_600 do
    minutes = div(seconds, 60)
    remaining_seconds = rem(seconds, 60)
    "#{minutes}m #{remaining_seconds}s"
  end

  defp format_duration(seconds) when seconds < 86_400 do
    hours = div(seconds, 3_600)
    remaining_minutes = div(rem(seconds, 3_600), 60)
    "#{hours}h #{remaining_minutes}m"
  end

  defp format_duration(seconds) do
    days = div(seconds, 86_400)
    remaining_hours = div(rem(seconds, 86_400), 3_600)
    "#{days}d #{remaining_hours}h"
  end

  defp format_bytes(bytes) when bytes < 1024 do
    "#{bytes}B"
  end

  defp format_bytes(bytes) when bytes < 1024 * 1024 do
    kb = bytes / 1024
    "#{:erlang.float_to_binary(kb, decimals: 2)}KB"
  end

  defp format_bytes(bytes) when bytes < 1024 * 1024 * 1024 do
    mb = bytes / (1024 * 1024)
    "#{:erlang.float_to_binary(mb, decimals: 2)}MB"
  end

  defp format_bytes(bytes) do
    gb = bytes / (1024 * 1024 * 1024)
    "#{:erlang.float_to_binary(gb, decimals: 2)}GB"
  end
end
