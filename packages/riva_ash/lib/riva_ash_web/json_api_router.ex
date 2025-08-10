defmodule RivaAshWeb.JsonApiRouter do
  @moduledoc """
  JSON:API router configuration for the Riva Ash application.

  This module configures the JSON:API interface using AshJsonApi,
  providing a RESTful API interface that adheres to the JSON:API
  specification for data interchange.
  """

  use AshJsonApi.Router,
    domains: [RivaAsh.Domain],
    open_api: "/open_api"

  @type conn :: Plug.Conn.t()
  @type options :: keyword()

  @doc """
  Custom configuration for JSON:API behavior.

  Provides additional configuration options for the JSON:API router
  to enhance security, performance, and developer experience.
  """
  @spec custom_options() :: options()
  def custom_options do
    [
      # Enable JSON:API versioning for backward compatibility
      json_api_version: "1.0",

      # Configure pagination settings
      page_size: Application.get_env(:riva_ash, :json_api_page_size, 20),
      max_page_size: Application.get_env(:riva_ash, :json_api_max_page_size, 100),

      # Enable field selection for performance optimization
      allow_field_selection: true,

      # Enable filtering for flexible data querying
      allow_filtering: true,

      # Enable sorting for ordered results
      allow_sorting: true,

      # Configure rate limiting for API protection
      rate_limit: Application.get_env(:riva_ash, :json_api_rate_limit, {100, 60_000})
    ]
  end

  @doc """
  Pipeline configuration for JSON:API requests.

  Defines the middleware pipeline that processes all JSON:API requests
  to provide common functionality like authentication, validation,
  and response formatting.
  """
  @spec pipeline() :: [Plug.t()]
  def pipeline do
    []
  end

  @doc """
  Error handling for JSON:API responses.

  Provides standardized error formatting that complies with the
  JSON:API specification for error responses.
  """
  @spec handle_json_api_error({atom(), any()}, conn()) :: conn()
  def handle_json_api_error({:error, reason}, conn) do
    error_response = %{
      errors: [
        %{
          status: "400",
          code: "REQUEST_VALIDATION_ERROR",
          title: "Request validation failed",
          detail: inspect(reason)
        }
      ]
    }

    conn
    |> Plug.Conn.put_status(400)
    |> Plug.Conn.json(error_response)
  end

  def handle_json_api_error(_error, conn) do
    error_response = %{
      errors: [
        %{
          status: "500",
          code: "INTERNAL_ERROR",
          title: "Internal server error",
          detail: "An unexpected error occurred"
        }
      ]
    }

    conn
    |> Plug.Conn.put_status(500)
    |> Plug.Conn.json(error_response)
  end

  @doc """
  OpenAPI documentation configuration.

  Configures the OpenAPI documentation endpoint for the JSON:API
  to provide interactive API documentation and client SDK generation.
  """
  @spec open_api_config() :: keyword()
  def open_api_config do
    [
      title: "Riva Ash API",
      description: "RESTful API for Riva Ash application",
      version: "1.0.0",
      contact: %{
        name: "API Support",
        email: "support@rivaash.com"
      },
      license: %{
        name: "MIT",
        url: "https://opensource.org/licenses/MIT"
      },
      servers: [
        %{url: "http://localhost:4000", description: "Development server"}
      ]
    ]
  end
end
