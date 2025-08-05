defmodule RivaAshWeb.Schema do
  @moduledoc """
  GraphQL schema definition for the Riva Ash application.
  
  This module defines the GraphQL API interface using Absinthe and
  integrates with Ash for data access and business logic. It provides
  query and mutation operations for application resources.
  """

  use Absinthe.Schema
  use AshGraphql, domains: [RivaAsh.Domain]

  @type context :: map()
  @type info :: map()

  @doc """
  Root query type definition.
  
  Defines all available GraphQL query operations for retrieving
  data from the application. Queries are automatically generated
  from Ash resources through the AshGraphql integration.
  """
  query do
    # Query fields are automatically generated from Ash resources
    # when using AshGraphql integration
  end

  @doc """
  Root mutation type definition.
  
  Defines all available GraphQL mutation operations for creating,
  updating, and deleting data in the application. Mutations are
  automatically generated from Ash resources through the AshGraphql
  integration with proper authorization and validation.
  """
  mutation do
    # Mutation fields are automatically generated from Ash resources
    # when using AshGraphql integration
  end

  @doc """
  Error handling for GraphQL execution.
  
  Provides custom error formatting for GraphQL responses to ensure
  consistent error reporting across the API.
  """
  @spec handle_absinthe_error({atom(), any()}, context(), info()) :: map()
  def handle_absinthe_error({:error, reason}, _context, _info) do
    %{
      message: "GraphQL execution error",
      details: inspect(reason),
      code: "GRAPHQL_ERROR"
    }
  end

  def handle_absinthe_error(error, _context, _info) do
    %{
      message: "Unexpected error occurred",
      details: inspect(error),
      code: "INTERNAL_ERROR"
    }
  end

  @doc """
  Plugin configuration for Absinthe schema.
  
  Configures additional plugins and middleware for enhanced
  GraphQL functionality, including introspection and validation.
  """
  @spec plugins() :: [Absinthe.Plugin.t()]
  def plugins do
    [
      Absinthe.Plug.Introspection,
      Absinthe.Middleware.Dataloader
    ]
  end

  @doc """
  Middleware pipeline for GraphQL execution.
  
  Defines middleware that runs around each GraphQL execution
  to provide common functionality like authentication,
  authorization, and logging.
  """
  @spec middleware(Absinthe.Type.t()) :: [Absinthe.Middleware.t()]
  def middleware(_schema) do
    [
      Absinthe.Middleware.Dataloader,
      RivaAshWeb.GraphqlMiddleware.Authorization
    ]
  end
end
