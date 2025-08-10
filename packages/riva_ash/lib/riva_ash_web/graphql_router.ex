# alias Absinthe.Plugin, as: Plugin
# alias Absinthe.Plug, as: Plug
# alias Absinthe.Middleware, as: Middleware
# alias Absinthe.Type, as: Type
# alias RivaAshWeb.GraphqlMiddleware, as: GraphqlMiddleware

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

  query do
    # Query fields are automatically generated from Ash resources
    # when using AshGraphql integration
  end

  mutation do
    # Mutation fields are automatically generated from Ash resources
    # when using AshGraphql integration
  end

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

  def plugins do
    [
      Absinthe.Plug.Introspection,
      Absinthe.Middleware.Dataloader
    ]
  end

  def middleware(_schema) do
    [
      Absinthe.Middleware.Dataloader,
      RivaAshWeb.GraphqlMiddleware.Authorization
    ]
  end
end
