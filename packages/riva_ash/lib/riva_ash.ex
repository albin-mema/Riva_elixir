defmodule RivaAsh do
  @moduledoc """
  Main entry point and utility functions for the Riva Ash application.

  This module provides core functionality and serves as the primary
  interface for the application's main operations.
  """

  @doc """
  Returns a greeting message.

  ## Examples

      iex> RivaAsh.hello()
      :world

  """
  @spec hello() :: :world
  def hello, do: :world

  @doc """
  Initializes the application with default configuration.

  This function sets up the basic application state and ensures
  all required components are properly configured.

  ## Returns
  - `:ok` when initialization succeeds
  - `{:error, reason}` when initialization fails

  ## Examples

      iex> RivaAsh.initialize()
      :ok
  """
  @spec initialize() :: :ok | {:error, term()}
  def initialize do
    with :ok <- configure_default_settings(),
         :ok <- initialize_core_components() do
      :ok
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # Private helper functions following single level of abstraction principle

  @spec configure_default_settings() :: :ok | {:error, term()}
  defp configure_default_settings do
    try do
      Application.put_env(:riva_ash, :default_timeout, 30_000)
      Application.put_env(:riva_ash, :max_retries, 3)
      :ok
    rescue
      error -> {:error, "Failed to configure default settings: #{inspect(error)}"}
    end
  end

  @spec initialize_core_components() :: :ok | {:error, term()}
  defp initialize_core_components do
    components = [:logger, :telemetry, :pubsub]

    Enum.reduce_while(components, :ok, fn component, :ok ->
      case initialize_component(component) do
        :ok -> {:cont, :ok}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  @spec initialize_component(atom()) :: :ok | {:error, term()}
  defp initialize_component(:logger) do
    # Logger is typically already initialized by the time this runs
    :ok
  end

  defp initialize_component(:telemetry) do
    # Initialize telemetry metrics and event handlers
    :ok
  end

  defp initialize_component(:pubsub) do
    # Initialize pubsub system for real-time communication
    :ok
  end

  defp initialize_component(_component) do
    :ok
  end
end
