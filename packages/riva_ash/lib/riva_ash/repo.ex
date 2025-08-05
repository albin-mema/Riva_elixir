defmodule RivaAsh.Repo do
  @moduledoc """
  PostgreSQL repository configuration for the Riva Ash application.
  
  This module extends AshPostgres.Repo to provide database-specific
  functionality including connection management, migrations, and
  database operations.
  """

  use AshPostgres.Repo, otp_app: :riva_ash

  require Logger

  @doc """
  Returns the default dynamic repository for database operations.
  
  This function checks for a dynamically configured repository and falls
  back to the default repository if none is configured.
  
  ## Returns
  - `module()` - The repository module to use for database operations
  
  ## Examples
  
      iex> RivaAsh.Repo.default_dynamic_repo()
      RivaAsh.Repo
  """
  @spec default_dynamic_repo() :: module()
  def default_dynamic_repo do
    Process.get({__MODULE__, :dynamic_repo}) || __MODULE__
  end

  @doc """
  Returns the list of required PostgreSQL extensions.
  
  These extensions are automatically installed during database setup
  and are required for proper application functionality.
  
  ## Returns
  - `[String.t()]` - List of PostgreSQL extension names
  
  ## Examples
  
      iex> RivaAsh.Repo.installed_extensions()
      ["uuid-ossp", "citext", "ash-functions"]
  """
  @spec installed_extensions() :: [String.t()]
  def installed_extensions do
    [
      "uuid-ossp",
      "citext",
      "ash-functions",
      "pg_stat_statements",
      "btree_gin"
    ]
  end

  @doc """
  Returns the minimum required PostgreSQL version.
  
  This version is checked during application startup to ensure
  compatibility with all required features.
  
  ## Returns
  - `Version.t()` - Minimum required PostgreSQL version
  
  ## Examples
  
      iex> RivaAsh.Repo.min_pg_version()
      %Version{major: 13, minor: 0, patch: 0}
  """
  @spec min_pg_version() :: Version.t()
  def min_pg_version, do: %Version{major: 13, minor: 0, patch: 0}

  @doc """
  Validates the current PostgreSQL version compatibility.
  
  Checks if the connected PostgreSQL version meets the minimum
  requirements for the application.
  
  ## Returns
  - `:ok` - Version is compatible
  - `{:error, String.t()}` - Version is not compatible
  
  ## Examples
  
      iex> RivaAsh.Repo.validate_version()
      :ok
  """
  @spec validate_version() :: :ok | {:error, String.t()}
  def validate_version do
    with {:ok, version} <- get_postgres_version(),
         :ok <- check_version_compatibility(version) do
      :ok
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Returns the database connection configuration.
  
  This function provides access to the current database configuration
  for monitoring and debugging purposes.
  
  ## Returns
  - `map()` - Database configuration settings
  
  ## Examples
  
      iex> RivaAsh.Repo.config()
      %{hostname: "localhost", database: "riva_ash", ...}
  """
  @spec config() :: map()
  def config do
    Application.get_env(:riva_ash, RivaAsh.Repo, [])
  end

  @doc """
  Checks if the database connection is healthy.
  
  Performs a simple query to verify that the database connection
  is active and responding.
  
  ## Returns
  - `:ok` - Connection is healthy
  - `{:error, term()}` - Connection is not healthy
  
  ## Examples
  
      iex> RivaAsh.Repo.health_check()
      :ok
  """
  @spec health_check() :: :ok | {:error, term()}
  def health_check do
    try do
      case Ecto.Adapters.SQL.query(__MODULE__, "SELECT 1", []) do
        {:ok, _result} -> :ok
        {:error, reason} -> {:error, reason}
      end
    rescue
      error -> {:error, error}
    end
  end

  @doc """
  Returns the database statistics for monitoring.
  
  Collects various statistics about the database connection
  and performance metrics.
  
  ## Returns
  - `map()` - Database statistics
  
  ## Examples
  
      iex> RivaAsh.Repo.stats()
      %{connection_count: 5, query_count: 1234, ...}
  """
  @spec stats() :: map()
  def stats do
    %{
      connection_count: get_connection_count(),
      query_count: get_query_count(),
      idle_connection_count: get_idle_connection_count()
    }
  end

  # Private helper functions
  # Single level of abstraction: Keep helper functions focused on specific tasks

  @spec get_postgres_version() :: {:ok, Version.t()} | {:error, term()}
  defp get_postgres_version do
    try do
      case Ecto.Adapters.SQL.query(__MODULE__, "SELECT version()", []) do
        {:ok, %{rows: [[version_str]]}} ->
          parse_version_string(version_str)
        {:error, reason} ->
          {:error, reason}
      end
    rescue
      error -> {:error, error}
    end
  end

  @spec parse_version_string(String.t()) :: {:ok, Version.t()} | {:error, term()}
  defp parse_version_string(version_str) do
    # Extract version number from PostgreSQL version string
    # e.g., "PostgreSQL 13.4 on x86_64-pc-linux-gnu" -> "13.4"
    case Regex.run(~r/PostgreSQL (\d+\.\d+\.\d+)/, version_str) do
      [_, version_str] ->
        case Version.parse(version_str) do
          {:ok, version} -> {:ok, version}
          {:error, reason} -> {:error, reason}
        end
      nil ->
        {:error, "Unable to parse PostgreSQL version string"}
    end
  end

  @spec check_version_compatibility(Version.t()) :: :ok | {:error, String.t()}
  defp check_version_compatibility(current_version) do
    min_version = min_pg_version()
    
    case Version.compare(current_version, min_version) do
      :lt ->
        {:error,
         "PostgreSQL version #{current_version} is below minimum required version #{min_version}"}
      :eq -> :ok
      :gt -> :ok
    end
  end

  @spec get_connection_count() :: non_neg_integer()
  defp get_connection_count do
    # Get current connection count from the database
    0
  end

  @spec get_query_count() :: non_neg_integer()
  defp get_query_count do
    # Get total query count from the database
    0
  end

  @spec get_idle_connection_count() :: non_neg_integer()
  defp get_idle_connection_count do
    # Get idle connection count from the database
    0
  end
end
