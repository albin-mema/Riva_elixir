defmodule RivaAsh.DatabaseHealth do
  @moduledoc """
  Database health checking and connection retry logic.

  This module provides utilities for checking database connectivity
  and implementing retry logic during application startup.
  """

  require Logger

  @max_retries 30
  @retry_interval 2_000
  @connection_timeout 10_000

  @doc """
  Waits for the database to become available with retry logic.

  This function will attempt to connect to the database up to `@max_retries` times
  with a `@retry_interval` delay between attempts.

  Returns `:ok` when the database is available, or `{:error, reason}` if all retries are exhausted.
  """
  @spec wait_for_database() :: :ok | {:error, String.t()}
  def wait_for_database do
    Logger.info("Checking database connectivity...")
    wait_for_database(@max_retries)
  end

  @spec wait_for_database(non_neg_integer()) :: :ok | {:error, String.t()}
  defp wait_for_database(0) do
    error_msg = "Database connection failed after #{@max_retries} attempts"
    Logger.error(error_msg)
    {:error, error_msg}
  end

  defp wait_for_database(retries_left) when retries_left > 0 do
    case check_database_connection() do
      {:ok, :ok} ->
        Logger.info("Database connection successful!")
        :ok

      {:error, reason} ->
        attempts_made = @max_retries - retries_left + 1

        Logger.warning(
          "Database connection attempt #{attempts_made}/#{@max_retries} failed: #{inspect(reason)}. " <>
            "Retrying in #{@retry_interval}ms..."
        )

        Process.sleep(@retry_interval)
        wait_for_database(retries_left - 1)
    end
  end

  @doc """
  Checks if the database connection is working.

  Returns `:ok` if the connection is successful, or `{:error, reason}` otherwise.
  """
  @spec check_database_connection() :: :ok | {:error, any()}
  def check_database_connection do
    try do
      case RivaAsh.Repo.query("SELECT 1", [], timeout: @connection_timeout) do
        {:ok, _result} -> {:ok, :ok}
        {:error, error} -> {:error, error}
      end
    rescue
      exception ->
        {:error, exception}
    catch
      :exit, reason ->
        {:error, reason}
    end
  end

  @spec execute_query_with_timeout(String.t(), keyword()) :: :ok | {:error, any()}
  defp execute_query_with_timeout(query, opts) do
    try do
      case RivaAsh.Repo.query(query, opts, timeout: @connection_timeout) do
        {:ok, _result} -> {:ok, :ok}
        {:error, error} -> {:error, error}
      end
    rescue
      exception ->
        {:error, exception}
    catch
      :exit, reason ->
        {:error, reason}
    end
  end

  @doc """
  Checks if the database has the required extensions installed.

  Returns `:ok` if all extensions are available, or `{:error, missing_extensions}` otherwise.
  """
  @spec check_database_extensions() :: :ok | {:error, [String.t()]}
  def check_database_extensions do
    required_extensions = RivaAsh.Repo.installed_extensions()

    with {:ok, installed} <- get_installed_extensions(),
         missing = required_extensions -- installed,
         {:ok, _} <- validate_extensions(missing, required_extensions) do
      log_extension_success(required_extensions)
    else
      {:error, error} ->
        log_extension_error(error)
        {:error, error}

      error ->
        log_extension_error(error)
        {:error, error}
    end
  end

  @spec log_extension_success([String.t()]) :: :ok
  defp log_extension_success(required_extensions) do
    Logger.info(
      "All required database extensions are installed: #{inspect(required_extensions)}"
    )

    {:ok, :ok}
  end

  @spec log_extension_error(any()) :: :ok
  defp log_extension_error(error) do
    Logger.error("Failed to check database extensions: #{inspect(error)}")
    :ok
  end

  @spec validate_extensions([String.t()], [String.t()]) :: :ok | {:error, [String.t()]}
  defp validate_extensions([], _required), do: {:ok, :ok}

  defp validate_extensions(missing, _required) do
    Logger.error("Missing database extensions: #{inspect(missing)}")
    {:error, missing}
  end

  @spec get_installed_extensions() :: {:ok, [String.t()]} | {:error, any()}
  defp get_installed_extensions do
    query = "SELECT extname FROM pg_extension"

    case execute_query_with_timeout(query, []) do
      {:ok, :ok} ->
        case RivaAsh.Repo.query(query, [], timeout: @connection_timeout) do
          {:ok, %{rows: rows}} ->
            extensions = extract_extension_names(rows)
            {:ok, extensions}

          {:error, error} ->
            {:error, error}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  @spec extract_extension_names([[String.t()]]) :: [String.t()]
  defp extract_extension_names(rows) do
    Enum.map(rows, fn [name] -> name end)
  end

  @doc """
  Performs a comprehensive database health check.

  This includes:
  - Basic connectivity test
  - Extension availability check
  - PostgreSQL version check

  Returns `:ok` if all checks pass, or `{:error, reasons}` with a list of issues.
  """
  @spec comprehensive_health_check() :: :ok | {:error, [String.t()]}
  def comprehensive_health_check do
    Logger.info("Performing comprehensive database health check...")

    checks = [
      {"Database connectivity", &check_database_connection/0},
      {"Database extensions", &check_database_extensions/0},
      {"PostgreSQL version", &check_postgres_version/0}
    ]

    case run_health_checks(checks) do
      :ok ->
        Logger.info("All database health checks passed!")
        :ok

      {:error, errors} ->
        {:error, errors}
    end
  end

  @spec run_health_checks([{String.t(), (-> :ok | {:error, any()})}]) :: :ok | {:error, [String.t()]}
  defp run_health_checks(checks) do
    Enum.reduce_while(checks, [], fn {check_name, check_fn}, errors ->
      case check_fn.() do
        :ok ->
          {:cont, errors}

        {:error, error} ->
          {:cont, [error | errors]}
      end
    end)
    |> case do
      [] -> :ok
      errors -> {:error, Enum.reverse(errors)}
    end
  end

  @spec log_check_result(:ok | {:ok, any()} | {:error, any()}, String.t()) ::
        {:ok, :ok} | {:error, String.t()}
  defp log_check_result(result, check_name) do
    result
    |> case do
      :ok ->
        Logger.info("✓ #{check_name} check passed")
        {:ok, :ok}

      {:ok, _} ->
        Logger.info("✓ #{check_name} check passed")
        {:ok, :ok}

      {:error, reason} ->
        Logger.error("✗ #{check_name} check failed: #{inspect(reason)}")
        {:error, "#{check_name}: #{inspect(reason)}"}
    end
  end

  @spec check_postgres_version() :: :ok | {:error, String.t()}
  defp check_postgres_version do
    min_version = RivaAsh.Repo.min_pg_version()

    with {:ok, current_version} <- get_postgres_version(),
         {:ok, _} <- validate_postgres_version(current_version, min_version) do
      log_version_success(current_version, min_version)
    else
      {:error, error} -> {:error, "Failed to validate PostgreSQL version: #{inspect(error)}"}
      error -> {:error, "Failed to validate PostgreSQL version: #{inspect(error)}"}
    end
  end

  @spec log_version_success(Version.t(), Version.t()) :: :ok
  defp log_version_success(current_version, min_version) do
    Logger.info(
      "PostgreSQL version #{current_version} meets minimum requirement #{min_version}"
    )

    {:ok, :ok}
  end

  @spec validate_postgres_version(Version.t(), Version.t()) :: :ok | {:error, String.t()}
  defp validate_postgres_version(current_version, min_version) do
    if Version.compare(current_version, min_version) in [:gt, :eq] do
      {:ok, :ok}
    else
      {:error,
       "PostgreSQL version #{current_version} is below minimum requirement #{min_version}"}
    end
  end

  @spec get_postgres_version() :: {:ok, Version.t()} | {:error, any()}
  defp get_postgres_version do
    case execute_query_with_timeout("SELECT version()", []) do
      {:ok, :ok} ->
        case RivaAsh.Repo.query("SELECT version()", [], timeout: @connection_timeout) do
          {:ok, %{rows: [[version_string]]}} ->
            parse_postgres_version(version_string)

          {:error, error} ->
            {:error, error}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  @spec parse_postgres_version(String.t()) :: {:ok, Version.t()} | {:error, String.t()}
  defp parse_postgres_version(version_string) do
    # Extract version number from string like "PostgreSQL 15.4 on x86_64-pc-linux-gnu..."
    case Regex.run(~r/PostgreSQL (\d+)\.(\d+)(?:\.(\d+))?/, version_string) do
      [_, major, minor] ->
        Version.parse("#{major}.#{minor}.0")

      [_, major, minor, patch] ->
        Version.parse("#{major}.#{minor}.#{patch}")

      _ ->
        {:error, "Could not parse PostgreSQL version from: #{version_string}"}
    end
  end
end
