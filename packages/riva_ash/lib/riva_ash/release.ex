defmodule RivaAsh.Release do
  @moduledoc """
  Database release management for the Riva Ash application.

  This module provides functionality for running database migrations,
  rollbacks, and other release-related tasks in production environments
  where Mix may not be available as a dependency.

  It follows the functional core, imperative shell pattern with
  comprehensive error handling and logging.
  """

  @app :riva_ash
  @migrations_dir "priv/repo/migrations"

  @doc """
  Runs all pending database migrations.

  This function loads the application, gets all configured repositories,
  and runs migrations for each one. It provides detailed logging and
  error handling for production environments.

  ## Returns
  - `{:ok, [{:ok | :error, module()}]}` - Migration results for each repository
  - `{:error, term()}` - Application loading or configuration error

  ## Examples

      iex> RivaAsh.Release.migrate()
      {:ok, [{:ok, RivaAsh.Repo}]}
  """
  @spec migrate() :: {:ok, [{:ok | :error, module()}]} | {:error, term()}
  def migrate do
    with {:ok, _app} <- load_app(),
         {:ok, repos} <- get_repos(),
         :ok <- validate_repos(repos),
         results <- run_migrations(repos) do
      log_migration_results(results)
      {:ok, results}
    else
      {:error, reason} ->
        log_migration_error(reason)
        {:error, reason}
    end
  end

  @doc """
  Rolls back migrations to a specific version.

  This function rolls back migrations for a specific repository
  to the given version number.

  ## Arguments
  - `repo` - The repository module to rollback
  - `version` - The target version to rollback to

  ## Returns
  - `{:ok, term()}` - Rollback completed successfully
  - `{:error, term()}` - Rollback failed

  ## Examples

      iex> RivaAsh.Release.rollback(RivaAsh.Repo, 20230101120000)
      {:ok, %Ecto.Migrator.Migrations{...}}
  """
  @spec rollback(module(), integer()) :: {:ok, term()} | {:error, term()}
  def rollback(repo, version) do
    with {:ok, _app} <- load_app(),
         :ok <- validate_repo(repo),
         result <- perform_rollback(repo, version) do
      log_rollback_success(repo, version)
      {:ok, result}
    else
      {:error, reason} ->
        log_rollback_error(repo, version, reason)
        {:error, reason}
    end
  end

  @doc """
  Rolls back the most recent migration.

  This is a convenience function that rolls back the most recent
  migration for all repositories.

  ## Returns
  - `{:ok, [{:ok | :error, module()}]}` - Rollback results for each repository
  - `{:error, term()}` - Rollback failed

  ## Examples

      iex> RivaAsh.Release.rollback_last()
      {:ok, [{:ok, RivaAsh.Repo}]}
  """
  @spec rollback_last() :: {:ok, [{:ok | :error, module()}]} | {:error, term()}
  def rollback_last do
    with {:ok, _app} <- load_app(),
         {:ok, repos} <- get_repos(),
         :ok <- validate_repos(repos),
         results <- rollback_last_migrations(repos) do
      log_rollback_results(results)
      {:ok, results}
    else
      {:error, reason} ->
        log_rollback_error_all(reason)
        {:error, reason}
    end
  end

  @doc """
  Checks the migration status of all repositories.

  This function provides information about the current migration
  status including applied and pending migrations.

  ## Returns
  - `{:ok, [map()]}` - Status information for each repository
  - `{:error, term()}` - Status check failed

  ## Examples

      iex> RivaAsh.Release.status()
      {:ok, [%{repo: RivaAsh.Repo, applied: 10, pending: 2}]}
  """
  @spec status() :: {:ok, [map()]} | {:error, term()}
  def status do
    with {:ok, _app} <- load_app(),
         {:ok, repos} <- get_repos(),
         statuses <- check_migration_statuses(repos) do
      {:ok, statuses}
    else
      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Creates a new migration file.

  This function generates a new migration file with the given name
  and inserts it into the migrations directory.

  ## Arguments
  - `name` - The name of the migration
  - `opts` - Optional parameters (repo, change, etc.)

  ## Returns
  - `{:ok, String.t()}` - Path to the created migration file
  - `{:error, term()}` - File creation failed

  ## Examples

      iex> RivaAsh.Release.create_migration("add_users_table")
      {:ok, "priv/repo/migrations/20230101120000_add_users_table.exs"}
  """
  @spec create_migration(String.t(), keyword()) :: {:ok, String.t()} | {:error, term()}
  def create_migration(name, opts \\ []) do
    timestamp = generate_timestamp()
    repo = Keyword.get(opts, :repo, RivaAsh.Repo)

    migration_name = "#{timestamp}_#{name}.exs"
    migration_path = Path.join(@migrations_dir, migration_name)

    with :ok <- validate_migration_name(name),
         :ok <- ensure_migrations_dir(),
         content <- generate_migration_content(name, repo, opts),
         :ok <- File.write(migration_path, content) do
      log_migration_creation(migration_path)
      {:ok, migration_path}
    else
      {:error, reason} ->
        {:error, reason}
    end
  end

  # Private helper functions following single level of abstraction principle
  # Functional programming patterns: Use pattern matching and pipelines

  @spec validate_repos([module()]) :: :ok | {:error, term()}
  defp validate_repos([]), do: {:error, "No repositories configured"}
  defp validate_repos(repos) when is_list(repos), do: :ok

  @spec validate_repo(module()) :: :ok | {:error, term()}
  defp validate_repo(repo) when is_atom(repo), do: :ok
  defp validate_repo(_repo), do: {:error, "Invalid repository module"}

  @spec run_migrations([module()]) :: [{:ok | :error, module()}]
  defp run_migrations(repos) do
    Enum.map(repos, fn repo ->
      case Ecto.Migrator.with_repo(repo, &Ecto.Migrator.run(&1, :up, all: true)) do
        {status, _migrations, _apps} -> {status, repo}
        {:error, reason} -> {:error, {repo, reason}}
      end
    end)
  end

  @spec rollback_last_migrations([module()]) :: [{:ok | :error, module()}]
  defp rollback_last_migrations(repos) do
    Enum.map(repos, fn repo ->
      case Ecto.Migrator.with_repo(repo, &Ecto.Migrator.run(&1, :down, all: false)) do
        {status, _migrations, _apps} -> {status, repo}
        {:error, reason} -> {:error, {repo, reason}}
      end
    end)
  end

  @spec check_migration_statuses([module()]) :: [map()]
  defp check_migration_statuses(repos) do
    Enum.map(repos, fn repo ->
      case Ecto.Migrator.with_repo(repo, &Ecto.Migrator.migrations(&1)) do
        {:ok, migrations} ->
          applied = Enum.filter(migrations, &(&1.version <= get_last_version(repo)))
          pending = Enum.filter(migrations, &(&1.version > get_last_version(repo)))
          %{repo: repo, applied: length(applied), pending: length(pending)}

        {:error, reason} ->
          %{repo: repo, error: reason}
      end
    end)
  end

  @spec get_last_version(module()) :: integer()
  defp get_last_version(repo) do
    case Ecto.Migrator.with_repo(repo, &Ecto.Migrator.migrations(&1)) do
      {:ok, migrations} ->
        case Enum.map(migrations, & &1.version) |> Enum.sort() |> List.last() do
          nil -> 0
          version -> version
        end

      {:error, _} ->
        0
    end
  end

  @spec perform_rollback(module(), integer()) :: term()
  defp perform_rollback(repo, version) do
    Ecto.Migrator.with_repo(repo, &Ecto.Migrator.run(&1, :down, to: version))
  end

  @spec generate_timestamp() :: String.t()
  defp generate_timestamp do
    DateTime.utc_now() |> DateTime.to_unix() |> Integer.to_string()
  end

  @spec validate_migration_name(String.t()) :: :ok | {:error, term()}
  defp validate_migration_name(name) when is_binary(name) and byte_size(name) > 0, do: :ok
  defp validate_migration_name(_name), do: {:error, "Invalid migration name"}

  @spec ensure_migrations_dir() :: :ok | {:error, term()}
  defp ensure_migrations_dir do
    case File.exists?(@migrations_dir) do
      true -> :ok
      false -> File.mkdir_p(@migrations_dir)
    end
  end

  @spec generate_migration_content(String.t(), module(), keyword()) :: String.t()
  defp generate_migration_content(name, _repo, _opts) do
    """
    defmodule #{String.capitalize(name)} do
      use Ecto.Migration

      def change do
        # Add your migration changes here
      end
    end
    """
  end

  # Logging functions

  @spec log_migration_results([{:ok | :error, module()}]) :: :ok
  defp log_migration_results(results) do
    successful = Enum.count(results, &match?({:ok, _}, &1))
    failed = Enum.count(results, &match?({:error, _}, &1))

    Logger.info("Migration completed: #{successful} successful, #{failed} failed")

    Enum.each(results, fn
      {:ok, repo} -> Logger.info("✓ #{repo} migrated successfully")
      {:error, {repo, reason}} -> Logger.error("✗ #{repo} migration failed: #{inspect(reason)}")
    end)
  end

  @spec log_migration_error(term()) :: :ok
  defp log_migration_error(reason) do
    Logger.error("Migration failed: #{inspect(reason)}")
  end

  @spec log_rollback_success(module(), integer()) :: :ok
  defp log_rollback_success(repo, version) do
    Logger.info("Rollback completed for #{repo} to version #{version}")
  end

  @spec log_rollback_error(module(), integer(), term()) :: :ok
  defp log_rollback_error(repo, version, reason) do
    Logger.error("Rollback failed for #{repo} to version #{version}: #{inspect(reason)}")
  end

  @spec log_rollback_results([{:ok | :error, module()}]) :: :ok
  defp log_rollback_results(results) do
    successful = Enum.count(results, &match?({:ok, _}, &1))
    failed = Enum.count(results, &match?({:error, _}, &1))

    Logger.info("Rollback completed: #{successful} successful, #{failed} failed")
  end

  @spec log_rollback_error_all(term()) :: :ok
  defp log_rollback_error_all(reason) do
    Logger.error("Rollback failed for all repositories: #{inspect(reason)}")
  end

  @spec log_migration_creation(String.t()) :: :ok
  defp log_migration_creation(path) do
    Logger.info("Created migration file: #{path}")
  end

  # Legacy function compatibility

  @spec get_repos() :: {:ok, [module()]} | {:error, :ecto_repos_not_configured}
  defp get_repos do
    case Application.fetch_env(@app, :ecto_repos) do
      {:ok, repos} -> {:ok, repos}
      :error -> {:error, :ecto_repos_not_configured}
    end
  end

  @spec load_app() :: {:ok, :loaded} | {:error, term()}
  defp load_app do
    case Application.load(@app) do
      :ok -> {:ok, :loaded}
      {:error, reason} -> {:error, reason}
    end
  end
end
