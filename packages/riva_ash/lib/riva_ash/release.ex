defmodule RivaAsh.Release do
  @moduledoc """
  Used for executing DB release tasks when run in production without Mix
  installed as a dependency.
  """
  @app :riva_ash
  alias RivaAsh.ErrorHelpers

  def migrate do
    with {:ok, _} <- load_app(),
         {:ok, repos} <- repos() do
      results = Enum.map(repos, fn repo ->
        case Ecto.Migrator.with_repo(repo, &Ecto.Migrator.run(&1, :up, all: true)) do
          {status, _, _} -> {status, repo}
        end
      end)
      ErrorHelpers.success(results)
    else
      {:error, reason} -> ErrorHelpers.failure(reason)
    end
  end

  def rollback(repo, version) do
    with {:ok, _} <- load_app(),
         result <- Ecto.Migrator.with_repo(repo, &Ecto.Migrator.run(&1, :down, to: version)) do
      ErrorHelpers.success(result)
    else
      {:error, reason} -> ErrorHelpers.failure(reason)
    end
  end

  defp repos do
    @app
    |> Application.fetch_env(:ecto_repos)
    |> case do
      {:ok, repos} -> ErrorHelpers.success(repos)
      :error -> ErrorHelpers.failure(:ecto_repos_not_configured)
    end
  end

  defp load_app do
    @app
    |> Application.load()
    |> case do
      :ok -> ErrorHelpers.success(:loaded)
      {:error, reason} -> ErrorHelpers.failure(reason)
    end
  end
end
