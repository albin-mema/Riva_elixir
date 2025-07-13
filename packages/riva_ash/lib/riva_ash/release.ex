defmodule RivaAsh.Release do
  @moduledoc """
  Used for executing DB release tasks when run in production without Mix
  installed as a dependency.
  """
  @app :riva_ash
  import OK, only: [success: 1, failure: 1, ~>>: 2, for: 1, map_all: 2]

  def migrate do
    OK.for do
      _ <- load_app()
      repos <- repos()
      results <- OK.map_all(repos, fn repo ->
        Ecto.Migrator.with_repo(repo, &Ecto.Migrator.run(&1, :up, all: true))
        ~> fn {status, _, _} -> {status, repo} end
      end)
    after
      results
    end
  end

  def rollback(repo, version) do
    OK.for do
      _ <- load_app()
      result <- Ecto.Migrator.with_repo(repo, &Ecto.Migrator.run(&1, :down, to: version))
    after
      result
    end
  end

  defp repos do
    @app
    |> Application.fetch_env(:ecto_repos)
    |> case do
      {:ok, repos} -> success(repos)
      :error -> failure(:ecto_repos_not_configured)
    end
  end

  defp load_app do
    @app
    |> Application.load()
    |> case do
      :ok -> success(:loaded)
      {:error, reason} -> failure(reason)
    end
  end
end
