defmodule RivaAsh.Repo do
  use AshPostgres.Repo, otp_app: :riva_ash

  # Support SQL Sandbox for testing
  def default_dynamic_repo do
    Process.get({__MODULE__, :dynamic_repo}) || __MODULE__
  end

  def installed_extensions do
    ["uuid-ossp", "citext", "ash-functions"]
  end

  def min_pg_version do
    %Version{major: 13, minor: 0, patch: 0}
  end
end
