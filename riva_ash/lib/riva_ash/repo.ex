defmodule RivaAsh.Repo do
  use AshPostgres.Repo, otp_app: :riva_ash

  def installed_extensions do
    ["uuid-ossp", "citext", "ash-functions"]
  end

  def min_pg_version do
    %Version{major: 13, minor: 0, patch: 0}
  end
end
