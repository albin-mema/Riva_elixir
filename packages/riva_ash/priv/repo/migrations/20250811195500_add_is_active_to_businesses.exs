defmodule RivaAsh.Repo.Migrations.AddIsActiveToBusinesses do
  use Ecto.Migration

  def up do
    alter table(:businesses) do
      add(:is_active, :boolean, null: false, default: true)
    end

    execute("UPDATE businesses SET is_active = TRUE WHERE archived_at IS NULL")
  end

  def down do
    alter table(:businesses) do
      remove(:is_active)
    end
  end
end

