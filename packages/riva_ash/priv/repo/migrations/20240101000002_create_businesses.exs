defmodule RivaAsh.Repo.Migrations.CreateBusinesses do
  use Ecto.Migration

  def up do
    create table(:businesses, primary_key: false) do
      add :id, :uuid, primary_key: true, default: fragment("gen_random_uuid()")
      add :name, :text, null: false
      add :description, :text
      add :owner_id, references(:users, type: :uuid, on_delete: :restrict)
      add :archived_at, :utc_datetime_usec

      timestamps(type: :utc_datetime_usec)
    end

    create unique_index(:businesses, [:name])
    create index(:businesses, [:owner_id])
  end

  def down do
    drop table(:businesses)
  end
end
