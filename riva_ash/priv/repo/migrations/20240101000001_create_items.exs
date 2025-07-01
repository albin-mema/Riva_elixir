defmodule RivaAsh.Repo.Migrations.CreateItems do
  use Ecto.Migration

  def up do
    create table(:items, primary_key: false) do
      add :id, :uuid, primary_key: true, default: fragment("gen_random_uuid()")
      add :name, :string, null: false
      
      timestamps(type: :utc_datetime_usec)
    end

    create unique_index(:items, [:name])
  end

  def down do
    drop table(:items)
  end
end