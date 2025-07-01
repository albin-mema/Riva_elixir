defmodule RivaAsh.Repo.Migrations.CreateBusinesses do
  use Ecto.Migration

  def up do
    create table(:businesses, primary_key: false) do
      add :id, :uuid, primary_key: true, default: fragment("gen_random_uuid()")
      add :name, :string, null: false
      add :description, :text
      
      timestamps(type: :utc_datetime_usec)
    end

    create unique_index(:businesses, [:name])
  end

  def down do
    drop table(:businesses)
  end
end
