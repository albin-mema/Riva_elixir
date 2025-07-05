defmodule RivaAsh.Repo.Migrations.CreateSections do
  use Ecto.Migration

  def up do
    create table(:sections, primary_key: false) do
      add :id, :uuid, primary_key: true, default: fragment("gen_random_uuid()")
      add :name, :string, null: false
      add :description, :text
      add :business_id, references(:businesses, type: :uuid, on_delete: :delete_all), null: false
      
      timestamps(type: :utc_datetime_usec)
    end

    create unique_index(:sections, [:name, :business_id])
    create index(:sections, [:business_id])
  end

  def down do
    drop table(:sections)
  end
end
