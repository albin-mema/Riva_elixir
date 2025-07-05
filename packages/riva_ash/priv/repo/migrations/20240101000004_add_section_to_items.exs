defmodule RivaAsh.Repo.Migrations.AddSectionToItems do
  use Ecto.Migration

  def up do
    alter table(:items) do
      add :section_id, references(:sections, type: :uuid, on_delete: :nilify_all)
    end

    create index(:items, [:section_id])
  end

  def down do
    alter table(:items) do
      remove :section_id
    end
  end
end
