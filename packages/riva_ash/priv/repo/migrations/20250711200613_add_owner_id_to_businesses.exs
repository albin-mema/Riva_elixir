defmodule RivaAsh.Repo.Migrations.AddOwnerIdToBusinesses do
  use Ecto.Migration

  def change do
    alter table(:businesses) do
      add :owner_id, references(:users, type: :uuid, on_delete: :restrict)
    end

    create index(:businesses, [:owner_id])
  end
end
