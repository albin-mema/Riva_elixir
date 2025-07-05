defmodule RivaAsh.Repo.Migrations.CreateClientsAndReservations do
  use Ecto.Migration

  def change do
    create table(:clients, primary_key: false) do
      add :id, :uuid, null: false, default: fragment("gen_random_uuid()"), primary_key: true
      add :name, :text, null: false
      add :email, :citext
      add :phone, :text
      add :is_registered, :boolean, null: false, default: false
      
      add :inserted_at, :utc_datetime_usec, null: false, default: fragment("(now() AT TIME ZONE 'utc')")
      add :updated_at, :utc_datetime_usec, null: false, default: fragment("(now() AT TIME ZONE 'utc')")
    end

    create unique_index(:clients, [:email], where: "email IS NOT NULL", name: "clients_email_index")

    create table(:reservations, primary_key: false) do
      add :id, :uuid, null: false, default: fragment("gen_random_uuid()"), primary_key: true
      add :reserved_from, :utc_datetime, null: false
      add :reserved_until, :utc_datetime, null: false
      add :status, :text, null: false, default: "pending"
      add :notes, :text
      
      add :client_id, references(:clients, type: :uuid, on_delete: :restrict), null: false
      add :item_id, references(:items, type: :uuid, on_delete: :restrict), null: false
      
      add :inserted_at, :utc_datetime_usec, null: false, default: fragment("(now() AT TIME ZONE 'utc')")
      add :updated_at, :utc_datetime_usec, null: false, default: fragment("(now() AT TIME ZONE 'utc')")
    end

    create index(:reservations, [:client_id])
    create index(:reservations, [:item_id])
    create index(:reservations, [:reserved_from, :reserved_until])
  end
end
