defmodule RivaAsh.Repo.Migrations.CreateChatParticipants do
  use Ecto.Migration

  def change do
    create table(:chat_participants, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :room_id, references(:chat_rooms, type: :binary_id, on_delete: :delete_all), null: false
      add :user_id, references(:users, type: :binary_id, on_delete: :delete_all)
      add :client_id, references(:clients, type: :binary_id, on_delete: :delete_all)
      add :role, :citext, null: false, default: "member"

      timestamps(type: :utc_datetime_usec)
    end

    create index(:chat_participants, [:room_id])
    create index(:chat_participants, [:user_id])
    create index(:chat_participants, [:client_id])
    create unique_index(:chat_participants, [:room_id, :user_id], where: "user_id IS NOT NULL")
    create unique_index(:chat_participants, [:room_id, :client_id], where: "client_id IS NOT NULL")
  end
end

