defmodule RivaAsh.Repo.Migrations.AlterChatMessagesAddSenderCols do
  use Ecto.Migration

  def change do
    alter table(:chat_messages) do
      add :sender_user_id, references(:users, type: :binary_id, on_delete: :delete_all)
      add :sender_client_id, references(:clients, type: :binary_id, on_delete: :delete_all)
    end

    create index(:chat_messages, [:sender_user_id])
    create index(:chat_messages, [:sender_client_id])
  end
end

