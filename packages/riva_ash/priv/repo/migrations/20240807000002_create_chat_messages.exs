defmodule RivaAsh.Repo.Migrations.CreateChatMessages do
  @moduledoc """
  Creates the chat_messages table for storing chat messages.
  """

  use Ecto.Migration

  def up do
    create table(:chat_messages, primary_key: false) do
      add(:id, :binary_id, primary_key: true)
      add(:content, :text, null: false)
      add(:room_id, :binary_id, null: false)
      add(:sender_id, :binary_id, null: false)

      timestamps(type: :utc_datetime_usec)
    end

    create(index(:chat_messages, [:room_id]))
    create(index(:chat_messages, [:sender_id]))
    create(index(:chat_messages, [:inserted_at]))
    create(index(:chat_messages, [:room_id, :inserted_at]))

    # Add foreign key constraints
    alter table(:chat_messages) do
      modify(:room_id, references(:chat_rooms, type: :binary_id, on_delete: :delete_all))
      modify(:sender_id, references(:users, type: :binary_id, on_delete: :delete_all))
    end
  end

  def down do
    drop(table(:chat_messages))
  end
end
