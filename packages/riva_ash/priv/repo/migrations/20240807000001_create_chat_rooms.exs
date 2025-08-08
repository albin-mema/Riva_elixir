defmodule RivaAsh.Repo.Migrations.CreateChatRooms do
  @moduledoc """
  Creates the chat_rooms table for multi-room chat functionality.
  """

  use Ecto.Migration

  def up do
    create table(:chat_rooms, primary_key: false) do
      add(:id, :binary_id, primary_key: true)
      add(:name, :string, null: false)
      add(:description, :text)
      add(:room_type, :string, default: "general", null: false)
      add(:is_active, :boolean, default: true, null: false)
      add(:business_id, :binary_id, null: false)
      add(:created_by_id, :binary_id, null: false)

      timestamps(type: :utc_datetime_usec)
    end

    create(index(:chat_rooms, [:business_id]))
    create(index(:chat_rooms, [:created_by_id]))
    create(index(:chat_rooms, [:is_active]))
    create(index(:chat_rooms, [:room_type]))
    create(unique_index(:chat_rooms, [:business_id, :name], name: :unique_room_name_per_business))

    # Add foreign key constraints
    alter table(:chat_rooms) do
      modify(:business_id, references(:businesses, type: :binary_id, on_delete: :delete_all))
      modify(:created_by_id, references(:users, type: :binary_id, on_delete: :delete_all))
    end
  end

  def down do
    drop(table(:chat_rooms))
  end
end
