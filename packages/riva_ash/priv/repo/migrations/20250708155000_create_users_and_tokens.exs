defmodule RivaAsh.Repo.Migrations.CreateUsersAndTokens do
  use Ecto.Migration

  def change do
    create table(:users, primary_key: false) do
      add :id, :uuid, primary_key: true
      add :email, :citext, null: false
      add :hashed_password, :string, null: false
      add :name, :string
      add :role, :string, null: false, default: "user"
      
      timestamps(type: :utc_datetime_usec)
    end

    create unique_index(:users, [:email])

    create table(:user_tokens, primary_key: false) do
      add :id, :uuid, primary_key: true
      add :subject, :string, null: false
      add :token, :text, null: false
      add :purpose, :string, null: false
      add :expires_at, :utc_datetime, null: false
      add :extra_data, :map
      
      timestamps(updated_at: false, type: :utc_datetime_usec)
    end

    create index(:user_tokens, [:subject])
    create index(:user_tokens, [:token], unique: true)
  end
end
