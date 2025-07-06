defmodule RivaAsh.Repo.Migrations.AddClientVerificationFields do
  use Ecto.Migration

  def change do
    alter table(:clients) do
      # Add new fields for email verification
      add :email_verified, :boolean, null: false, default: false
      add :verification_token, :string

      # Add index for verification token for faster lookups
      create index(:clients, [:verification_token])

      # Add index for email_verified to speed up queries for verified clients
      create index(:clients, [:email_verified])

      # Add index for is_registered to speed up queries for registered clients
      create index(:clients, [:is_registered])
    end

    # Add constraint to ensure email is present for registered clients
    execute """
    ALTER TABLE clients
    ADD CONSTRAINT email_required_for_registered
    CHECK (NOT is_registered OR (email IS NOT NULL AND email != ''))
    """

    # Add constraint to ensure verification token is present for unverified emails
    execute """
    ALTER TABLE clients
    ADD CONSTRAINT token_required_for_unverified
    CHECK (email_verified OR verification_token IS NOT NULL OR email IS NULL)
    """
  end

  def down do
    # Remove constraints first
    execute "ALTER TABLE clients DROP CONSTRAINT IF EXISTS email_required_for_registered"
    execute "ALTER TABLE clients DROP CONSTRAINT IF EXISTS token_required_for_unverified"

    # Drop indexes first
    drop index(:clients, [:verification_token])
    drop index(:clients, [:email_verified])
    drop index(:clients, [:is_registered])

    alter table(:clients) do
      # Drop columns
      remove :email_verified
      remove :verification_token
      remove :is_registered
    end
  end
end
