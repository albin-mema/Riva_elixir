defmodule RivaAsh.Repo.Migrations.AddOwnerIdToBusinesses do
  use Ecto.Migration

  def change do
    # Conditionally add owner_id to businesses if it doesn't already exist
    execute("""
    DO $$
    BEGIN
      IF NOT EXISTS (
        SELECT 1
        FROM information_schema.columns
        WHERE table_name = 'businesses'
          AND column_name = 'owner_id'
      ) THEN
        ALTER TABLE businesses
          ADD COLUMN owner_id uuid
          REFERENCES users(id)
          ON DELETE RESTRICT;
      END IF;
    END;
    $$;
    """)

    # Ensure the index exists (safe to run even if column pre-existed)
    execute("""
    DO $$
    BEGIN
      IF NOT EXISTS (
        SELECT 1 FROM pg_class c
        JOIN pg_namespace n ON n.oid = c.relnamespace
        WHERE c.relkind = 'i'
          AND c.relname = 'businesses_owner_id_index'
          AND n.nspname = 'public'
      ) THEN
        CREATE INDEX businesses_owner_id_index ON businesses (owner_id);
      END IF;
    END;
    $$;
    """)
  end
end
