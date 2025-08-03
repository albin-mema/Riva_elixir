defmodule RivaAsh.Repo.Migrations.AddSuperadminRole do
  use Ecto.Migration

  def change do
    # Add constraint to allow superadmin role in addition to existing admin and user roles
    execute(
      """
      ALTER TABLE users DROP CONSTRAINT IF EXISTS users_role_check;
      """,
      """
      ALTER TABLE users ADD CONSTRAINT users_role_check CHECK (role IN ('user', 'admin'));
      """
    )

    execute(
      """
      ALTER TABLE users ADD CONSTRAINT users_role_check CHECK (role IN ('user', 'admin', 'superadmin'));
      """,
      """
      ALTER TABLE users DROP CONSTRAINT users_role_check;
      ALTER TABLE users ADD CONSTRAINT users_role_check CHECK (role IN ('user', 'admin'));
      """
    )
  end
end
