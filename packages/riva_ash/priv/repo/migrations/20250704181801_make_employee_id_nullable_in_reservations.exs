defmodule RivaAsh.Repo.Migrations.MakeEmployeeIdNullableInReservations do
  use Ecto.Migration

  def change do
    alter table(:reservations) do
      modify(:employee_id, :uuid, null: true)
    end
  end
end
