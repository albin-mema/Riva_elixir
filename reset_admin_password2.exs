# Script to reset admin password

# Start the application
Application.ensure_all_started(:riva_ash)

# Get the admin user
admin = RivaAsh.Accounts.User
       |> Ash.Query.filter(expr(email == "admin@example.com"))
       |> Ash.read_one!()

# Update the password
case RivaAsh.Accounts.User
     |> Ash.Changeset.for_update(:update, %{password: "admin123"}, actor: admin)
     |> Ash.update() do
  {:ok, _user} ->
    IO.puts("Successfully updated admin password to: admin123")
  {:error, error} ->
    IO.puts("Failed to update password: #{inspect(error)}")
end
