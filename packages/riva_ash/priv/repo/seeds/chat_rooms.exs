# Chat Rooms Seeder
alias RivaAsh.Resources.{ChatRoom, Business}
alias RivaAsh.Accounts.User

case Ash.read(Business, domain: RivaAsh.Domain) do
  {:ok, businesses} when length(businesses) > 0 ->
    case Ash.read(User, domain: RivaAsh.Accounts) do
      {:ok, [user | _]} ->
        Enum.each(businesses, fn business ->
          rooms = [
            %{name: "General", description: "General discussion", room_type: "general"},
            %{name: "Support", description: "Customer support", room_type: "support"},
            %{name: "Team", description: "Internal team chat", room_type: "team"},
            %{name: "Reservations", description: "Booking discussions", room_type: "reservation"}
          ]

          Enum.each(rooms, fn room_data ->
            case ChatRoom
                 |> Ash.Changeset.for_create(:create, Map.put(room_data, :business_id, business.id), actor: user)
                 |> Ash.create() do
              {:ok, _room} -> IO.puts("✓ Created #{room_data.name} room for #{business.name}")
              {:error, _} -> IO.puts("✗ Failed to create #{room_data.name} room for #{business.name}")
            end
          end)
        end)

      {:ok, []} -> IO.puts("⚠ No users found")
      {:error, _} -> IO.puts("✗ Failed to load users")
    end
  {:ok, []} -> IO.puts("⚠ No businesses found")
  {:error, _} -> IO.puts("✗ Failed to load businesses")
end
