# Seeds for default permissions
# This file creates the basic permissions that can be assigned to employees

alias RivaAsh.Resources.Permission
alias RivaAsh.Domain
alias RivaAsh.Permissions.Constants

# Generate permissions from constants with metadata
permissions =
  Constants.permission_metadata()
  |> Enum.map(fn {name, metadata} ->
    %{
      name: name,
      description: metadata.description,
      category: metadata.category,
      is_assignable: true
    }
  end)

# Create permissions if they don't exist
Enum.each(permissions, fn permission_attrs ->
  case Permission
       |> Ash.Query.filter(name: permission_attrs.name)
       |> Ash.read_one(domain: Domain) do
    {:ok, nil} ->
      # Permission doesn't exist, create it
      Permission
      |> Ash.Changeset.for_create(:create, permission_attrs)
      |> Ash.create!(domain: Domain)
      |> then(fn permission ->
        IO.puts("Created permission: #{permission.name}")
      end)

    {:ok, _existing} ->
      # Permission already exists, skip
      IO.puts("Permission already exists: #{permission_attrs.name}")

    {:error, error} ->
      IO.puts("Error checking permission #{permission_attrs.name}: #{inspect(error)}")
  end
end)

IO.puts("\nPermissions seeding completed!")
IO.puts("Total permissions defined: #{length(permissions)}")

# Display permission summary by category
permissions
|> Enum.group_by(& &1.category)
|> Enum.each(fn {category, perms} ->
  IO.puts("\n#{String.upcase(to_string(category))} (#{length(perms)} permissions):")
  Enum.each(perms, fn perm ->
    IO.puts("  - #{perm.name}: #{perm.description}")
  end)
end)

IO.puts("\nTo assign permissions to employees, use the RivaAsh.Permissions module:")
IO.puts("  RivaAsh.Permissions.grant_permission(granter_id, employee_id, \"permission_name\")")
