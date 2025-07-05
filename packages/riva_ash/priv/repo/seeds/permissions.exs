# Seeds for default permissions
# This file creates the basic permissions that can be assigned to employees

alias RivaAsh.Resources.Permission
alias RivaAsh.Domain

# Define default permissions by category
permissions = [
  # Reservation permissions
  %{
    name: "can_create_reservations",
    description: "Can create new reservations for clients",
    category: :reservations,
    is_assignable: true
  },
  %{
    name: "can_view_all_reservations",
    description: "Can view all reservations, not just own",
    category: :reservations,
    is_assignable: true
  },
  %{
    name: "can_modify_reservations",
    description: "Can modify existing reservations",
    category: :reservations,
    is_assignable: true
  },
  %{
    name: "can_cancel_reservations",
    description: "Can cancel reservations",
    category: :reservations,
    is_assignable: true
  },

  # Employee management permissions
  %{
    name: "can_view_employees",
    description: "Can view employee information",
    category: :employees,
    is_assignable: true
  },
  %{
    name: "can_create_employees",
    description: "Can create new employee accounts",
    category: :employees,
    is_assignable: true
  },
  %{
    name: "can_modify_employees",
    description: "Can modify employee information",
    category: :employees,
    is_assignable: true
  },
  %{
    name: "can_give_permissions",
    description: "Can grant permissions to other employees",
    category: :employees,
    is_assignable: true
  },

  # Business management permissions
  %{
    name: "can_manage_business_settings",
    description: "Can modify business settings and configuration",
    category: :business,
    is_assignable: true
  },
  %{
    name: "can_manage_items",
    description: "Can create, modify, and delete items",
    category: :business,
    is_assignable: true
  },
  %{
    name: "can_manage_schedules",
    description: "Can manage item schedules and availability",
    category: :business,
    is_assignable: true
  },

  # Reporting permissions
  %{
    name: "can_view_reports",
    description: "Can access reporting and analytics",
    category: :reports,
    is_assignable: true
  },
  %{
    name: "can_export_data",
    description: "Can export data and reports",
    category: :reports,
    is_assignable: true
  },

  # System permissions
  %{
    name: "can_access_admin_panel",
    description: "Can access the admin panel interface",
    category: :system,
    is_assignable: true
  }
]

# Create permissions if they don't exist
Enum.each(permissions, fn permission_attrs ->
  case Permission
       |> Ash.Query.filter(expr(name == ^permission_attrs.name))
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
