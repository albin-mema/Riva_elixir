# Permissions and Policies Documentation

This document provides comprehensive documentation for Reservo's permission system, including centralized permission constants, auto-admin functionality, and policy matrices aligned with domain roles.

## Table of Contents

1. [Overview](#overview)
2. [Centralized Permission Constants](#centralized-permission-constants)
3. [Auto-Admin on First Business Creation](#auto-admin-on-first-business-creation)
4. [Domain Roles and Policy Matrices](#domain-roles-and-policy-matrices)
5. [Ash Policy Integration](#ash-policy-integration)
6. [Permission Usage Examples](#permission-usage-examples)
7. [Testing Permissions](#testing-permissions)

## Overview

Reservo implements a sophisticated permission system built on top of the Ash Framework with SimpleSat SAT solver integration. The system provides:

- **Centralized Permission Management**: All permissions defined in one location
- **Type Safety**: No hardcoded permission strings
- **Efficient Authorization**: SAT solver for complex permission combinations
- **Domain-Driven Design**: Permissions aligned with business domains
- **Auto-Admin**: Automatic admin privileges for business creators

## Centralized Permission Constants

All permissions are defined in `RivaAsh.Permissions.Constants` and integrated with Ash policies using the SimpleSat SAT solver for efficient permission resolution.

### Permission Constants Module

```elixir
defmodule RivaAsh.Permissions.Constants do
  @moduledoc """
  Centralized permission constants used across Ash policies and SimpleSat authorization.
  """

  # Core permissions
  @manage_all "manage:all"
  @read_all "read:all"
  @write_all "write:all"

  # Domain-specific permissions
  @manage_reservations "manage:reservations"
  @view_all_reservations "view:all_reservations"
  @modify_reservations "modify:reservations"
  @cancel_reservations "cancel:reservations"

  # Business management permissions
  @manage_business "manage:business"
  @can_manage_business_settings "can_manage_business_settings"
  @can_manage_items "can_manage_items"
  @can_manage_layouts "can_manage_layouts"
  @can_manage_sections "can_manage_sections"
  @can_update_pricing "can_update_pricing"
  @can_view_pricing "can_view_pricing"

  # Employee management permissions
  @manage_employees "manage:employees"
  @can_view_employees "can_view_employees"
  @can_manage_employee_permissions "can_manage_employee_permissions"

  # Client management permissions
  @manage_clients "manage:clients"
  @can_view_clients "can_view_clients"

  # System permissions
  @can_view_audit_logs "can_view_audit_logs"
  @can_view_reports "can_view_reports"
  @can_system_admin "can_system_admin"

  @doc """
  Get all available permissions.
  """
  def all_permissions, do: [
    @manage_all, @read_all, @write_all,
    @manage_reservations, @view_all_reservations, @modify_reservations, @cancel_reservations,
    @manage_business, @can_manage_business_settings, @can_manage_items, @can_manage_layouts,
    @can_manage_sections, @can_update_pricing, @can_view_pricing,
    @manage_employees, @can_view_employees, @can_manage_employee_permissions,
    @manage_clients, @can_view_clients,
    @can_view_audit_logs, @can_view_reports, @can_system_admin
  ]

  @doc """
  Get permissions by category.
  """
  def permissions_by_category, do: %{
    :reservations => [
      @manage_reservations, @view_all_reservations, @modify_reservations, @cancel_reservations
    ],
    :business => [
      @manage_business, @can_manage_business_settings, @can_manage_items, 
      @can_manage_layouts, @can_manage_sections, @can_update_pricing, @can_view_pricing
    ],
    :employees => [
      @manage_employees, @can_view_employees, @can_manage_employee_permissions
    ],
    :clients => [
      @manage_clients, @can_view_clients
    ],
    :system => [
      @can_view_audit_logs, @can_view_reports, @can_system_admin
    ]
  }

  # Convenience functions for each permission
  def manage_all, do: @manage_all
  def read_all, do: @read_all
  def write_all, do: @write_all
  def manage_reservations, do: @manage_reservations
  def view_all_reservations, do: @view_all_reservations
  def modify_reservations, do: @modify_reservations
  def cancel_reservations, do: @cancel_reservations
  def manage_business, do: @manage_business
  def can_manage_business_settings, do: @can_manage_business_settings
  def can_manage_items, do: @can_manage_items
  def can_manage_layouts, do: @can_manage_layouts
  def can_manage_sections, do: @can_manage_sections
  def can_update_pricing, do: @can_update_pricing
  def can_view_pricing, do: @can_view_pricing
  def manage_employees, do: @manage_employees
  def can_view_employees, do: @can_view_employees
  def can_manage_employee_permissions, do: @can_manage_employee_permissions
  def manage_clients, do: @manage_clients
  def can_view_clients, do: @can_view_clients
  def can_view_audit_logs, do: @can_view_audit_logs
  def can_view_reports, do: @can_view_reports
  def can_system_admin, do: @can_system_admin
end
```

### Key Benefits

- **Type Safety**: No more hardcoded permission strings
- **SAT Solver Integration**: Efficient authorization evaluation
- **Centralized Management**: All permissions defined in one place
- **IDE Support**: Autocomplete and refactoring support
- **Consistency**: Uniform naming and validation

## Auto-Admin on First Business Creation

When a user creates their first business, they automatically receive administrative privileges for that business. This ensures immediate usability while maintaining security.

### Business Creation Process

```elixir
# When a user creates a business, they become the owner
defmodule RivaAsh.Resources.Business do
  policies do
    # Any authenticated user can create a business (they become the owner)
    policy action_type(:create) do
      authorize_if(actor_present())
    end

    # Business owners can update or delete their own businesses
    policy action_type([:update, :destroy]) do
      authorize_if(expr(owner_id == ^actor(:id)))
    end

    # Business owners have full access to their business data
    policy action_type([:read, :create, :update, :destroy]) do
      authorize_if(expr(owner_id == ^actor(:id)))
    end
  end
end
```

### Auto-Admin Privileges

Business owners automatically receive these permissions:

- **Full Business Management**: Can modify all business settings
- **Item Management**: Can create, edit, and delete business items
- **Employee Management**: Can manage employees and their permissions
- **Reservation Management**: Full access to all reservations
- **Financial Access**: Can view and update pricing information
- **System Access**: Can view reports and audit logs for their business

### Implementation Details

The auto-admin functionality is implemented through:

1. **Owner Relationship**: Each business has an `owner_id` field linking to the user
2. **Policy Bypass**: Business owners bypass most permission checks for their own businesses
3. **Automatic Permissions**: No manual permission assignment required
4. **Inheritance**: All business-related permissions are automatically granted

## Domain Roles and Policy Matrices

Reservo defines several domain roles, each with specific permission sets aligned with business functions.

### Domain Roles

| Role | Description | Scope |
|------|-------------|-------|
| **Superadmin** | Full system access, can manage all businesses and users | System-wide |
| **Business Owner** | Full control over their own business | Business-specific |
| **Manager** | Business management with some restrictions | Business-specific |
| **Staff** | Day-to-day operations and customer interactions | Business-specific |
| **Client** | Can make reservations and view their own data | Business-specific |

### Permission Matrix by Role

| Permission Category | Superadmin | Business Owner | Manager | Staff | Client |
|-------------------|------------|----------------|---------|-------|--------|
| **Business Management** | | | | | |
| Manage Business Settings | ✅ | ✅ | ❌ | ❌ | ❌ |
| Manage Items | ✅ | ✅ | ✅ | ❌ | ❌ |
| Manage Layouts | ✅ | ✅ | ✅ | ❌ | ❌ |
| Manage Sections | ✅ | ✅ | ✅ | ❌ | ❌ |
| Update Pricing | ✅ | ✅ | ✅ | ❌ | ❌ |
| View Pricing | ✅ | ✅ | ✅ | ✅ | ❌ |
| **Reservation Management** | | | | | |
| Manage All Reservations | ✅ | ✅ | ✅ | ✅ | ❌ |
| View All Reservations | ✅ | ✅ | ✅ | ✅ | ❌ |
| Modify Reservations | ✅ | ✅ | ✅ | ✅ | ❌ |
| Cancel Reservations | ✅ | ✅ | ✅ | ✅ | ❌ |
| **Employee Management** | | | | | |
| Manage Employees | ✅ | ✅ | ✅ | ❌ | ❌ |
| View Employees | ✅ | ✅ | ✅ | ✅ | ❌ |
| Manage Employee Permissions | ✅ | ✅ | ❌ | ❌ | ❌ |
| **Client Management** | | | | | |
| Manage Clients | ✅ | ✅ | ✅ | ✅ | ❌ |
| View Clients | ✅ | ✅ | ✅ | ✅ | ✅ |
| **System Access** | | | | | |
| View Audit Logs | ✅ | ✅ | ❌ | ❌ | ❌ |
| View Reports | ✅ | ✅ | ✅ | ❌ | ❌ |
| System Administration | ✅ | ❌ | ❌ | ❌ | ❌ |

### Permission Categories

#### 1. Business Management Permissions
- **`can_manage_business_settings`**: Modify business information and configuration
- **`can_manage_items`**: Create, edit, and delete business items
- **`can_manage_layouts`**: Manage physical layouts and spaces
- **`can_manage_sections`**: Organize business into logical sections
- **`can_update_pricing`**: Modify pricing information and rates
- **`can_view_pricing`**: View pricing information (read-only)

#### 2. Reservation Management Permissions
- **`manage_reservations`**: Full reservation management
- **`view_all_reservations`**: View all reservations in the business
- **`modify_reservations`**: Edit existing reservations
- **`cancel_reservations`**: Cancel reservations

#### 3. Employee Management Permissions
- **`manage_employees`**: Add, edit, and remove employees
- **`can_view_employees`**: View employee information
- **`can_manage_employee_permissions`**: Assign and modify employee permissions

#### 4. Client Management Permissions
- **`manage_clients`**: Manage client information and profiles
- **`can_view_clients`**: View client data

#### 5. System Permissions
- **`can_view_audit_logs`**: Access system audit trails
- **`can_view_reports`**: View business analytics and reports
- **`can_system_admin`**: System administration capabilities

## Ash Policy Integration

The permission system integrates seamlessly with Ash Framework policies using the SimpleSat SAT solver for efficient authorization.

### Policy Example

```elixir
defmodule RivaAsh.Policies.BusinessPolicy do
  use Ash.Policy

  policies do
    # Superadmins can do everything
    bypass actor_attribute_equals(:role, :superadmin) do
      authorize_if(always())
    end

    # Business owners have full access to their businesses
    policy action_type([:read, :create, :update, :destroy]) do
      authorize_if(expr(owner_id == ^actor(:id)))
    end

    # Managers can manage business settings
    policy action_type([:update]) do
      authorize_if(actor_attribute_equals(:role, :manager))
      authorize_if(has_permission(@can_manage_business_settings, :strict))
    end

    # Staff can view pricing but not modify it
    policy action_type(:read) do
      authorize_if(actor_attribute_equals(:role, :staff))
      authorize_if(has_permission(@can_view_pricing, :strict))
    end
  end
end
```

### Permission Check Helper

```elixir
defmodule RivaAsh.Policies.PermissionCheck do
  @moduledoc """
  Helper functions for permission checks in Ash policies.
  """

  # Import permission constants
  alias RivaAsh.Permissions.Constants

  # Reservation permissions
  def can_create_reservations, do: Constants.manage_reservations()
  def can_view_all_reservations, do: Constants.view_all_reservations()
  def can_modify_reservations, do: Constants.modify_reservations()
  def can_cancel_reservations, do: Constants.cancel_reservations()

  # Business permissions
  def can_manage_business_settings, do: Constants.can_manage_business_settings()
  def can_manage_items, do: Constants.can_manage_items()
  def can_manage_layouts, do: Constants.can_manage_layouts()
  def can_manage_sections, do: Constants.can_manage_sections()
  def can_update_pricing, do: Constants.can_update_pricing()
  def can_view_pricing, do: Constants.can_view_pricing()

  # Employee permissions
  def can_manage_employees, do: Constants.manage_employees()
  def can_view_employees, do: Constants.can_view_employees()
  def can_manage_employee_permissions, do: Constants.can_manage_employee_permissions()

  # Client permissions
  def can_manage_clients, do: Constants.manage_clients()
  def can_view_clients, do: Constants.can_view_clients()

  # System permissions
  def can_view_audit_logs, do: Constants.can_view_audit_logs()
  def can_view_reports, do: Constants.can_view_reports()
  def can_system_admin, do: Constants.can_system_admin()
end
```

## Permission Usage Examples

### 1. Using Permission Constants

```elixir
# Import the constants
alias RivaAsh.Permissions.Constants

# Get specific permissions
Constants.can_update_pricing()
# => "can_update_pricing"

Constants.can_create_reservations()
# => "manage:reservations"

# Get all permissions
Constants.all_permissions()
# => ["manage:all", "read:all", "write:all", ...]

# Get permissions by category
Constants.permissions_by_category()
# => %{reservations: [...], business: [...], ...}
```

### 2. Using in Ash Policies

```elixir
defmodule MyApp.Resources.SomeResource do
  use Ash.Resource,
    authorizers: [Ash.Policy.Authorizer]

  policies do
    # Admins can do everything
    bypass actor_attribute_equals(:role, :admin) do
      authorize_if(always())
    end

    # Permission-based authorization
    policy action_type(:update) do
      authorize_if(RivaAsh.Policies.PermissionCheck.can_update_pricing())
    end

    # Combining permissions with ownership
    policy action_type(:read) do
      # Either have view_all permission OR own the record
      authorize_if(RivaAsh.Policies.PermissionCheck.can_view_all_reservations())
      authorize_if(relating_to_actor(:employee_id))
    end

    # Multiple permission options
    policy action_type(:read) do
      authorize_if(RivaAsh.Policies.PermissionCheck.can_view_pricing())
      authorize_if(RivaAsh.Policies.PermissionCheck.can_update_pricing())
    end
  end
end
```

### 3. Using Helper Functions

```elixir
# Check permissions programmatically
employee = get_employee()

# Using the helper module
RivaAsh.Permissions.has_permission?(employee, "can_update_pricing")
# => true/false

# Using convenience functions
RivaAsh.Permissions.can_update_pricing?(employee)
# => true/false

RivaAsh.Permissions.can_create_reservations?(employee.id)
# => true/false

# Grant permissions
RivaAsh.Permissions.grant_permission(
  manager_id,
  employee_id,
  Constants.can_update_pricing()
)
```

### 4. Business Creation with Auto-Admin

```elixir
# When creating a business, the creator automatically becomes admin
{:ok, business} = RivaAsh.Resources.Business.create(%{
  name: "My Business",
  description: "A great business",
  owner_id: current_user.id  # Creator becomes owner
}, domain: RivaAsh.Domain)

# The owner now has all business permissions automatically
# No additional permission assignment needed
```

## Testing Permissions

### Property-Based Testing

```elixir
property "permission checks work correctly" do
  check all permission <- member_of(RivaAsh.Permissions.Constants.all_permissions()),
            role <- member_of([:admin, :manager, :staff, :client]),
            max_runs: 200 do

    user = create_user!(%{role: role})
    business = create_business!(user)

    expected = case {role, permission} do
      {:admin, _} -> true
      {:manager, perm} when perm in @manager_permissions -> true
      {:staff, perm} when perm in @staff_permissions -> true
      {:client, perm} when perm in @client_permissions -> true
      _ -> false
    end

    assert RivaAsh.Permissions.has_permission?(user.id, permission) == expected
  end
end
```

### Policy Testing

```elixir
defmodule RivaAsh.PoliciesTest do
  use ExUnit.Case

  test "business owners can manage their businesses" do
    owner = create_user_with_role(:owner)
    business = create_business(owner)
    
    assert RivaAsh.Permissions.has_permission?(
      owner.id, 
      RivaAsh.Permissions.Constants.can_manage_business_settings()
    )
  end

  test "non-owners cannot manage others' businesses" do
    owner = create_user_with_role(:owner)
    other_user = create_user_with_role(:staff)
    business = create_business(owner)
    
    refute RivaAsh.Permissions.has_permission?(
      other_user.id, 
      RivaAsh.Permissions.Constants.can_manage_business_settings()
    )
  end
end
```

### Integration Testing

```elixir
defmodule RivaAshWeb.BusinessControllerTest do
  use RivaAshWeb.ConnCase

  test "business owner can update business settings" do
    owner = create_user_with_role(:owner)
    business = create_business(owner)
    
    conn = build_conn()
    |> authenticate_user(owner)
    |> put("/api/businesses/#{business.id}", %{name: "Updated Name"})
    
    assert json_response(conn, 200)["data"]["name"] == "Updated Name"
  end

  test "non-owner cannot update business settings" do
    owner = create_user_with_role(:owner)
    other_user = create_user_with_role(:staff)
    business = create_business(owner)
    
    conn = build_conn()
    |> authenticate_user(other_user)
    |> put("/api/businesses/#{business.id}", %{name: "Updated Name"})
    
    assert json_response(conn, 403)["error"] == "Insufficient permissions"
  end
end
```

## Best Practices

1. **Always Use Constants**: Never hardcode permission strings
2. **Principle of Least Privilege**: Grant only necessary permissions
3. **Regular Audits**: Review permissions periodically
4. **Clear Naming**: Use descriptive permission names
5. **Documentation**: Keep permission documentation up to date
6. **Testing**: Test all permission scenarios thoroughly
7. **Auto-Admin**: Leverage auto-admin for business creators
8. **Domain Alignment**: Keep permissions aligned with business domains

## Security Considerations

- **Never bypass authorization** except for legitimate superadmin access
- **Validate all permission checks** at both resource and controller levels
- **Use secure defaults** - deny access unless explicitly granted
- **Monitor permission usage** for suspicious patterns
- **Implement proper logging** for all permission decisions
- **Regular security reviews** of the permission system

---

*This documentation should be kept in sync with the actual permission implementation in `RivaAsh.Permissions.Constants` and related modules.*