# RivaAsh Permission System

This document explains how to use the centralized permission system in RivaAsh, which is designed to work seamlessly with Ash's SAT solver for efficient authorization.

## Overview

The permission system consists of:

1. **Constants Module** (`RivaAsh.Permissions.Constants`) - Centralized permission definitions
2. **Policy Checks** (`RivaAsh.Policies.PermissionCheck`) - Ash policy integration
3. **Helper Module** (`RivaAsh.Permissions`) - Business logic functions
4. **Database Storage** - Permissions stored in the database with assignments

## Key Benefits

- **Type Safety**: No more hardcoded permission strings
- **SAT Solver Integration**: Efficient authorization evaluation
- **Centralized Management**: All permissions defined in one place
- **IDE Support**: Autocomplete and refactoring support
- **Consistency**: Uniform naming and validation

## Usage Examples

### 1. Using Permission Constants

```elixir
# Import the constants
alias RivaAsh.Permissions.Constants

# Get specific permissions
Constants.can_update_pricing()
# => "can_update_pricing"

Constants.can_create_reservations()
# => "can_create_reservations"

# Get all permissions
Constants.all_permissions()
# => ["can_create_reservations", "can_view_all_reservations", ...]

# Get permissions by category
Constants.permissions_by_category()
# => %{reservations: [...], employees: [...], ...}
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

### 4. Available Permission Checks

The `RivaAsh.Policies.PermissionCheck` module provides convenience functions for all permissions:

```elixir
# Reservation permissions
PermissionCheck.can_create_reservations()
PermissionCheck.can_view_all_reservations()
PermissionCheck.can_modify_reservations()
PermissionCheck.can_cancel_reservations()

# Employee permissions
PermissionCheck.can_view_employees()
PermissionCheck.can_create_employees()
PermissionCheck.can_modify_employees()
PermissionCheck.can_give_permissions()

# Business permissions
PermissionCheck.can_manage_business_settings()
PermissionCheck.can_manage_items()
PermissionCheck.can_update_pricing()
PermissionCheck.can_view_pricing()

# And many more...
```

## How the SAT Solver Works

Ash's SAT solver evaluates authorization policies efficiently by:

1. **Converting policies to boolean expressions**
2. **Evaluating expressions in parallel when possible**
3. **Short-circuiting when results are determined**
4. **Caching results for repeated evaluations**

The permission checks integrate seamlessly with this system because they:
- Return boolean values
- Are deterministic
- Can be evaluated independently
- Support logical combinations (AND, OR, NOT)

## Adding New Permissions

1. **Add to Constants module**:
```elixir
# In RivaAsh.Permissions.Constants
@can_new_action "can_new_action"
def can_new_action, do: @can_new_action

# Add to all_permissions/0 and permissions_by_category/0
# Add to permission_metadata/0
```

2. **Add convenience function to PermissionCheck**:
```elixir
# In RivaAsh.Policies.PermissionCheck
def can_new_action, do: has_permission(:can_new_action)
```

3. **Add to helper module** (optional):
```elixir
# In RivaAsh.Permissions
def can_new_action?(employee_or_id) do
  has_permission?(employee_or_id, Constants.can_new_action())
end
```

4. **Update seeds** - The seeds automatically pick up new permissions from the constants.

## Best Practices

1. **Always use constants** instead of hardcoded strings
2. **Combine role-based and permission-based checks** when appropriate
3. **Use bypass for admin roles** to avoid unnecessary permission checks
4. **Group related permissions** logically in policies
5. **Test permission combinations** thoroughly
6. **Document permission purposes** in the metadata

## Migration from Role-Only Authorization

If you're migrating from role-only authorization:

1. Keep existing role checks as bypass conditions
2. Add permission checks for granular control
3. Gradually move logic from role-based to permission-based
4. Test thoroughly with different permission combinations

This approach ensures backward compatibility while enabling fine-grained authorization.
