# Permissions Matrix

This document provides a comprehensive overview of the permission system in Reservo, detailing the access levels and capabilities for each role within the platform.

## Overview

Reservo implements a role-based permission system built on the Ash Framework with SimpleSat SAT solver integration. The system ensures that users have appropriate access levels based on their roles and responsibilities within the organization.

## Domain Roles

| Role | Description | Scope |
|------|-------------|-------|
| **Superadmin** | Full system access, can manage all businesses and users | System-wide |
| **Business Owner** | Full control over their own business | Business-specific |
| **Manager** | Business management with some restrictions | Business-specific |
| **Staff** | Day-to-day operations and customer interactions | Business-specific |
| **Client** | Can make reservations and view their own data | Business-specific |

## Permission Matrix

### Business Management Permissions

| Permission | Description | Superadmin | Business Owner | Manager | Staff | Client |
|------------|-------------|------------|----------------|---------|-------|--------|
| `can_manage_business_settings` | Modify business information and configuration | ✅ | ✅ | ❌ | ❌ | ❌ |
| `can_manage_items` | Create, edit, and delete business items | ✅ | ✅ | ✅ | ❌ | ❌ |
| `can_manage_layouts` | Manage physical layouts and spaces | ✅ | ✅ | ✅ | ❌ | ❌ |
| `can_manage_sections` | Organize business into logical sections | ✅ | ✅ | ✅ | ❌ | ❌ |
| `can_update_pricing` | Modify pricing information and rates | ✅ | ✅ | ✅ | ❌ | ❌ |
| `can_view_pricing` | View pricing information (read-only) | ✅ | ✅ | ✅ | ✅ | ❌ |

### Reservation Management Permissions

| Permission | Description | Superadmin | Business Owner | Manager | Staff | Client |
|------------|-------------|------------|----------------|---------|-------|--------|
| `manage_reservations` | Full reservation management | ✅ | ✅ | ✅ | ✅ | ❌ |
| `view_all_reservations` | View all reservations in the business | ✅ | ✅ | ✅ | ✅ | ❌ |
| `modify_reservations` | Edit existing reservations | ✅ | ✅ | ✅ | ✅ | ❌ |
| `cancel_reservations` | Cancel reservations | ✅ | ✅ | ✅ | ✅ | ❌ |

### Employee Management Permissions

| Permission | Description | Superadmin | Business Owner | Manager | Staff | Client |
|------------|-------------|------------|----------------|---------|-------|--------|
| `manage_employees` | Add, edit, and remove employees | ✅ | ✅ | ✅ | ❌ | ❌ |
| `can_view_employees` | View employee information | ✅ | ✅ | ✅ | ✅ | ❌ |
| `can_manage_employee_permissions` | Assign and modify employee permissions | ✅ | ✅ | ❌ | ❌ | ❌ |

### Client Management Permissions

| Permission | Description | Superadmin | Business Owner | Manager | Staff | Client |
|------------|-------------|------------|----------------|---------|-------|--------|
| `manage_clients` | Manage client information and profiles | ✅ | ✅ | ✅ | ✅ | ❌ |
| `can_view_clients` | View client data | ✅ | ✅ | ✅ | ✅ | ✅ |

### System Permissions

| Permission | Description | Superadmin | Business Owner | Manager | Staff | Client |
|------------|-------------|------------|----------------|---------|-------|--------|
| `can_view_audit_logs` | Access system audit trails | ✅ | ✅ | ❌ | ❌ | ❌ |
| `can_view_reports` | View business analytics and reports | ✅ | ✅ | ✅ | ❌ | ❌ |
| `can_system_admin` | System administration capabilities | ✅ | ❌ | ❌ | ❌ | ❌ |

## Permission Categories

### Core Permissions
- `manage:all` - Full system access
- `read:all` - Read access to all resources
- `write:all` - Write access to all resources

### Reservation-Specific Permissions
- `manage:reservations` - Full reservation management
- `view:all_reservations` - View all reservations
- `modify:reservations` - Modify existing reservations
- `cancel:reservations` - Cancel reservations

### Business Management Permissions
- `manage:business` - Full business management
- `can_manage_business_settings` - Business configuration
- `can_manage_items` - Item management
- `can_manage_layouts` - Layout management
- `can_manage_sections` - Section management
- `can_update_pricing` - Pricing management
- `can_view_pricing` - Pricing viewing

### Employee Management Permissions
- `manage:employees` - Full employee management
- `can_view_employees` - Employee viewing
- `can_manage_employee_permissions` - Permission management

### Client Management Permissions
- `manage:clients` - Full client management
- `can_view_clients` - Client viewing

### System Permissions
- `can_view_audit_logs` - Audit log access
- `can_view_reports` - Report access
- `can_system_admin` - System administration

## Auto-Admin Functionality

### Business Creation Auto-Admin
When a user creates their first business, they automatically receive Business Owner privileges for that business, including:

- Full business management capabilities
- All business-related permissions
- Ability to manage employees and their permissions
- Full access to reservations and client data
- Access to business reports and audit logs

### Permission Inheritance
- **Business Owners** inherit all business-specific permissions
- **Managers** inherit most business permissions except for employee management
- **Staff** have operational permissions but cannot modify business settings
- **Clients** have self-service permissions only

## Implementation Details

### Centralized Permission Constants
All permissions are defined in `RivaAsh.Permissions.Constants` to ensure:
- Type safety and consistency
- Easy maintenance and updates
- IDE support with autocomplete
- Refactoring capabilities

### Ash Policy Integration
The permission system integrates with Ash Framework using:
- SimpleSat SAT solver for efficient authorization
- Policy-based access control
- Support for complex permission combinations
- Real-time permission evaluation

### Permission Checking
```elixir
# Example permission checks
alias RivaAsh.Permissions.Constants

# Check if user has specific permission
has_permission?(user_id, Constants.can_update_pricing())

# Check permissions by category
get_permissions_by_category(:business)
```

## Security Considerations

### Principle of Least Privilege
- Users receive only the permissions necessary for their role
- Permissions are granted based on job requirements
- Regular permission reviews recommended

### Audit Trail
- All permission changes are logged
- Access to sensitive features requires additional verification
- Regular security audits recommended

### Best Practices
1. **Use Permission Constants**: Always import and use `RivaAsh.Permissions.Constants`
2. **Test Permissions**: Include comprehensive permission tests
3. **Document Changes**: Keep permission documentation updated
4. **Regular Reviews**: Conduct periodic permission audits
5. **Monitor Usage**: Monitor for unusual permission patterns

## Related Documentation

- [Permissions and Policies Documentation](./PERMISSIONS_AND_POLICIES.md) - Detailed implementation guide
- [Contributing Guidelines](./CONTRIBUTING.md) - How to contribute to permission system
- [Ash Framework Documentation](https://ash-hq.org) - Framework documentation

---

*This permissions matrix should be kept in sync with the actual implementation in `RivaAsh.Permissions.Constants` and related modules.*