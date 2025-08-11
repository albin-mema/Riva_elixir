# Superadmin Implementation Guide

## Overview

This document describes the implementation of a superadmin user type in the Riva Ash application. The superadmin role provides system-wide oversight capabilities while maintaining GDPR compliance and proper audit trails.

## Features Implemented

### 1. User Role Hierarchy

The application now supports three user roles:
- **superadmin**: Full system access for oversight and compliance
- **admin**: Organization-level administrative access
- **user**: Regular user access

### 2. Superadmin Capabilities

Superadmins can:
- Access the AshAdmin interface at `/admin` to view all system data
- View the superadmin dashboard at `/superadmin` for system overview
- Manage user roles and permissions
- Access all organization data across the platform for compliance purposes
- View audit trails and system activity logs

### 3. GDPR Compliance Features

- **Purpose Limitation**: Clear documentation of data access purposes
- **Audit Trails**: All superadmin access is logged via AshPaperTrail
- **Access Controls**: Strict authorization policies prevent unauthorized access
- **Transparency**: GDPR compliance notice displayed in admin interfaces

## Implementation Details

### Database Changes

1. **User Role Constraint**: Updated to allow `superadmin` role
   ```sql
   ALTER TABLE users ADD CONSTRAINT users_role_check 
   CHECK (role IN ('user', 'admin', 'superadmin'));
   ```

### Authorization Updates

1. **User Resource Policies**: Added superadmin bypass policies
2. **Authorization Helper**: Updated to recognize superadmin role
3. **Router Protection**: Added superadmin-only routes with proper middleware

### Key Files Modified

- `lib/riva_ash/resources/user.ex` - Added superadmin role and policies
- `lib/riva_ash/authorization.ex` - Updated permission checking
- `lib/riva_ash_web/router.ex` - Added superadmin routes and middleware
- `lib/riva_ash_web/controllers/auth_helpers.ex` - Added superadmin authorization
- `lib/riva_ash_web/ash_admin_config.ex` - Enhanced admin access control

### New Components

1. **Mix Task**: `mix create_superadmin` for creating superadmin users
2. **Dashboard**: Superadmin dashboard with system overview
3. **Middleware**: Superadmin authorization pipeline

## Usage Instructions

### Creating a Superadmin User

#### Option 1: Using Mix Task (Recommended)
```bash
cd packages/riva_ash
mix create_superadmin --email admin@yourcompany.com --name "System Admin" --password "SecurePassword123!"
```

#### Option 2: Interactive Creation
```bash
cd packages/riva_ash
mix create_superadmin
# Follow the prompts
```

#### Option 3: Manual Creation (IEx)
```elixir
iex -S mix

alias RivaAsh.Accounts.User
alias RivaAsh.Accounts

User 
|> Ash.Changeset.for_create(:register_with_password, %{
  email: "admin@yourcompany.com", 
  name: "System Admin", 
  password: "SecurePassword123!", 
  role: :superadmin
}) 
|> Ash.create(domain: Accounts, actor: %{role: :superadmin})
```

### Accessing Superadmin Features

1. **Sign In**: Navigate to `/sign-in` and use superadmin credentials
2. **Dashboard**: Access the superadmin dashboard at `/superadmin`
3. **Admin Interface**: Access the full admin interface at `/admin`

## Security Considerations

### Access Control
- Superadmin routes are protected by dedicated middleware
- All access requires authentication and role verification
- Failed access attempts are logged and redirected

### Audit Trail
- All superadmin actions are logged via AshPaperTrail
- Database queries include actor information for audit purposes
- Access patterns can be monitored for compliance

### GDPR Compliance
- Clear purpose statements for data access
- Audit trails for all data access
- User consent tracking integration
- Data minimization principles applied

## Monitoring and Compliance

### Audit Queries
```elixir
# View all superadmin actions
RivaAsh.Resources.Version
|> Ash.Query.filter(actor_id == ^superadmin_user_id)
|> Ash.read!(domain: RivaAsh.Domain)

# Monitor data access patterns
RivaAsh.Resources.Version
|> Ash.Query.filter(inserted_at > ^one_month_ago)
|> Ash.Query.filter(actor_role == :superadmin)
|> Ash.read!(domain: RivaAsh.Domain)
```

### Compliance Reports
The superadmin dashboard provides:
- System-wide data statistics
- Recent activity summaries
- GDPR consent status overview
- Quick access to compliance tools

## Best Practices

### Superadmin Account Management
1. **Limited Accounts**: Create only necessary superadmin accounts
2. **Strong Passwords**: Enforce strong password requirements
3. **Regular Review**: Periodically review superadmin access
4. **Activity Monitoring**: Monitor superadmin activity logs

### Data Access Guidelines
1. **Purpose-Driven**: Access data only for legitimate operational purposes
2. **Minimal Access**: Access only the data necessary for the task
3. **Documentation**: Document the reason for data access
4. **Time-Limited**: Limit access duration when possible

### Security Maintenance
1. **Regular Updates**: Keep the system updated
2. **Access Review**: Regularly review and audit access logs
3. **Policy Updates**: Update policies as regulations change
4. **Training**: Ensure superadmins understand compliance requirements

## Troubleshooting

### Common Issues

1. **Cannot Create Superadmin**
   - Ensure database migrations are run: `mix ecto.migrate`
   - Check that the role constraint allows 'superadmin'

2. **Access Denied to Admin Interface**
   - Verify user has superadmin role in database
   - Check authentication session is valid
   - Ensure proper middleware is applied

3. **Policy Errors**
   - Verify actor is properly set in requests
   - Check policy definitions in resources
   - Ensure bypass policies are correctly configured

### Debug Commands
```bash
# Check user role
iex -S mix
RivaAsh.Accounts.User |> Ash.get!("user_id", domain: RivaAsh.Accounts)

# Test authorization
RivaAshWeb.AuthHelpers.is_superadmin?(%{assigns: %{current_user: %{role: :superadmin}}})
```

## Future Enhancements

Potential improvements to consider:
1. **Multi-Factor Authentication**: Add MFA for superadmin accounts
2. **Session Management**: Enhanced session controls and timeouts
3. **Advanced Auditing**: More detailed audit trail features
4. **Compliance Automation**: Automated compliance report generation
5. **Role Delegation**: Temporary elevated access capabilities
