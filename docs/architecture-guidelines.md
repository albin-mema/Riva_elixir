# Architecture Guidelines

## Project Overview

**Riva** is a comprehensive business management system built with Elixir/Phoenix and Ash Framework, featuring reservation management, employee permissions, and real-time capabilities. The project follows a packages-based architecture with all code organized under the `packages/` directory.

### Key Technologies
- **Backend**: Elixir 1.19+, Phoenix 1.7+, Ash Framework 3.5+
- **Database**: PostgreSQL with UUID primary keys
- **Frontend**: LiveView with React integration (live_react)
- **Testing**: ExUnit with property-based testing (StreamData)
- **Authentication**: AshAuthentication with role-based access
- **Authorization**: Ash Policies with SimpleSat SAT solver for permission management
- **UI**: LiveView + Tailwind CSS with Atomic Design patterns and canonical UI system

## Architecture Patterns

### 1. Ash Framework Patterns

**Resource Definition**: All business entities are Ash Resources with standardized extensions:
```elixir
use Ash.Resource,
  domain: RivaAsh.Domain,
  data_layer: AshPostgres.DataLayer,
  authorizers: [Ash.Policy.Authorizer],
  extensions: [
    AshJsonApi.Resource,
    AshGraphql.Resource,
    AshPaperTrail.Resource,    # Audit trails
    AshArchival.Resource,      # Soft delete
    AshAdmin.Resource
  ]
```

**Domain Organization**: Resources are grouped in `RivaAsh.Domain` with clear boundaries.

### 2. Complex Business Logic - Use Reactor

**CRITICAL**: For any complex business logic involving multiple resources or multi-step operations, use Reactor workflows instead of regular Ash actions.

**When to use Reactor**:
- Multi-resource creation/updates
- Complex validation chains
- Business workflows with compensation
- Operations requiring rollback capabilities

**Example**: Business setup, reservation creation with availability checks, employee onboarding.

**Location**: `lib/riva_ash/reactors/`

### 3. UI Component Architecture

**Canonical UI System**: The application uses a canonical UI component system:

- **UI Components** (`RivaAshWeb.Components.UI.*`): Single source of truth for design system primitives
  - `UI.Button`, `UI.Input`, `UI.Card`, `UI.Text`, etc.
  - Follow design system tokens and consistent styling
  - Comprehensive prop APIs with variants, sizes, and states

- **Compatibility Wrappers** (`RivaAshWeb.Components.Atoms.*`): Backward-compatible wrappers
  - Delegate to UI components while preserving legacy APIs
  - Map legacy sizes/variants to canonical equivalents
  - Will be removed after migration completion

- **Composed Components** (`RivaAshWeb.Components.Molecules.*`): Higher-level compositions
  - Use UI components internally while preserving composed APIs
  - Examples: `FormField`, `Card` with header/body/footer slots
  - Provide domain-specific functionality

**Component Import Patterns**:
```elixir
# ✅ Preferred for new code
alias RivaAshWeb.Components.UI.Button, as: UIButton
alias RivaAshWeb.Components.UI.Input, as: UIInput

# ✅ Acceptable during migration
import RivaAshWeb.Components.Atoms.Button  # Delegates to UI.Button

# ✅ For composed functionality
import RivaAshWeb.Components.Molecules.FormField
```

**Design System Integration**:
- All UI components use Tailwind CSS with design tokens
- Consistent spacing, typography, and color schemes
- Support for dark/light themes through CSS variables
- Accessibility-first approach with proper ARIA attributes

**Testing Strategy**:
- Focus testing on canonical UI components
- Property-based testing for component variants and states
- Compatibility wrapper tests ensure proper delegation
- Integration tests for composed molecule components

**Custom Components**: Always create reusable custom components instead of inline HTML.

**Flop Integration**: Use Flop library for ALL table functionality and pagination.

**Form Handling**: Use AshPhoenix.Form for all form operations with proper validation.

### 4. Permission System

**Centralized Permissions**: All permissions are defined in `RivaAsh.Permissions.Constants` and integrated with Ash policies using the SimpleSat SAT solver for efficient permission resolution:
```elixir
# Example permission constant definition
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
  @read_reservations "read:reservations"
  @create_reservations "create:reservations"
  
  # Business-specific permissions
  @manage_business "manage:business"
  @read_business "read:business"
  @update_business "update:business"
  
  @doc """
  Get all available permissions.
  """
  def all_permissions, do: [
    @manage_all, @read_all, @write_all,
    @manage_reservations, @read_reservations, @create_reservations,
    @manage_business, @read_business, @update_business
  ]
end
```

**Policy Integration**: Permissions work with Ash policies and SimpleSat SAT solver for efficient and scalable authorization:
```elixir
# Example policy using SimpleSat for permission checking
defmodule RivaAsh.Policies.BusinessPolicy do
  use Ash.Policy

  policies do
    # Allow business owners to manage their own businesses
    policy action_type(:read) do
      authorize_if always()
    end
    
    policy action_type(:update) do
      authorize_if relates_to_actor_via(:business_owners)
      authorize_if has_permission(@manage_business, :strict)
    end
    
    policy action_type(:destroy) do
      authorize_if has_permission(@manage_all, :strict)
    end
  end
end
```

**Never hardcode permission strings** - always use constants. The SimpleSat SAT solver efficiently resolves complex permission combinations.

## Project Structure

```
packages/
├── riva_ash/                 # Main Ash application
│   ├── lib/
│   │   ├── riva_ash/
│   │   │   ├── resources/    # Ash resources (Business, Item, etc.)
│   │   │   ├── reactors/     # Complex business logic workflows
│   │   │   ├── policies/     # Authorization policies
│   │   │   ├── permissions/  # Permission system
│   │   │   └── validations/  # Custom validations
│   │   └── riva_ash_web/
│   │       ├── components/   # UI components (atomic design)
│   │       ├── live/         # LiveView pages
│   │       └── controllers/  # Phoenix controllers
│   ├── test/                 # Test files
│   └── priv/                 # Migrations, seeds
└── test/                     # Shared test utilities
```

## Development Guidelines

### 1. Resource Development

**Standard Extensions**: Every resource must include:
- AshPaperTrail (comprehensive audit trails for change tracking and compliance)
- AshArchival (soft delete for data retention and audit purposes)
- Proper policies with admin bypass
- UUID primary keys
- Timestamps (inserted_at, updated_at)

**Audit and Retention Philosophy**:
- AshPaperTrail and AshArchival are implemented as audit/retention features, not document management systems
- All changes are tracked for compliance and regulatory requirements
- Soft delete operations maintain data integrity for audit purposes
- Archival focuses on retention policies and lifecycle management, not document storage

**Relationships**: Use proper Ash relationships with foreign key constraints.

### 2. Database Patterns

**Soft Delete**: Use AshArchival for all resources requiring deletion.

**Audit Trails**: AshPaperTrail tracks all changes.

**Grid Positioning**: Use row/column grid system instead of x,y coordinates for layouts.

### 3. Business Logic Patterns

**Platform**:
- Full-day billing only
- No weekend/weekday differentiation
- Constant pricing with business exceptions
- Row/column positioning for items

**Permission Hierarchy**:
- Admin: Full system access
- Business Owner: Full business access
- Employee: Permission-based access
- Client: Limited self-service access

## Common Patterns

### 1. Creating New Resources

1. Define in `lib/riva_ash/resources/`
2. Include all standard extensions
3. Add to domain
4. Create migration
5. Add policies with admin bypass
6. **MANDATORY**: Write comprehensive test suite:
   - Property-based tests for all actions (create, read, update, destroy)
   - Policy tests with various user roles and permissions
   - Validation tests with random invalid data
   - Relationship tests with associated resources
   - Archive/soft delete functionality tests

### 2. Adding Complex Workflows

1. Create Reactor in `lib/riva_ash/reactors/`
2. Define clear inputs/outputs
3. Include compensation logic
4. Add to resource as custom action if needed
5. **MANDATORY**: Comprehensive test suite:
   - Property-based tests with random valid input combinations
   - Error scenario tests with invalid inputs
   - Compensation logic tests (rollback scenarios)
   - Integration tests with all affected resources
   - Performance tests for complex workflows
   - Edge case tests with boundary conditions

## API Design Patterns

### 1. GraphQL and JSON:API Support

Reservo provides dual API support for maximum flexibility:

**GraphQL**:
- Available for complex queries and efficient data fetching
- Proper field selection and error handling
- Authorization enforced through Ash policies using SimpleSat SAT solver
- Ideal for frontend applications requiring specific data shapes

**JSON:API**:
- RESTful API following JSON:API specification for standardized resource-oriented communication
- Full support for filtering, sorting, pagination, and relationships
- Consistent HTTP status codes and error responses
- Ideal for third-party integrations and standardized API consumption

**Dual API Support**: Both GraphQL and JSON:API are wired in router/domain docs and expose the same Ash resources through different transport mechanisms, ensuring consistent authorization and business logic enforcement across all API layers.

### 2. Authorization Consistency

Both API layers enforce authorization through Ash policies. The web layer must pass the authenticated actor into the context for policy evaluation so policies run consistently across all transports.

## Runtime and Secrets (Production)

The following environment variables are required in production and are validated at runtime:

- DATABASE_URL (ecto://USER:PASS@HOST/DATABASE)
- SECRET_KEY_BASE (generate via: mix phx.gen.secret)
- AUTH_TOKEN_SECRET (used by AshAuthentication to sign tokens)
- PHX_HOST, PORT (endpoint configuration)
- POOL_SIZE (DB pool sizing)

Docker notes:
- docker-compose defaults MIX_ENV to dev to avoid accidental prod with weak defaults.
- SECRET_KEY_BASE and AUTH_TOKEN_SECRET must be provided explicitly when running in prod.
- A JSON health endpoint is exposed at GET /health, used by container health checks.

## Deployment Considerations

### 1. Environment Configuration
- Use environment variables for secrets
- Configure different environments properly
- Set up proper logging levels
- Configure database connections

### 2. Database Management
- Run migrations in order
- Use seeds for initial data
- Backup strategies for production
- Monitor database performance

## Troubleshooting Common Issues

### 1. Ash Policy Errors
- Check policy definitions in resources
- Verify actor is properly set
- Use policy breakdowns for debugging
- Ensure permissions exist in Constants

### 2. LiveView Issues
- Check socket connections
- Verify proper assigns usage
- Debug with LiveView debugger
- Test with different browsers

### 3. Database Issues
- Check migration status
- Verify foreign key constraints
- Monitor query performance
- Check connection pool settings