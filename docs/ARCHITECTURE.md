# Architecture

## System Overview
Domain-driven design built on Ash Framework for scalable reservation management.

### Core Stack
- **Ash Framework**: Domain resources, automatic APIs, policy-based auth
- **Phoenix**: LiveView-first web layer with component architecture  
- **PostgreSQL**: ACID-compliant data store with audit trails
- **Tailwind + esbuild**: Modern frontend tooling

### High-Level Architecture
```
┌─────────────────────────────────────────────────────────────┐
│                     Presentation Layer                       │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │   Web UI    │  │   Admin UI  │  │   APIs      │         │
│  │  (LiveView) │  │  (Phoenix)  │  │ (JSON/GQL)  │         │
│  └─────────────┘  └─────────────┘  └─────────────┘         │
└─────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────┐
│                     Application Layer                        │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │   Actions   │  │   Queries   │  │   Policies  │         │
│  │   (Ash)     │  │   (Ash)     │  │   (Ash)     │         │
│  └─────────────┘  └─────────────┘  └─────────────┘         │
└─────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────┐
│                     Domain Layer                            │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │   Accounts  │  │   Business  │  │ Reservations│         │
│  │   (Users/   │  │   (Orgs/    │  │  (Bookings/ │         │
│  │ Employees)  │  │ Clients)    │  │ Scheduling) │         │
│  └─────────────┘  └─────────────┘  └─────────────┘         │
│  ┌─────────────┐                                           │
│  │    GDPR     │                                           │
│  │  (Consent/  │                                           │
│  │   Audit)    │                                           │
│  └─────────────┘                                           │
└─────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────┐
│                   Infrastructure Layer                       │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │  PostgreSQL │  │   Assets    │  │   External  │         │
│  │ (AshPostgres│  │ (Tailwind/  │  │   Services  │         │
│  │   + Audit)  │  │  esbuild)   │  │ (Email/Auth)│         │
│  └─────────────┘  └─────────────┘  └─────────────┘         │
└─────────────────────────────────────────────────────────────┘
```

## Domains

### Accounts Domain
- **Users**: Organization owners (data controllers)
- **Employees**: Staff members (data processors + subjects)
- **Authentication**: Login, registration, tokens, sessions
- **Authorization**: Role-based permissions, tenant isolation

### Business Domain  
- **Organizations**: Business settings and configuration
- **Clients**: Customer data and preferences
- **Services**: Offerings, pricing, availability

### Reservations Domain
- **Bookings**: Reservation creation and lifecycle
- **Scheduling**: Time slots, availability, conflicts
- **Status**: Pending, confirmed, completed, cancelled

### GDPR Domain
- **Consent**: Purpose tracking, version management
- **Data Subject Rights**: Export, rectification, deletion
- **Audit**: Comprehensive change tracking for compliance

## Dual-Actor Model

### Users (Organization Owners)
- **GDPR Role**: Data Controllers
- **Scope**: Business operations, employee management, client data
- **Permissions**: Full control over organization data
- **Isolation**: Organization-level tenant boundaries

### Employees (Staff Members)
- **GDPR Role**: Data Processors + Data Subjects
- **Scope**: Process reservations, manage clients (as processors)
- **Rights**: Full GDPR rights for personal data (as subjects)
- **Permissions**: Role-based access within organization

## Key Architectural Decisions

### Resource-Centric Design
- Business logic lives in Ash resources (not external services)
- Actions define what can be done; policies define who can do it
- Automatic API generation from resource definitions

### Policy-Based Authorization
- Declarative policies at resource level
- Actor-aware (Users vs Employees)
- Tenant isolation via organization relationships

### Audit-First Approach
- All changes tracked via AshPaperTrail
- Soft deletes via AshArchival (archived_at)
- GDPR compliance built into data model

### API-First
- JSON:API and GraphQL auto-generated from resources
- Consistent filtering, sorting, pagination
- Bearer token auth for APIs, sessions for web

## Data Flow

### Typical Request Flow
1. **Authentication**: Verify actor (User/Employee) and load context
2. **Authorization**: Check policies against requested action/resource
3. **Action**: Execute business logic via Ash action
4. **Audit**: Log changes via PaperTrail
5. **Response**: Return data via JSON:API/GraphQL/LiveView

### Multi-Tenancy
- Organization-level isolation via policies
- Users can only access their organization's data
- Employees inherit organization context from employment relationship

## Extensions Used

### Ash Extensions
- **AshPostgres**: Database adapter with advanced querying
- **AshAuthentication**: Token-based auth with multiple strategies
- **AshArchival**: Soft delete with archived_at timestamps
- **AshPaperTrail**: Complete audit trail with diffs
- **AshJsonApi**: Auto-generated JSON:API endpoints
- **AshGraphql**: Auto-generated GraphQL schema
- **AshAdmin**: Web-based admin interface

### Phoenix Extensions
- **LiveView**: Real-time, stateful web interfaces
- **Phoenix.Component**: Reusable UI components
- **Tailwind**: Utility-first CSS framework
- **Esbuild**: Modern JavaScript bundling

## Performance Considerations

### Database
- Indexes on frequently queried fields (organization_id, archived_at)
- Soft deletes require explicit filtering (base_filter: false)
- Audit tables can grow large; consider retention policies

### Caching
- Ash resources support caching at multiple levels
- LiveView provides automatic change tracking
- Consider Redis for session storage in production

### Scaling
- Stateless design enables horizontal scaling
- Database is primary bottleneck; consider read replicas
- Asset serving can be offloaded to CDN
