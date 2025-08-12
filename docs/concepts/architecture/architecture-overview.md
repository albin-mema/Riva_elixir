# Architecture Overview

This document provides a comprehensive overview of the Reservo reservation management system architecture, including domain boundaries, data flow, and key architectural decisions.

## System Architecture

### High-Level Architecture

Reservo follows a clean architecture pattern with clear separation of concerns for reservation management:

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

### Core Technologies

- **Backend**: Elixir with Phoenix Framework
- **Database**: PostgreSQL with AshPostgres
- **Domain Layer**: Ash Framework for domain logic
- **Authentication**: Ash Authentication
- **Frontend**: Phoenix LiveView + Tailwind CSS (UI default with Flop for tables)
- **API**: JSON:API and GraphQL endpoints

## Domain Boundaries

### 1. Accounts Domain

**Responsibilities**:
- User authentication and authorization
- Role-based access control and tenant isolation
- Employee management and organization boundaries
- GDPR compliance for user data

**Key Entities**:
- `User`: Organization owners (data controllers)
- `Employee`: Staff members (data processors + subjects)
- `Role`: Role definitions and permissions
- `Authentication`: Login, registration, tokens, sessions

**Boundary Rules**:
- Authentication is handled by the domain layer using AshAuthentication
- Authorization decisions are made based on user roles and tenant isolation
- User data is isolated by organization boundaries
- All authentication operations must go through the domain layer

### 2. Business Domain

**Responsibilities**:
- Organization settings and configuration
- Customer data and preferences management
- Service offerings, pricing, and availability
- Business-specific reservation rules

**Key Entities**:
- `Organization`: Business settings and configuration
- `Client`: Customer data and preferences
- `Service`: Offerings, pricing, availability
- `Resource`: Reservable items (venues, rooms, equipment, etc.)

**Boundary Rules**:
- Business operations are scoped to organization boundaries
- Client data is managed with proper consent tracking
- Service definitions drive reservation availability and pricing
- All business logic lives in Ash resources

### 3. Reservations Domain

**Responsibilities**:
- Reservation creation and lifecycle management
- Scheduling, availability, and conflict detection
- Recurring reservation processing
- Status management and booking workflows

**Key Entities**:
- `Reservation`: Individual booking instances
- `RecurringReservation`: Template for repeated bookings
- `Availability`: Time slot and resource availability management
- `BookingStatus`: Pending, confirmed, completed, cancelled states

**Boundary Rules**:
- Reservations cannot be created without valid client and resource
- Availability conflicts must be detected before booking
- All reservation operations go through the domain layer
- Status changes follow defined business workflows

### 4. GDPR Domain

**Responsibilities**:
- Comprehensive audit logging using AshPaperTrail
- Data subject rights management (export, rectification, deletion)
- Consent tracking and purpose management
- Regulatory compliance and reporting

**Key Entities**:
- `AuditEntry`: Individual audit log entries created by AshPaperTrail
- `Consent`: Purpose tracking, version management
- `DataSubjectRequest`: Export, rectification, deletion requests
- `ComplianceRecord`: Compliance-related audit data

**Boundary Rules**:
- All domain changes must create audit entries via AshPaperTrail
- Audit data is immutable once created for integrity
- Data subject rights are enforced through domain policies
- Compliance requirements are built into the data model

## Data Flow

### Reservation Creation Flow

```
User Request → Web Controller → Action → Domain Validation →
Database Transaction → Availability Check → Audit Entry → Response
```

1. **Request**: User submits reservation via web interface or API
2. **Controller**: Phoenix controller receives and validates request
3. **Action**: Ash action processes the request with business logic
4. **Domain**: Ash domain entities validate business rules and availability
5. **Database**: PostgreSQL transaction commits data
6. **Availability**: System checks for conflicts and validates time slots
7. **Audit**: Audit entry records the creation with full change tracking
8. **Response**: Success response with reservation details returned to user

### Reservation Retrieval Flow

```
User Request → Query → Domain Logic → Database Query →
Availability Data → Response
```

1. **Request**: User requests reservation list or details
2. **Query**: Ash query processes the request with proper authorization
3. **Domain**: Ash domain entities apply business rules and tenant isolation
4. **Database**: PostgreSQL query retrieves reservation data with relationships
5. **Availability**: Related availability and resource information retrieved
6. **Response**: Reservation data with full context returned to user

### Recurring Reservation Processing Flow

```
Recurring Template → Date Generation → Instance Creation →
Availability Validation → Batch Processing → Audit Trail
```

1. **Template**: Recurring reservation template defines the pattern
2. **Generation**: System calculates consecutive dates based on pattern
3. **Creation**: Individual reservation instances are created in batch
4. **Validation**: Each instance validates availability and business rules
5. **Processing**: Instances are processed based on availability results
6. **Audit**: Comprehensive audit trail tracks all changes and decisions

### Audit Trail Flow

```
Domain Change → AshPaperTrail → Audit Entry → Database Storage →
Compliance Check → Notification
```

1. **Change**: Any domain entity is modified through Ash actions
2. **AshPaperTrail**: Automatic change tracking intercepts all modifications
3. **Entry**: Detailed audit entry created with before/after states
4. **Storage**: Audit data stored in PostgreSQL with full relationships
5. **Compliance**: GDPR compliance rules applied to audit data
6. **Notification**: System notifications sent for critical changes

## Key Design Patterns

### 1. CQRS (Command Query Responsibility Segregation)

**Implementation**:
- Commands handle write operations (create, update, delete)
- Queries handle read operations (list, show, search)
- Separate validation and business logic for each

**Benefits**:
- Clear separation of concerns
- Optimized read and write performance
- Better testability

### 2. Domain-Driven Design (DDD)

**Implementation**:
- Rich domain models with business logic
- Domain events for cross-domain communication
- Bounded contexts for different domains

**Benefits**:
- Business logic encapsulation
- Better maintainability
- Clear domain boundaries

### 3. Event Sourcing

**Implementation**:
- All changes stored as events
- Current state derived from event history
- Event replay for data migration

**Benefits**:
- Complete audit trail
- Data consistency
- Easy data migration

## Performance Considerations

### Database Optimization

- **Indexing**: Strategic indexing on frequently queried fields
- **Partitioning**: Table partitioning for large audit tables
- **Connection Pooling**: Optimized connection pool configuration
- **Query Optimization**: Efficient queries with proper joins

### Asset Storage Optimization

- **Caching**: Asset metadata caching for frequently accessed resources
- **CDN**: Content delivery network for UI assets and downloads
- **Compression**: Asset compression for optimal loading performance
- **Storage Tiers**: Hot/cold storage separation for different asset types

### Caching Strategy

- **Ecto Cache**: Database result caching
- **Phoenix Cache**: Template and fragment caching
- **Redis**: Distributed caching for shared data
- **CDN**: Static asset caching

## Security Architecture

### Authentication Flow

```
User Login → Credential Validation → Token Generation → 
Session Creation → Authorization Check → Access Granted
```

### Authorization Layers

1. **Route Level**: Phoenix router authorization
2. **Controller Level**: Controller authorization checks
3. **Domain Level**: Ash policy-based authorization using SimpleSat SAT solver for efficient permission resolution
4. **Database Level**: Row-level security

### Data Protection

- **Encryption**: AES-256 encryption for sensitive data
- **Token Security**: JWT with proper expiration and refresh
- **Audit Logging**: Complete access and change logging
- **Input Validation**: Comprehensive input sanitization

## Scalability Architecture

### Horizontal Scaling

- **Stateless Services**: Application servers can be scaled horizontally
- **Database Scaling**: Read replicas for query scaling
- **File Storage**: Distributed file storage systems
- **Load Balancing**: Multiple application instances with load balancing

### Vertical Scaling

- **Database Optimization**: Connection pooling and query optimization
- **Memory Management**: Proper EVM configuration
- **File Processing**: Background job processing for large files

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

## Monitoring and Observability

### Application Monitoring

- **Metrics**: Application performance metrics collection
- **Logging**: Structured logging with correlation IDs
- **Tracing**: Distributed tracing for request tracking
- **Health Checks**: Application health monitoring

### Database Monitoring

- **Query Performance**: Slow query detection and optimization
- **Connection Monitoring**: Database connection pool monitoring
- **Storage Monitoring**: Database storage usage tracking

## Future Architecture Considerations

### Microservices Migration

- **Service Boundaries**: Clear service boundaries for each domain
- **API Gateway**: Centralized API management
- **Service Communication**: Event-driven communication
- **Data Consistency**: Eventual consistency patterns

### Cloud-Native Architecture

- **Containerization**: Docker containers for deployment
- **Orchestration**: Kubernetes for container orchestration
- **Infrastructure as Code**: Terraform for infrastructure management
- **CI/CD**: Automated deployment pipelines

This architecture provides a solid foundation for the Reservo reservation management system, ensuring scalability, maintainability, and security while supporting current and future reservation business requirements.