# Architecture Overview

This document provides a comprehensive overview of the Reservo system architecture, including domain boundaries, data flow, and key architectural decisions.

## System Architecture

### High-Level Architecture

Reservo follows a clean architecture pattern with clear separation of concerns:

```
┌─────────────────────────────────────────────────────────────┐
│                     Presentation Layer                       │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │   Web UI    │  │   Admin UI  │  │   API       │         │
│  │  (Phoenix)  │  │  (Phoenix)  │  │  (GraphQL/  │         │
│  └─────────────┘  └─────────────┘  │  REST)      │         │
└─────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────┐
│                     Application Layer                        │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │   Commands  │  │   Queries   │  │   Policies  │         │
│  │  (CQRS)     │  │  (CQRS)     │  │  (Ash)      │         │
│  └─────────────┘  └─────────────┘  └─────────────┘         │
└─────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────┐
│                     Domain Layer                            │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │   Documents │  │   Users     │  │   Archives  │         │
│  │   (Ash)     │  │   (Ash)     │  │   (Ash)     │         │
│  └─────────────┘  └─────────────┘  └─────────────┘         │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │   Audit     │  │   Categories│  │   Metadata  │         │
│  │   Trail     │  │   (Ash)     │  │   (Ash)     │         │
│  │   (Ash)     │  └─────────────┘  └─────────────┘         │
│  └─────────────┘                                          │
└─────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────┐
│                   Infrastructure Layer                       │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │   Database  │  │   File      │  │   External  │         │
│  │  (PostgreSQL│  │   Storage   │  │   Services  │         │
│  │             │  │  (S3/Local) │  │   (Email/   │         │
│  └─────────────┘  └─────────────┘  │   Auth)     │         │
└─────────────────────────────────────────────────────────────┘
```

### Core Technologies

- **Backend**: Elixir with Phoenix Framework
- **Database**: PostgreSQL with Ecto
- **Domain Layer**: Ash Framework for domain logic
- **Authentication**: Ash Authentication
- **File Storage**: S3-compatible storage or local filesystem
- **Frontend**: Phoenix LiveView + Tailwind CSS (UI default with Flop for tables)
- **API**: GraphQL and REST endpoints

## Domain Boundaries

### 1. Document Management Domain

**Responsibilities**:
- Document lifecycle management
- File storage and retrieval
- Document metadata management
- Version control

**Key Entities**:
- `Document`: Core document entity with file, metadata, and relationships
- `DocumentVersion`: Version history and change tracking
- `DocumentCategory`: Classification and organization
- `DocumentTag`: Tagging and search capabilities

**Boundary Rules**:
- Documents cannot exist without a valid category
- All document operations must go through the domain layer
- File operations are abstracted behind storage interfaces

### 2. User Management Domain

**Responsibilities**:
- User authentication and authorization
- Role-based access control
- User profile management
- Permission management

**Key Entities**:
- `User`: User account and profile information
- `Role`: Role definitions and permissions
- `UserRole`: Assignment of roles to users
- `Permission`: Granular permission definitions

**Boundary Rules**:
- Authentication is handled by the domain layer
- Authorization decisions are made based on user roles
- User data is isolated by organization

### 3. Audit Trail Domain

**Responsibilities**:
- Comprehensive audit logging using AshPaperTrail
- Change tracking for all entities with full history
- Compliance reporting and regulatory adherence
- Historical data preservation for audit and retention purposes

**Key Entities**:
- `AuditEntry`: Individual audit log entries created by AshPaperTrail
- `AuditTrail`: Collection of related audit entries for comprehensive tracking
- `ComplianceRecord`: Compliance-related audit data for regulatory requirements

**Boundary Rules**:
- All domain changes must create audit entries via AshPaperTrail
- Audit data is immutable once created for integrity
- Audit trails are maintained independently of business logic as retention features
- Designed for audit and compliance purposes, not document management

### 4. Data Retention Domain

**Responsibilities**:
- Soft delete and archival policies using AshArchival
- Automated archival processes for data lifecycle management
- Retention management for regulatory compliance
- Archive retrieval for audit and historical purposes

**Key Entities**:
- `ArchivedRecord`: Soft-deleted instances retained for audit purposes
- `RetentionPolicy`: Rules for data archival and lifecycle management
- `AuditLog`: Comprehensive audit trails for all data operations

**Boundary Rules**:
- Archival decisions are based on configured retention policies
- Soft delete operations use AshArchival for audit trail maintenance
- Archived data is retained for compliance and audit purposes, not document management
- Retrieval focuses on audit and compliance requirements

## Data Flow

### Document Creation Flow

```
User Request → Web Controller → Command → Domain Validation → 
Database Transaction → File Storage → Audit Entry → Response
```

1. **Request**: User submits document via web interface or API
2. **Controller**: Phoenix controller receives and validates request
3. **Command**: CQRS command processes the request
4. **Domain**: Ash domain entities handle business logic
5. **Database**: PostgreSQL transaction commits data
6. **Storage**: File is stored in S3 or local filesystem
7. **Audit**: Audit entry records the creation
8. **Response**: Success response returned to user

### Document Retrieval Flow

```
User Request → Query Handler → Domain Logic → Database Query → 
File Metadata → Response
```

1. **Request**: User requests document list or details
2. **Query**: CQRS query processes the request
3. **Domain**: Ash domain entities apply business rules
4. **Database**: PostgreSQL query retrieves metadata
5. **Storage**: File location information retrieved
6. **Response**: Document data returned to user

### Audit Trail Flow

```
Domain Change → Audit Middleware → Audit Entry → Database Storage → 
Compliance Check → Notification
```

1. **Change**: Domain entity is modified
2. **Middleware**: Ash audit middleware intercepts change
3. **Entry**: Audit entry created with change details
4. **Storage**: Audit data stored in PostgreSQL
5. **Compliance**: Compliance rules applied
6. **Notification**: Notifications sent if required

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

### File Storage Optimization

- **Caching**: File metadata caching for frequently accessed documents
- **CDN**: Content delivery network for file downloads
- **Compression**: File compression where appropriate
- **Storage Tiers**: Hot/cold storage separation

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

This architecture provides a solid foundation for the Reservo application, ensuring scalability, maintainability, and security while supporting current and future business requirements.