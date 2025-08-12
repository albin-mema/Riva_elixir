# GraphQL API Guide

This guide provides comprehensive instructions for using the GraphQL API in Riva Ash. It covers schema structure, queries, mutations, subscriptions, error handling, and best practices for efficient data fetching.

For general API information including authentication, base URLs, and common patterns, see the main [API Reference](API_REFERENCE.md).

## Table of Contents

1. [Overview](#overview)
2. [GraphQL Schema](#graphql-schema)
3. [Queries](#queries)
4. [Mutations](#mutations)
5. [Subscriptions](#subscriptions)
6. [Error Handling](#error-handling)
7. [Authentication](#authentication)
8. [Best Practices](#best-practices)
9. [Examples](#examples)
10. [Tooling](#tooling)

## Overview

Riva Ash provides a GraphQL API that allows clients to request exactly the data they need, reducing over-fetching and under-fetching of data. The GraphQL API is built on top of the Ash Framework and provides a type-safe, efficient interface for interacting with the document management system.

### Key Features

- **Type-safe**: Strong typing with GraphQL SDL
- **Efficient**: Request only the data you need
- **Real-time**: Subscriptions for live updates
- **Introspective**: Self-documenting schema
- **Extensible**: Easy to add new types and fields

### GraphQL Endpoint

```
https://api.riva-ash.com/graphql
```

### GraphiQL Interface

Access the interactive GraphQL IDE at:

```
https://api.riva-ash.com/graphiql
```

## GraphQL Schema

### Schema Definition

```graphql
type Query {
  # Document queries
  documents(
    first: Int = 20
    after: String
    last: Int
    before: String
    filter: DocumentFilter
    sort: [DocumentSort!]
  ): DocumentConnection!
  document(id: ID!): Document
  documentsByCategory(categoryId: ID!): [Document!]!
  documentsByUser(userId: ID!): [Document!]!
  searchDocuments(query: String!): [Document!]!

  # User queries
  users(
    first: Int = 20
    after: String
    filter: UserFilter
    sort: [UserSort!]
  ): UserConnection!
  user(id: ID!): User
  currentUser: User

  # Category queries
  categories(
    first: Int = 20
    after: String
    filter: CategoryFilter
    sort: [CategorySort!]
  ): CategoryConnection!
  category(id: ID!): Category
  categoriesByParent(parentId: ID): [Category!]!

  # Audit queries
  auditLogs(
    first: Int = 20
    after: String
    filter: AuditFilter
    sort: [AuditSort!]
  ): AuditConnection!
  auditLog(id: ID!): AuditLog

  # System queries
  systemStats: SystemStats!
  searchTypes: [String!]!
}

type Mutation {
  # Document mutations
  createDocument(input: CreateDocumentInput!): CreateDocumentPayload!
  updateDocument(id: ID!, input: UpdateDocumentInput!): UpdateDocumentPayload!
  deleteDocument(id: ID!): DeleteDocumentPayload!
  uploadDocument(id: ID!, file: Upload!): UploadDocumentPayload!
  shareDocument(id: ID!, input: ShareDocumentInput!): ShareDocumentPayload!

  # User mutations
  createUser(input: CreateUserInput!): CreateUserPayload!
  updateUser(id: ID!, input: UpdateUserInput!): UpdateUserPayload!
  deleteUser(id: ID!): DeleteUserPayload!
  changeUserPassword(id: ID!, input: ChangePasswordInput!): ChangePasswordPayload!

  # Category mutations
  createCategory(input: CreateCategoryInput!): CreateCategoryPayload!
  updateCategory(id: ID!, input: UpdateCategoryInput!): UpdateCategoryPayload!
  deleteCategory(id: ID!): DeleteCategoryPayload!

  # System mutations
  generateReport(input: GenerateReportInput!): GenerateReportPayload!
  exportDocuments(input: ExportDocumentsInput!): ExportDocumentsPayload!
}

type Subscription {
  # Document subscriptions
  documentCreated: Document!
  documentUpdated: Document!
  documentDeleted: Document!
  documentShared: DocumentShare!

  # User subscriptions
  userCreated: User!
  userUpdated: User!
  userDeleted: User!

  # System subscriptions
  systemAlert: SystemAlert!
  auditLogCreated: AuditLog!
}

# Types
type DocumentConnection {
  edges: [DocumentEdge!]!
  pageInfo: PageInfo!
  totalCount: Int!
}

type DocumentEdge {
  node: Document!
  cursor: String!
}

type Document {
  id: ID!
  title: String!
  description: String
  status: DocumentStatus!
  fileSize: Int!
  fileType: String!
  mimeType: String!
  filePath: String!
  tags: [String!]!
  metadata: JSON
  retentionPeriod: Int
  archivalDate: DateTime
  createdAt: DateTime!
  updatedAt: DateTime!
  createdBy: User!
  category: Category!
  versions: [DocumentVersion!]!
  permissions: DocumentPermissions!
}

type DocumentVersion {
  id: ID!
  version: Int!
  fileSize: Int!
  fileType: String!
  createdAt: DateTime!
  createdBy: User!
}

type DocumentPermissions {
  read: Boolean!
  write: Boolean!
  delete: Boolean!
  share: Boolean!
}

type UserConnection {
  edges: [UserEdge!]!
  pageInfo: PageInfo!
  totalCount: Int!
}

type UserEdge {
  node: User!
  cursor: String!
}

type User {
  id: ID!
  email: String!
  name: String!
  role: UserRole!
  status: UserStatus!
  lastLoginAt: DateTime
  createdAt: DateTime!
  updatedAt: DateTime!
  documents: [Document!]!
  categories: [Category!]!
}

type CategoryConnection {
  edges: [CategoryEdge!]!
  pageInfo: PageInfo!
  totalCount: Int!
}

type CategoryEdge {
  node: Category!
  cursor: String!
}

type Category {
  id: ID!
  name: String!
  description: String
  color: String!
  parent: Category
  children: [Category!]!
  documents: [Document!]!
  documentCount: Int!
  createdAt: DateTime!
  updatedAt: DateTime!
}

type AuditConnection {
  edges: [AuditEdge!]!
  pageInfo: PageInfo!
  totalCount: Int!
}

type AuditEdge {
  node: AuditLog!
  cursor: String!
}

type AuditLog {
  id: ID!
  entityType: String!
  entityId: ID!
  action: AuditAction!
  user: User
  changes: JSON
  ipAddress: String
  userAgent: String
  createdAt: DateTime!
}

type DocumentShare {
  id: ID!
  document: Document!
  sharedWith: [User!]!
  permission: SharePermission!
  expiresAt: DateTime
  createdAt: DateTime!
  createdBy: User!
}

type SystemStats {
  totalDocuments: Int!
  totalUsers: Int!
  totalCategories: Int!
  totalStorage: Int!
  availableStorage: Int!
  uptime: String!
  version: String!
}

type SystemAlert {
  id: ID!
  type: AlertType!
  message: String!
  severity: AlertSeverity!
  timestamp: DateTime!
  resolved: Boolean!
}

type PageInfo {
  hasNextPage: Boolean!
  hasPreviousPage: Boolean!
  startCursor: String
  endCursor: String
}

# Enums
enum DocumentStatus {
  ACTIVE
  ARCHIVED
  DELETED
}

enum UserRole {
  ADMIN
  MANAGER
  USER
  VIEWER
}

enum UserStatus {
  ACTIVE
  INACTIVE
}

enum AuditAction {
  CREATE
  UPDATE
  DELETE
  LOGIN
  LOGOUT
  SHARE
  ARCHIVE
  RESTORE
}

enum SharePermission {
  READ
  WRITE
  DELETE
}

enum AlertType {
  SECURITY
  PERFORMANCE
  SYSTEM
  BACKUP
  STORAGE
}

enum AlertSeverity {
  LOW
  MEDIUM
  HIGH
  CRITICAL
}

# Inputs
input DocumentFilter {
  status: DocumentStatus
  category: ID
  createdBy: ID
  tags: [String!]
  createdAt: DateTimeFilter
  updatedAt: DateTimeFilter
  fileSize: IntFilter
  title: StringFilter
}

input UserFilter {
  role: UserRole
  status: UserStatus
  search: String
  createdAt: DateTimeFilter
  updatedAt: DateTimeFilter
}

input CategoryFilter {
  parent: ID
  search: String
  documentCount: IntFilter
}

input AuditFilter {
  entityType: String
  entityId: ID
  action: AuditAction
  user: ID
  createdAt: DateTimeFilter
  ipAddress: String
}

input DocumentSort {
  field: DocumentSortField!
  direction: SortDirection!
}

input UserSort {
  field: UserSortField!
  direction: SortDirection!
}

input CategorySort {
  field: CategorySortField!
  direction: SortDirection!
}

input AuditSort {
  field: AuditSortField!
  direction: SortDirection!
}

input DateTimeFilter {
  equals: DateTime
  before: DateTime
  after: DateTime
  between: [DateTime!]
}

input IntFilter {
  equals: Int
  gt: Int
  lt: Int
  gte: Int
  lte: Int
  between: [Int!]
}

input StringFilter {
  equals: String
  contains: String
  startsWith: String
  endsWith: String
  in: [String!]
}

input CreateDocumentInput {
  title: String!
  description: String
  categoryId: ID!
  tags: [String!]
  retentionPeriod: Int
  metadata: JSON
}

input UpdateDocumentInput {
  title: String
  description: String
  categoryId: ID
  tags: [String!]
  retentionPeriod: Int
  metadata: JSON
}

input CreateUserInput {
  email: String!
  name: String!
  role: UserRole!
  password: String!
  sendWelcomeEmail: Boolean = true
}

input UpdateUserInput {
  name: String
  role: UserRole
  status: UserStatus
}

input ChangePasswordInput {
  currentPassword: String!
  newPassword: String!
}

input CreateCategoryInput {
  name: String!
  description: String
  color: String!
  parentId: ID
}

input UpdateCategoryInput {
  name: String
  description: String
  color: String
  parentId: ID
}

input ShareDocumentInput {
  userIds: [ID!]!
  permission: SharePermission!
  expiresAt: DateTime
}

input GenerateReportInput {
  type: ReportType!
  filters: JSON
  format: ReportFormat!
  includeCharts: Boolean = false
}

input ExportDocumentsInput {
  documentIds: [ID!]!
  format: ExportFormat!
  includeMetadata: Boolean = true
}

# Scalars
scalar DateTime
scalar JSON
scalar Upload

# Enums
enum SortDirection {
  ASC
  DESC
}

enum DocumentSortField {
  CREATED_AT
  UPDATED_AT
  TITLE
  FILE_SIZE
}

enum UserSortField {
  CREATED_AT
  UPDATED_AT
  NAME
  EMAIL
  LAST_LOGIN
}

enum CategorySortField {
  CREATED_AT
  UPDATED_AT
  NAME
  DOCUMENT_COUNT
}

enum AuditSortField {
  CREATED_AT
  ENTITY_TYPE
  ACTION
}

enum ReportType {
  DOCUMENT_ACTIVITY
  USER_ACTIVITY
  STORAGE_USAGE
  AUDIT_SUMMARY
}

enum ReportFormat {
  PDF
  EXCEL
  CSV
  JSON
}

enum ExportFormat {
  PDF
  ZIP
  JSON
}
```

## Queries

### Basic Queries

#### Get All Documents

```graphql
query GetDocuments {
  documents(first: 10, sort: [{field: CREATED_AT, direction: DESC}]) {
    edges {
      node {
        id
        title
        status
        fileSize
        createdAt
        createdBy {
          id
          name
          email
        }
        category {
          id
          name
        }
      }
      cursor
    }
    pageInfo {
      hasNextPage
      hasPreviousPage
      startCursor
      endCursor
    }
    totalCount
  }
}
```

#### Get Single Document

```graphql
query GetDocument($id: ID!) {
  document(id: $id) {
    id
    title
    description
    status
    fileSize
    fileType
    mimeType
    tags
    metadata
    retentionPeriod
    archivalDate
    createdAt
    updatedAt
    createdBy {
      id
      name
      email
    }
    category {
      id
      name
      description
    }
    versions {
      id
      version
      fileSize
      fileType
      createdAt
      createdBy {
        id
        name
      }
    }
    permissions {
      read
      write
      delete
      share
    }
  }
}
```

#### Filtered Queries

```graphql
query GetFilteredDocuments($filter: DocumentFilter!) {
  documents(filter: $filter, first: 20) {
    edges {
      node {
        id
        title
        status
        createdAt
        category {
          id
          name
        }
      }
    }
    totalCount
  }
}

# Variables
{
  "filter": {
    "status": "ACTIVE",
    "category": "550e8400-e29b-41d4-a716-446655440001",
    "createdAt": {
      "after": "2023-01-01T00:00:00Z"
    },
    "tags": ["financial", "2023"]
  }
}
```

#### Search Documents

```graphql
query SearchDocuments($query: String!) {
  searchDocuments(query: $query) {
    id
    title
    description
    status
    category {
      id
      name
    }
    _score
  }
}
```

#### Get Users

```graphql
query GetUsers($filter: UserFilter, $sort: [UserSort!]) {
  users(first: 20, filter: $filter, sort: $sort) {
    edges {
      node {
        id
        email
        name
        role
        status
        lastLoginAt
        createdAt
        documentCount
      }
      cursor
    }
    pageInfo {
      hasNextPage
      endCursor
    }
    totalCount
  }
}
```

#### Get Categories

```graphql
query GetCategories($filter: CategoryFilter) {
  categories(first: 50, filter: $filter) {
    edges {
      node {
        id
        name
        description
        color
        documentCount
        children {
          id
          name
          documentCount
        }
      }
    }
  }
}
```

#### Get Audit Logs

```graphql
query GetAuditLogs($filter: AuditFilter, $first: Int = 20) {
  auditLogs(first: $first, filter: $filter, sort: [{field: CREATED_AT, direction: DESC}]) {
    edges {
      node {
        id
        entityType
        entityId
        action
        user {
          id
          name
          email
        }
        changes
        ipAddress
        userAgent
        createdAt
      }
    }
    totalCount
  }
}
```

#### Get System Stats

```graphql
query GetSystemStats {
  systemStats {
    totalDocuments
    totalUsers
    totalCategories
    totalStorage
    availableStorage
    uptime
    version
  }
}
```

### Advanced Queries

#### Nested Queries

```graphql
query GetDocumentWithDetails($id: ID!) {
  document(id: $id) {
    id
    title
    description
    status
    tags
    metadata
    createdBy {
      id
      name
      email
      role
      documents {
        id
        title
        status
        createdAt
      }
    }
    category {
      id
      name
      description
      parent {
        id
        name
      }
      children {
        id
        name
        documentCount
      }
      documents {
        id
        title
        status
        createdAt
      }
    }
    versions {
      id
      version
      fileSize
      fileType
      createdAt
      createdBy {
        id
        name
        email
      }
    }
    permissions {
      read
      write
      delete
      share
    }
  }
}
```

#### Pagination with Cursors

```graphql
query GetDocumentsWithPagination($after: String, $first: Int = 10) {
  documents(first: $first, after: $after) {
    edges {
      node {
        id
        title
        status
        createdAt
      }
      cursor
    }
    pageInfo {
      hasNextPage
      hasPreviousPage
      startCursor
      endCursor
    }
    totalCount
  }
}
```

#### Multiple Filters

```graphql
query GetDocumentsByMultipleFilters($status: DocumentStatus, $createdBy: ID, $tags: [String!]) {
  documents(
    filter: {
      status: $status
      createdBy: $createdBy
      tags: $tags
    }
    first: 20
  ) {
    edges {
      node {
        id
        title
        status
        tags
        createdAt
      }
    }
    totalCount
  }
}
```

## Mutations

### Basic Mutations

#### Create Document

```graphql
mutation CreateDocument($input: CreateDocumentInput!) {
  createDocument(input: $input) {
    document {
      id
      title
      description
      status
      createdAt
      createdBy {
        id
        name
        email
      }
      category {
        id
        name
      }
    }
    errors {
      field
      message
    }
  }
}

# Variables
{
  "input": {
    "title": "Quarterly Report Q4 2023",
    "description": "Q4 financial performance report",
    "categoryId": "550e8400-e29b-41d4-a716-446655440001",
    "tags": ["q4", "financial", "2023"],
    "retentionPeriod": 2555,
    "metadata": {
      "department": "Finance",
      "fiscalYear": 2023,
      "quarter": 4
    }
  }
}
```

#### Update Document

```graphql
mutation UpdateDocument($id: ID!, $input: UpdateDocumentInput!) {
  updateDocument(id: $id, input: $input) {
    document {
      id
      title
      description
      status
      tags
      metadata
      updatedAt
    }
    errors {
      field
      message
    }
  }
}

# Variables
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "input": {
    "title": "Updated Quarterly Report Q4 2023",
    "description": "Updated Q4 financial performance report",
    "tags": ["q4", "financial", "2023", "updated"],
    "metadata": {
      "department": "Finance",
      "fiscalYear": 2023,
      "quarter": 4,
      "approved": true
    }
  }
}
```

#### Delete Document

```graphql
mutation DeleteDocument($id: ID!) {
  deleteDocument(id: $id) {
    documentId
    success
    errors {
      field
      message
    }
  }
}
```

#### Upload Document File

```graphql
mutation UploadDocumentFile($id: ID!, $file: Upload!) {
  uploadDocument(id: $id, file: $file) {
    document {
      id
      title
      fileSize
      fileType
      mimeType
      filePath
      versions {
        id
        version
        fileSize
        fileType
        createdAt
        createdBy {
          id
          name
        }
      }
    }
    errors {
      field
      message
    }
  }
}
```

#### Share Document

```graphql
mutation ShareDocument($id: ID!, $input: ShareDocumentInput!) {
  shareDocument(id: $id, input: $input) {
    documentShare {
      id
      document {
        id
        title
      }
      sharedWith {
        id
        name
        email
      }
      permission
      expiresAt
      createdAt
      createdBy {
        id
        name
      }
    }
    errors {
      field
      message
    }
  }
}

# Variables
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "input": {
    "userIds": ["550e8400-e29b-41d4-a716-446655440001", "550e8400-e29b-41d4-a716-446655440002"],
    "permission": "READ",
    "expiresAt": "2023-12-31T23:59:59Z"
  }
}
```

### User Management Mutations

#### Create User

```graphql
mutation CreateUser($input: CreateUserInput!) {
  createUser(input: $input) {
    user {
      id
      email
      name
      role
      status
      createdAt
    }
    errors {
      field
      message
    }
  }
}

# Variables
{
  "input": {
    "email": "jane.smith@example.com",
    "name": "Jane Smith",
    "role": "USER",
    "password": "securepassword123",
    "sendWelcomeEmail": true
  }
}
```

#### Update User

```graphql
mutation UpdateUser($id: ID!, $input: UpdateUserInput!) {
  updateUser(id: $id, input: $input) {
    user {
      id
      name
      role
      status
      updatedAt
    }
    errors {
      field
      message
    }
  }
}
```

#### Change User Password

```graphql
mutation ChangeUserPassword($id: ID!, $input: ChangePasswordInput!) {
  changeUserPassword(id: $id, input: $input) {
    success
    errors {
      field
      message
    }
  }
}
```

### Category Management Mutations

#### Create Category

```graphql
mutation CreateCategory($input: CreateCategoryInput!) {
  createCategory(input: $input) {
    category {
      id
      name
      description
      color
      parent {
        id
        name
      }
      documentCount
      createdAt
    }
    errors {
      field
      message
    }
  }
}

# Variables
{
  "input": {
    "name": "Legal Documents",
    "description": "Legal and compliance documents",
    "color": "#d69e2e",
    "parentId": null
  }
}
```

#### Update Category

```graphql
mutation UpdateCategory($id: ID!, $input: UpdateCategoryInput!) {
  updateCategory(id: $id, input: $input) {
    category {
      id
      name
      description
      color
      parent {
        id
        name
      }
      documentCount
      updatedAt
    }
    errors {
      field
      message
    }
  }
}
```

### System Mutations

#### Generate Report

```graphql
mutation GenerateReport($input: GenerateReportInput!) {
  generateReport(input: $input) {
    report {
      id
      type
      format
      status
      downloadUrl
      createdAt
    }
    errors {
      field
      message
    }
  }
}

# Variables
{
  "input": {
    "type": "DOCUMENT_ACTIVITY",
    "filters": {
      "dateRange": "2023-Q4",
      "categories": ["financial", "hr"]
    },
    "format": "PDF",
    "includeCharts": true
  }
}
```

#### Export Documents

```graphql
mutation ExportDocuments($input: ExportDocumentsInput!) {
  exportDocuments(input: $input) {
    export {
      id
      format
      status
      downloadUrl
      documentCount
      createdAt
    }
    errors {
      field
      message
    }
  }
}

# Variables
{
  "input": {
    "documentIds": ["550e8400-e29b-41d4-a716-446655440000", "550e8400-e29b-41d4-a716-446655440001"],
    "format": "ZIP",
    "includeMetadata": true
  }
}
```

## Subscriptions

### Document Subscriptions

#### Document Created

```graphql
subscription DocumentCreated {
  documentCreated {
    id
    title
    status
    createdAt
    createdBy {
      id
      name
      email
    }
    category {
      id
      name
    }
  }
}
```

#### Document Updated

```graphql
subscription DocumentUpdated {
  documentUpdated {
    id
    title
    description
    status
    tags
    metadata
    updatedAt
    updatedBy {
      id
      name
      email
    }
  }
}
```

#### Document Deleted

```graphql
subscription DocumentDeleted {
  documentDeleted {
    id
    title
    deletedAt
    deletedBy {
      id
      name
      email
    }
  }
}
```

#### Document Shared

```graphql
subscription DocumentShared {
  documentShared {
    id
    document {
      id
      title
    }
    sharedWith {
      id
      name
      email
    }
    permission
    expiresAt
    createdAt
    createdBy {
      id
      name
    }
  }
}
```

### User Subscriptions

#### User Created

```graphql
subscription UserCreated {
  userCreated {
    id
    email
    name
    role
    status
    createdAt
  }
}
```

#### User Updated

```graphql
subscription UserUpdated {
  userUpdated {
    id
    name
    role
    status
    updatedAt
  }
}
```

### System Subscriptions

#### System Alert

```graphql
subscription SystemAlert {
  systemAlert {
    id
    type
    message
    severity
    timestamp
    resolved
  }
}
```

#### Audit Log Created

```graphql
subscription AuditLogCreated {
  auditLogCreated {
    id
    entityType
    entityId
    action
    user {
      id
      name
      email
    }
    changes
    ipAddress
    userAgent
    createdAt
  }
}
```

## Error Handling

### Error Response Format

```graphql
{
  "data": {
    "createDocument": null
  },
  "errors": [
    {
      "message": "Validation failed",
      "locations": [
        {
          "line": 2,
          "column": 3
        }
      ],
      "path": ["createDocument"],
      "extensions": {
        "code": "VALIDATION_ERROR",
        "validation": {
          "title": "Title is required"
        }
      }
    }
  ]
}
```

### Common Error Codes

| Code | Description |
|------|-------------|
| `GRAPHQL_VALIDATION_FAILED` | Query validation failed |
| `VALIDATION_ERROR` | Input validation failed |
| `UNAUTHORIZED` | Authentication required |
| `FORBIDDEN` | Insufficient permissions |
| `NOT_FOUND` | Resource not found |
| `CONFLICT` | Resource conflict |
| `RATE_LIMITED` | Rate limit exceeded |
| `INTERNAL_ERROR` | Internal server error |
| `FILE_TOO_LARGE` | File size exceeds limit |
| `INVALID_FILE_TYPE` | File type not allowed |

### Error Handling in JavaScript

```javascript
async function createDocument(documentInput) {
  const query = `
    mutation CreateDocument($input: CreateDocumentInput!) {
      createDocument(input: $input) {
        document {
          id
          title
        }
        errors {
          field
          message
        }
      }
    }
  `;

  try {
    const response = await fetch('/graphql', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${token}`
      },
      body: JSON.stringify({
        query,
        variables: { input: documentInput }
      })
    });

    const result = await response.json();

    if (result.errors) {
      // Handle GraphQL errors
      const error = result.errors[0];
      if (error.extensions?.code === 'VALIDATION_ERROR') {
        throw new Error(`Validation failed: ${error.extensions.validation.title}`);
      } else {
        throw new Error(error.message);
      }
    }

    if (result.data.createDocument.errors) {
      // Handle mutation errors
      const error = result.data.createDocument.errors[0];
      throw new Error(`${error.field}: ${error.message}`);
    }

    return result.data.createDocument.document;
  } catch (error) {
    console.error('Error creating document:', error);
    throw error;
  }
}
```

## Authentication

### JWT Authentication

```javascript
const fetchWithAuth = async (query, variables = {}) => {
  const token = localStorage.getItem('accessToken');
  
  const response = await fetch('/graphql', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${token}`
    },
    body: JSON.stringify({
      query,
      variables
    })
  });

  const result = await response.json();

  // Handle token expiration
  if (result.errors?.some(error => 
    error.message.includes('JWT expired') || 
    error.message.includes('Unauthorized')
  )) {
    // Refresh token logic
    const newToken = await refreshToken();
    localStorage.setItem('accessToken', newToken);
    
    // Retry the request
    return fetchWithAuth(query, variables);
  }

  return result;
};
```

### API Key Authentication

```javascript
const fetchWithAPIKey = async (query, variables = {}) => {
  const response = await fetch('/graphql', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'X-API-Key': apiKey
    },
    body: JSON.stringify({
      query,
      variables
    })
  });

  return response.json();
};
```

## Best Practices

### 1. Query Design

#### Use Fragments for Reusable Fields

```graphql
fragment DocumentFields on Document {
  id
  title
  status
  createdAt
  createdBy {
    id
    name
    email
  }
  category {
    id
    name
  }
}

query GetDocuments {
  documents(first: 10) {
    edges {
      node {
        ...DocumentFields
        tags
        fileSize
      }
    }
  }
}

query GetDocument($id: ID!) {
  document(id: $id) {
    ...DocumentFields
    description
    metadata
    versions {
      id
      version
      createdAt
    }
  }
}
```

#### Avoid Over-fetching

```graphql
# Bad: Fetches more data than needed
query GetDocumentList {
  documents {
    id
    title
    description
    status
    fileSize
    fileType
    mimeType
    filePath
    tags
    metadata
    retentionPeriod
    archivalDate
    createdAt
    updatedAt
    createdBy {
      id
      email
      name
      role
      status
      lastLoginAt
      createdAt
      updatedAt
      documents {
        id
        title
        status
        createdAt
      }
    }
    category {
      id
      name
      description
      color
      parent {
        id
        name
      }
      children {
        id
        name
        documentCount
      }
      documents {
        id
        title
        status
        createdAt
      }
    }
    versions {
      id
      version
      fileSize
      fileType
      createdAt
      createdBy {
        id
        name
        email
      }
    }
    permissions {
      read
      write
      delete
      share
    }
  }
}

# Good: Only fetch needed data
query GetDocumentList {
  documents(first: 10) {
    edges {
      node {
        id
        title
        status
        createdAt
        createdBy {
          id
          name
        }
        category {
          id
          name
        }
      }
    }
  }
}
```

### 2. Mutation Design

#### Use Input Objects

```graphql
mutation CreateDocument($input: CreateDocumentInput!) {
  createDocument(input: $input) {
    document {
      id
      title
      status
    }
    errors {
      field
      message
    }
  }
}
```

#### Handle Errors Gracefully

```javascript
async function handleCreateDocument(documentInput) {
  const query = `
    mutation CreateDocument($input: CreateDocumentInput!) {
      createDocument(input: $input) {
        document {
          id
          title
        }
        errors {
          field
          message
        }
      }
    }
  `;

  const result = await fetchWithAuth(query, { input: documentInput });

  if (result.errors) {
    // Handle GraphQL errors
    throw new Error(result.errors[0].message);
  }

  if (result.data.createDocument.errors) {
    // Handle validation errors
    const errors = result.data.createDocument.errors;
    const errorMessages = errors.map(error => `${error.field}: ${error.message}`);
    throw new Error(`Validation failed: ${errorMessages.join(', ')}`);
  }

  return result.data.createDocument.document;
}
```

### 3. Subscription Management

#### Clean Up Subscriptions

```javascript
class DocumentSubscriptionManager {
  constructor() {
    this.subscriptions = new Map();
  }

  subscribeToDocumentUpdates(documentId, callback) {
    const subscription = this.createSubscription(
      `subscription DocumentUpdated($id: ID!) {
        documentUpdated(id: $id) {
          id
          title
          description
          status
          updatedAt
        }
      }`,
      { id: documentId },
      callback
    );

    this.subscriptions.set(documentId, subscription);
    return subscription;
  }

  unsubscribeFromDocumentUpdates(documentId) {
    const subscription = this.subscriptions.get(documentId);
    if (subscription) {
      subscription.unsubscribe();
      this.subscriptions.delete(documentId);
    }
  }

  createSubscription(query, variables, callback) {
    return this.client.subscribe({
      query,
      variables,
      result: (result) => {
        if (result.errors) {
          console.error('Subscription error:', result.errors);
          return;
        }
        callback(result.data);
      },
      error: (error) => {
        console.error('Subscription error:', error);
      }
    });
  }
}
```

### 4. Caching Strategy

#### Use Apollo Client Cache

```javascript
const client = new ApolloClient({
  link: authLink.concat(httpLink),
  cache: new InMemoryCache({
    typePolicies: {
      Query: {
        fields: {
          documents: {
            keyArgs: false,
            merge(existing = { edges: [] }, incoming) {
              return {
                ...incoming,
                edges: [...existing.edges, ...incoming.edges]
              };
            }
          },
          users: {
            keyArgs: false,
            merge(existing = { edges: [] }, incoming) {
              return {
                ...incoming,
                edges: [...existing.edges, ...incoming.edges]
              };
            }
          }
        }
      }
    }
  })
});
```

## Examples

### Complete Document Management Example

```javascript
class DocumentManager {
  constructor(client) {
    this.client = client;
  }

  async getDocuments(filters = {}, pagination = {}) {
    const query = `
      query GetDocuments($filter: DocumentFilter, $first: Int, $after: String) {
        documents(filter: $filter, first: $first, after: $after) {
          edges {
            node {
              id
              title
              status
              createdAt
              createdBy {
                id
                name
              }
              category {
                id
                name
              }
            }
            cursor
          }
          pageInfo {
            hasNextPage
            endCursor
          }
          totalCount
        }
      }
    `;

    const result = await this.client.query({
      query,
      variables: {
        filter: filters,
        first: pagination.first || 20,
        after: pagination.after
      }
    });

    return result.data.documents;
  }

  async createDocument(input) {
    const mutation = `
      mutation CreateDocument($input: CreateDocumentInput!) {
        createDocument(input: $input) {
          document {
            id
            title
            status
            createdAt
          }
          errors {
            field
            message
          }
        }
      }
    `;

    const result = await this.client.mutate({
      mutation,
      variables: { input },
      refetchQueries: ['GetDocuments']
    });

    return result.data.createDocument.document;
  }

  async updateDocument(id, input) {
    const mutation = `
      mutation UpdateDocument($id: ID!, $input: UpdateDocumentInput!) {
        updateDocument(id: $id, input: $input) {
          document {
            id
            title
            status
            updatedAt
          }
          errors {
            field
            message
          }
        }
      }
    `;

    const result = await this.client.mutate({
      mutation,
      variables: { id, input },
      refetchQueries: ['GetDocuments']
    });

    return result.data.updateDocument.document;
  }

  async deleteDocument(id) {
    const mutation = `
      mutation DeleteDocument($id: ID!) {
        deleteDocument(id: $id) {
          documentId
          success
        }
      }
    `;

    const result = await this.client.mutate({
      mutation,
      variables: { id },
      refetchQueries: ['GetDocuments']
    });

    return result.data.deleteDocument.success;
  }

  subscribeToDocumentUpdates(callback) {
    const subscription = this.client.subscribe({
      query: `
        subscription DocumentUpdates {
          documentCreated {
            id
            title
            status
            createdAt
          }
          documentUpdated {
            id
            title
            status
            updatedAt
          }
          documentDeleted {
            id
            title
            deletedAt
          }
        }
      `
    });

    return subscription.subscribe({
      next: callback,
      error: (error) => {
        console.error('Subscription error:', error);
      }
    });
  }
}

// Usage
const documentManager = new DocumentManager(client);

// Get documents
const documents = await documentManager.getDocuments({
  status: 'ACTIVE',
  category: '550e8400-e29b-41d4-a716-446655440001'
});

// Create document
const newDocument = await documentManager.createDocument({
  title: 'Quarterly Report Q4 2023',
  description: 'Q4 financial performance report',
  categoryId: '550e8400-e29b-41d4-a716-446655440001',
  tags: ['q4', 'financial', '2023']
});

// Update document
await documentManager.updateDocument(newDocument.id, {
  title: 'Updated Quarterly Report Q4 2023',
  tags: ['q4', 'financial', '2023', 'updated']
});

// Subscribe to updates
const subscription = documentManager.subscribeToDocumentUpdates((result) => {
  console.log('Document update:', result);
});

// Cleanup
subscription.unsubscribe();
```

### React Component Example

```jsx
import React, { useState, useEffect } from 'react';
import { useQuery, useMutation, useSubscription, useApolloClient } from '@apollo/client';
import { GET_DOCUMENTS, CREATE_DOCUMENT, UPDATE_DOCUMENT, DELETE_DOCUMENT, DOCUMENT_UPDATES } from './queries';

const DocumentList = () => {
  const [filters, setFilters] = useState({});
  const [pagination, setPagination] = useState({ first: 10 });
  const [showCreateForm, setShowCreateForm] = useState(false);
  
  const { loading, error, data, fetchMore } = useQuery(GET_DOCUMENTS, {
    variables: { filter: filters, ...pagination }
  });

  const [createDocument] = useMutation(CREATE_DOCUMENT, {
    refetchQueries: [{ query: GET_DOCUMENTS }]
  });

  const [updateDocument] = useMutation(UPDATE_DOCUMENT, {
    refetchQueries: [{ query: GET_DOCUMENTS }]
  });

  const [deleteDocument] = useMutation(DELETE_DOCUMENT, {
    refetchQueries: [{ query: GET_DOCUMENTS }]
  });

  useSubscription(DOCUMENT_UPDATES, {
    onSubscriptionData: ({ subscriptionData }) => {
      console.log('Document update:', subscriptionData.data);
      // Optionally update local cache
    }
  });

  const handleLoadMore = () => {
    if (data.documents.pageInfo.hasNextPage) {
      fetchMore({
        variables: {
          after: data.documents.pageInfo.endCursor,
          first: pagination.first
        },
        updateQuery: (prev, { fetchMoreResult }) => {
          if (!fetchMoreResult) return prev;
          
          return {
            documents: {
              ...fetchMoreResult.documents,
              edges: [...prev.documents.edges, ...fetchMoreResult.documents.edges]
            }
          };
        }
      });
    }
  };

  const handleCreateDocument = async (input) => {
    try {
      await createDocument({ variables: { input } });
      setShowCreateForm(false);
    } catch (error) {
      console.error('Error creating document:', error);
    }
  };

  const handleUpdateDocument = async (id, input) => {
    try {
      await updateDocument({ variables: { id, input } });
    } catch (error) {
      console.error('Error updating document:', error);
    }
  };

  const handleDeleteDocument = async (id) => {
    try {
      await deleteDocument({ variables: { id } });
    } catch (error) {
      console.error('Error deleting document:', error);
    }
  };

  if (loading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;

  return (
    <div>
      <div className="flex justify-between items-center mb-4">
        <h1 className="text-2xl font-bold">Documents</h1>
        <button
          onClick={() => setShowCreateForm(true)}
          className="bg-blue-500 text-white px-4 py-2 rounded"
        >
          Create Document
        </button>
      </div>

      {showCreateForm && (
        <DocumentForm
          onSubmit={handleCreateDocument}
          onCancel={() => setShowCreateForm(false)}
        />
      )}

      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
        {data.documents.edges.map((edge) => (
          <DocumentCard
            key={edge.node.id}
            document={edge.node}
            onUpdate={handleUpdateDocument}
            onDelete={handleDeleteDocument}
          />
        ))}
      </div>

      {data.documents.pageInfo.hasNextPage && (
        <div className="mt-4 text-center">
          <button
            onClick={handleLoadMore}
            className="bg-gray-500 text-white px-4 py-2 rounded"
          >
            Load More
          </button>
        </div>
      )}
    </div>
  );
};

const DocumentCard = ({ document, onUpdate, onDelete }) => {
  const [isEditing, setIsEditing] = useState(false);
  const [editForm, setEditForm] = useState({
    title: document.title,
    description: document.description
  });

  const handleSave = () => {
    onUpdate(document.id, editForm);
    setIsEditing(false);
  };

  const handleDelete = () => {
    if (window.confirm('Are you sure you want to delete this document?')) {
      onDelete(document.id);
    }
  };

  return (
    <div className="border rounded-lg p-4">
      {isEditing ? (
        <div>
          <input
            type="text"
            value={editForm.title}
            onChange={(e) => setEditForm({ ...editForm, title: e.target.value })}
            className="w-full p-2 border rounded mb-2"
          />
          <textarea
            value={editForm.description}
            onChange={(e) => setEditForm({ ...editForm, description: e.target.value })}
            className="w-full p-2 border rounded mb-2"
          />
          <div className="flex space-x-2">
            <button
              onClick={handleSave}
              className="bg-green-500 text-white px-3 py-1 rounded"
            >
              Save
            </button>
            <button
              onClick={() => setIsEditing(false)}
              className="bg-gray-500 text-white px-3 py-1 rounded"
            >
              Cancel
            </button>
          </div>
        </div>
      ) : (
        <div>
          <h3 className="text-lg font-semibold mb-2">{document.title}</h3>
          <p className="text-gray-600 mb-2">{document.description}</p>
          <div className="flex justify-between items-center">
            <span className="text-sm text-gray-500">
              {document.createdAt}
            </span>
            <div className="space-x-2">
              <button
                onClick={() => setIsEditing(true)}
                className="text-blue-500 hover:text-blue-700"
              >
                Edit
              </button>
              <button
                onClick={handleDelete}
                className="text-red-500 hover:text-red-700"
              >
                Delete
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
};

export default DocumentList;
```

## Tooling

### GraphiQL

Access GraphiQL at `https://api.riva-ash.com/graphiql` for interactive exploration of the GraphQL API.

### Apollo Studio

Use Apollo Studio for advanced GraphQL development, including schema registry, performance monitoring, and query debugging.

### GraphQL Playground

For local development, use GraphQL Playground:

```bash
npm install -g graphql-playground
graphql-playground http://localhost:4000/graphql
```

### VS Code Extensions

Install these VS Code extensions for better GraphQL development:

- **GraphQL**: Language support and syntax highlighting
- **Apollo Client**: Apollo Client development tools
- **GraphQL**: GraphQL schema visualization

This GraphQL API guide provides comprehensive instructions for working with the GraphQL implementation in Riva Ash. Follow these best practices to build efficient and maintainable client applications.