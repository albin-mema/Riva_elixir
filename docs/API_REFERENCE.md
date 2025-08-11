# API Reference

Prefer the compact map: [API_REFERENCE_COMPACT.md](./API_REFERENCE_COMPACT.md)

This file remains as a deep‑dive reference.

## Table of Contents

1. [Overview](#overview)
2. [Authentication](#authentication)
3. [Endpoints](#endpoints)
4. [Data Models](#data-models)
5. [Error Handling](#error-handling)
6. [Rate Limiting](#rate-limiting)
7. [Webhooks](#webhooks)
8. [SDKs and Libraries](#sdks-and-libraries)
9. [Examples](#examples)
10. [Changelog](#changelog)

## Overview

The Riva Ash API provides RESTful and GraphQL interfaces for integrating with the document management system. The API supports document management, user management, audit logging, and system administration.

### Base URLs

- **Production**: `https://api.riva-ash.com/v1`
- **Staging**: `https://staging-api.riva-ash.com/v1`
- **Development**: `http://localhost:4000/api/v1`

### API Versioning

The API uses URL versioning (`/v1/`). New versions will be introduced when breaking changes are made. The current version is `v1`.

### Response Format

All API responses are in JSON format. The structure follows REST conventions:

```json
{
  "data": {
    // Response data
  },
  "meta": {
    "timestamp": "2023-12-01T12:00:00Z",
    "request_id": "req_123456789",
    "version": "1.0.0"
  },
  "links": {
    "self": "/api/v1/documents/123",
    "next": "/api/v1/documents/124"
  }
}
```

### Date Format

All dates are formatted as ISO 8601 UTC:

```json
{
  "created_at": "2023-12-01T12:00:00Z",
  "updated_at": "2023-12-01T12:00:00Z"
}
```

## Authentication

### API Key Authentication

```bash
curl -H "Authorization: Bearer YOUR_API_KEY" \
     -H "Content-Type: application/json" \
     https://api.riva-ash.com/v1/documents
```

### OAuth 2.0

The API supports OAuth 2.0 for third-party integrations:

```bash
curl -H "Authorization: Bearer YOUR_ACCESS_TOKEN" \
     -H "Content-Type: application/json" \
     https://api.riva-ash.com/v1/documents
```

### JWT Tokens

```bash
curl -H "Authorization: Bearer YOUR_JWT_TOKEN" \
     -H "Content-Type: application/json" \
     https://api.riva-ash.com/v1/documents
```

## Endpoints

### Authentication Endpoints

#### POST /auth/login

Authenticate a user and return access tokens.

**Request**:
```json
{
  "email": "user@example.com",
  "password": "password123",
  "remember_me": false
}
```

**Response**:
```json
{
  "data": {
    "access_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
    "refresh_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
    "expires_in": 3600,
    "token_type": "Bearer",
    "user": {
      "id": "550e8400-e29b-41d4-a716-446655440000",
      "email": "user@example.com",
      "name": "John Doe",
      "role": "user"
    }
  }
}
```

#### POST /auth/refresh

Refresh an access token using a refresh token.

**Request**:
```json
{
  "refresh_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
}
```

**Response**:
```json
{
  "data": {
    "access_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
    "expires_in": 3600,
    "token_type": "Bearer"
  }
}
```

#### POST /auth/logout

Invalidate the current access token.

**Request**:
```json
{
  "access_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
}
```

**Response**:
```json
{
  "data": {
    "message": "Successfully logged out"
  }
}
```

### Document Endpoints

#### GET /documents

List documents with filtering and pagination.

**Query Parameters**:
- `page`: Page number (default: 1)
- `per_page`: Items per page (default: 20, max: 100)
- `search`: Search term
- `category`: Filter by category ID
- `status`: Filter by status (active, archived, deleted)
- `user_id`: Filter by user ID
- `created_after`: Filter by creation date (ISO 8601)
- `created_before`: Filter by creation date (ISO 8601)
- `sort`: Sort field (created_at, updated_at, title)
- `order`: Sort order (asc, desc)

**Response**:
```json
{
  "data": [
    {
      "id": "550e8400-e29b-41d4-a716-446655440000",
      "title": "Annual Report 2023",
      "description": "Company annual financial report",
      "category": {
        "id": "550e8400-e29b-41d4-a716-446655440001",
        "name": "Financial Documents"
      },
      "status": "active",
      "file_size": 2048576,
      "file_type": "pdf",
      "created_at": "2023-12-01T12:00:00Z",
      "updated_at": "2023-12-01T12:00:00Z",
      "tags": ["annual", "financial", "report"]
    }
  ],
  "meta": {
    "total": 150,
    "page": 1,
    "per_page": 20,
    "total_pages": 8
  }
}
```

#### POST /documents

Create a new document.

**Request**:
```json
{
  "title": "Quarterly Report Q4 2023",
  "description": "Q4 financial performance report",
  "category_id": "550e8400-e29b-41d4-a716-446655440001",
  "tags": ["q4", "financial", "2023"],
  "retention_period": 2555,
  "metadata": {
    "department": "Finance",
    "fiscal_year": 2023,
    "quarter": 4
  }
}
```

**Response**:
```json
{
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440002",
    "title": "Quarterly Report Q4 2023",
    "description": "Q4 financial performance report",
    "category": {
      "id": "550e8400-e29b-41d4-a716-446655440001",
      "name": "Financial Documents"
    },
    "status": "active",
    "file_size": 0,
    "file_type": null,
    "created_at": "2023-12-01T12:00:00Z",
    "updated_at": "2023-12-01T12:00:00Z",
    "tags": ["q4", "financial", "2023"],
    "metadata": {
      "department": "Finance",
      "fiscal_year": 2023,
      "quarter": 4
    }
  }
}
```

#### GET /documents/{id}

Get document details.

**Response**:
```json
{
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440002",
    "title": "Quarterly Report Q4 2023",
    "description": "Q4 financial performance report",
    "category": {
      "id": "550e8400-e29b-41d4-a716-446655440001",
      "name": "Financial Documents"
    },
    "status": "active",
    "file_size": 2048576,
    "file_type": "pdf",
    "mime_type": "application/pdf",
    "file_path": "/documents/550e8400-e29b-41d4-a716-446655440002.pdf",
    "created_at": "2023-12-01T12:00:00Z",
    "updated_at": "2023-12-01T12:00:00Z",
    "tags": ["q4", "financial", "2023"],
    "metadata": {
      "department": "Finance",
      "fiscal_year": 2023,
      "quarter": 4
    },
    "versions": [
      {
        "id": "550e8400-e29b-41d4-a716-446655440003",
        "version": 1,
        "file_size": 2048576,
        "file_type": "pdf",
        "created_at": "2023-12-01T12:00:00Z",
        "created_by": {
          "id": "550e8400-e29b-41d4-a716-446655440004",
          "name": "John Doe"
        }
      }
    ],
    "permissions": {
      "read": true,
      "write": true,
      "delete": true,
      "share": true
    }
  }
}
```

#### PUT /documents/{id}

Update document metadata.

**Request**:
```json
{
  "title": "Updated Quarterly Report Q4 2023",
  "description": "Updated Q4 financial performance report",
  "tags": ["q4", "financial", "2023", "updated"],
  "metadata": {
    "department": "Finance",
    "fiscal_year": 2023,
    "quarter": 4,
    "approved": true
  }
}
```

**Response**:
```json
{
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440002",
    "title": "Updated Quarterly Report Q4 2023",
    "description": "Updated Q4 financial performance report",
    "category": {
      "id": "550e8400-e29b-41d4-a716-446655440001",
      "name": "Financial Documents"
    },
    "status": "active",
    "file_size": 2048576,
    "file_type": "pdf",
    "mime_type": "application/pdf",
    "file_path": "/documents/550e8400-e29b-41d4-a716-446655440002.pdf",
    "created_at": "2023-12-01T12:00:00Z",
    "updated_at": "2023-12-01T12:30:00Z",
    "tags": ["q4", "financial", "2023", "updated"],
    "metadata": {
      "department": "Finance",
      "fiscal_year": 2023,
      "quarter": 4,
      "approved": true
    }
  }
}
```

#### DELETE /documents/{id}

Delete a document.

**Response**:
```json
{
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440002",
    "message": "Document deleted successfully"
  }
}
```

#### POST /documents/{id}/upload

Upload file for a document.

**Request**:
```bash
curl -X POST \
  -H "Authorization: Bearer YOUR_ACCESS_TOKEN" \
  -H "Content-Type: multipart/form-data" \
  -F "file=@/path/to/document.pdf" \
  https://api.riva-ash.com/v1/documents/550e8400-e29b-41d4-a716-446655440002/upload
```

**Response**:
```json
{
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440002",
    "title": "Quarterly Report Q4 2023",
    "description": "Q4 financial performance report",
    "category": {
      "id": "550e8400-e29b-41d4-a716-446655440001",
      "name": "Financial Documents"
    },
    "status": "active",
    "file_size": 2048576,
    "file_type": "pdf",
    "mime_type": "application/pdf",
    "file_path": "/documents/550e8400-e29b-41d4-a716-446655440002.pdf",
    "created_at": "2023-12-01T12:00:00Z",
    "updated_at": "2023-12-01T12:30:00Z",
    "tags": ["q4", "financial", "2023"],
    "metadata": {
      "department": "Finance",
      "fiscal_year": 2023,
      "quarter": 4
    },
    "versions": [
      {
        "id": "550e8400-e29b-41d4-a716-446655440003",
        "version": 1,
        "file_size": 2048576,
        "file_type": "pdf",
        "created_at": "2023-12-01T12:30:00Z",
        "created_by": {
          "id": "550e8400-e29b-41d4-a716-446655440004",
          "name": "John Doe"
        }
      }
    ]
  }
}
```

#### GET /documents/{id}/download

Download document file.

**Response**:
- File download with appropriate Content-Type header

#### POST /documents/{id}/share

Share document with other users.

**Request**:
```json
{
  "emails": ["user1@example.com", "user2@example.com"],
  "permission": "read",
  "expires_at": "2023-12-31T23:59:59Z"
}
```

**Response**:
```json
{
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440005",
    "document_id": "550e8400-e29b-41d4-a716-446655440002",
    "shared_with": [
      {
        "email": "user1@example.com",
        "permission": "read",
        "expires_at": "2023-12-31T23:59:59Z"
      },
      {
        "email": "user2@example.com",
        "permission": "read",
        "expires_at": "2023-12-31T23:59:59Z"
      }
    ],
    "created_at": "2023-12-01T12:30:00Z",
    "created_by": {
      "id": "550e8400-e29b-41d4-a716-446655440004",
      "name": "John Doe"
    }
  }
}
```

### Category Endpoints

#### GET /categories

List document categories.

**Response**:
```json
{
  "data": [
    {
      "id": "550e8400-e29b-41d4-a716-446655440001",
      "name": "Financial Documents",
      "description": "Financial and accounting documents",
      "color": "#3182ce",
      "parent_id": null,
      "created_at": "2023-12-01T12:00:00Z",
      "updated_at": "2023-12-01T12:00:00Z",
      "document_count": 45
    },
    {
      "id": "550e8400-e29b-41d4-a716-446655440002",
      "name": "HR Records",
      "description": "Human resources documents",
      "color": "#38a169",
      "parent_id": null,
      "created_at": "2023-12-01T12:00:00Z",
      "updated_at": "2023-12-01T12:00:00Z",
      "document_count": 23
    }
  ]
}
```

#### POST /categories

Create a new category.

**Request**:
```json
{
  "name": "Legal Documents",
  "description": "Legal and compliance documents",
  "color": "#d69e2e",
  "parent_id": null
}
```

**Response**:
```json
{
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440003",
    "name": "Legal Documents",
    "description": "Legal and compliance documents",
    "color": "#d69e2e",
    "parent_id": null,
    "created_at": "2023-12-01T12:30:00Z",
    "updated_at": "2023-12-01T12:30:00Z",
    "document_count": 0
  }
}
```

### User Endpoints

#### GET /users

List users.

**Query Parameters**:
- `page`: Page number (default: 1)
- `per_page`: Items per page (default: 20, max: 100)
- `search`: Search term
- `role`: Filter by role
- `status`: Filter by status (active, inactive)
- `sort`: Sort field (created_at, updated_at, name)
- `order`: Sort order (asc, desc)

**Response**:
```json
{
  "data": [
    {
      "id": "550e8400-e29b-41d4-a716-446655440004",
      "email": "john.doe@example.com",
      "name": "John Doe",
      "role": "user",
      "status": "active",
      "last_login_at": "2023-12-01T12:00:00Z",
      "created_at": "2023-11-01T10:00:00Z",
      "updated_at": "2023-12-01T12:00:00Z"
    }
  ]
}
```

#### POST /users

Create a new user.

**Request**:
```json
{
  "email": "jane.smith@example.com",
  "name": "Jane Smith",
  "password": "securepassword123",
  "role": "user",
  "send_welcome_email": true
}
```

**Response**:
```json
{
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440005",
    "email": "jane.smith@example.com",
    "name": "Jane Smith",
    "role": "user",
    "status": "active",
    "last_login_at": null,
    "created_at": "2023-12-01T12:30:00Z",
    "updated_at": "2023-12-01T12:30:00Z"
  }
}
```

### Audit Endpoints

#### GET /audit

Get audit trail.

**Query Parameters**:
- `page`: Page number (default: 1)
- `per_page`: Items per page (default: 20, max: 100)
- `entity_type`: Filter by entity type
- `entity_id`: Filter by entity ID
- `action`: Filter by action (create, update, delete)
- `user_id`: Filter by user ID
- `created_after`: Filter by creation date (ISO 8601)
- `created_before`: Filter by creation date (ISO 8601)
- `sort`: Sort field (created_at)
- `order`: Sort order (desc)

**Response**:
```json
{
  "data": [
    {
      "id": "550e8400-e29b-41d4-a716-446655440006",
      "entity_type": "document",
      "entity_id": "550e8400-e29b-41d4-a716-446655440002",
      "action": "create",
      "user": {
        "id": "550e8400-e29b-41d4-a716-446655440004",
        "name": "John Doe",
        "email": "john.doe@example.com"
      },
      "changes": {
        "title": "Quarterly Report Q4 2023",
        "description": "Q4 financial performance report",
        "category_id": "550e8400-e29b-41d4-a716-446655440001"
      },
      "ip_address": "192.168.1.100",
      "user_agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
      "created_at": "2023-12-01T12:00:00Z"
    }
  ]
}
```

### System Endpoints

#### GET /health

Health check endpoint.

**Response**:
```json
{
  "data": {
    "status": "healthy",
    "timestamp": "2023-12-01T12:00:00Z",
    "version": "1.0.0",
    "database": {
      "status": "healthy",
      "connection_pool": {
        "size": 10,
        "available": 8,
        "checking_out": 2
      }
    },
    "storage": {
      "status": "healthy",
      "available_space": "100GB",
      "used_space": "25GB"
    }
  }
}
```

#### GET /metrics

Application metrics.

**Response**:
```json
{
  "data": {
    "requests": {
      "total": 15420,
      "rate": "15.42/s"
    },
    "database": {
      "queries": {
        "total": 45620,
        "rate": "45.62/s"
      },
      "connections": {
        "active": 8,
        "idle": 2
      }
    },
    "memory": {
      "usage": "256MB",
      "limit": "1GB"
    },
    "uptime": "15d 4h 32m"
  }
}
```

## Data Models

### User Model

```json
{
  "id": "string (UUID)",
  "email": "string (email)",
  "name": "string",
  "role": "string (admin, manager, user, viewer)",
  "status": "string (active, inactive)",
  "last_login_at": "string (ISO 8601 datetime, nullable)",
  "created_at": "string (ISO 8601 datetime)",
  "updated_at": "string (ISO 8601 datetime)"
}
```

### Document Model

```json
{
  "id": "string (UUID)",
  "title": "string",
  "description": "string (nullable)",
  "category": {
    "id": "string (UUID)",
    "name": "string"
  },
  "status": "string (active, archived, deleted)",
  "file_size": "integer",
  "file_type": "string",
  "mime_type": "string",
  "file_path": "string",
  "tags": ["string"],
  "metadata": "object",
  "retention_period": "integer (days, nullable)",
  "archival_date": "string (ISO 8601 datetime, nullable)",
  "created_at": "string (ISO 8601 datetime)",
  "updated_at": "string (ISO 8601 datetime)",
  "created_by": {
    "id": "string (UUID)",
    "name": "string"
  },
  "versions": [
    {
      "id": "string (UUID)",
      "version": "integer",
      "file_size": "integer",
      "file_type": "string",
      "created_at": "string (ISO 8601 datetime)",
      "created_by": {
        "id": "string (UUID)",
        "name": "string"
      }
    }
  ],
  "permissions": {
    "read": "boolean",
    "write": "boolean",
    "delete": "boolean",
    "share": "boolean"
  }
}
```

### Category Model

```json
{
  "id": "string (UUID)",
  "name": "string",
  "description": "string (nullable)",
  "color": "string (hex color)",
  "parent_id": "string (UUID, nullable)",
  "created_at": "string (ISO 8601 datetime)",
  "updated_at": "string (ISO 8601 datetime)",
  "document_count": "integer"
}
```

### Audit Entry Model

```json
{
  "id": "string (UUID)",
  "entity_type": "string",
  "entity_id": "string (UUID)",
  "action": "string (create, update, delete, login, logout)",
  "user": {
    "id": "string (UUID)",
    "name": "string",
    "email": "string"
  },
  "changes": "object",
  "ip_address": "string (IP address, nullable)",
  "user_agent": "string (nullable)",
  "created_at": "string (ISO 8601 datetime)"
}
```

### Error Model

```json
{
  "error": {
    "code": "string",
    "message": "string",
    "details": "object (nullable)",
    "request_id": "string"
  }
}
```

## Error Handling

### Error Response Format

```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "The request contains invalid data",
    "details": {
      "field": "email",
      "message": "Email is required"
    },
    "request_id": "req_123456789"
  }
}
```

### Common Error Codes

| Code | HTTP Status | Description |
|------|-------------|-------------|
| `VALIDATION_ERROR` | 400 | Request validation failed |
| `UNAUTHORIZED` | 401 | Authentication required |
| `FORBIDDEN` | 403 | Insufficient permissions |
| `NOT_FOUND` | 404 | Resource not found |
| `CONFLICT` | 409 | Resource conflict |
| `RATE_LIMITED` | 429 | Rate limit exceeded |
| `INTERNAL_ERROR` | 500 | Internal server error |
| `SERVICE_UNAVAILABLE` | 503 | Service temporarily unavailable |

### Error Details

#### Validation Error

```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "The request contains invalid data",
    "details": [
      {
        "field": "email",
        "message": "Email is required"
      },
      {
        "field": "email",
        "message": "Email must be a valid email address"
      },
      {
        "field": "password",
        "message": "Password must be at least 8 characters long"
      }
    ],
    "request_id": "req_123456789"
  }
}
```

#### Authentication Error

```json
{
  "error": {
    "code": "UNAUTHORIZED",
    "message": "Invalid credentials",
    "details": {
      "attempts_remaining": 4
    },
    "request_id": "req_123456789"
  }
}
```

#### Permission Error

```json
{
  "error": {
    "code": "FORBIDDEN",
    "message": "You don't have permission to perform this action",
    "details": {
      "required_permission": "documents:delete",
      "user_role": "user"
    },
    "request_id": "req_123456789"
  }
}
```

## Rate Limiting

### Rate Limit Headers

```
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 95
X-RateLimit-Reset: 1672531200
```

### Rate Limit Response

When rate limited, the API returns a 429 status code:

```json
{
  "error": {
    "code": "RATE_LIMITED",
    "message": "Rate limit exceeded",
    "details": {
      "limit": 100,
      "window": "60s",
      "reset_at": "2023-12-31T23:59:59Z"
    },
    "request_id": "req_123456789"
  }
}
```

### Rate Limit Endpoints

| Endpoint | Limit | Window |
|----------|-------|--------|
| Authentication | 5 | 5 minutes |
| Document Operations | 100 | 1 minute |
| User Operations | 50 | 1 minute |
| Category Operations | 30 | 1 minute |
| Audit Operations | 200 | 1 minute |
| System Operations | 1000 | 1 minute |

## Webhooks

### Webhook Configuration

```json
{
  "url": "https://your-webhook-endpoint.com",
  "events": [
    "document.created",
    "document.updated",
    "document.deleted",
    "user.created",
    "user.updated",
    "user.deleted"
  ],
  "secret": "your-webhook-secret",
  "retry_count": 3,
  "timeout": 30
}
```

### Webhook Payload

#### Document Created

```json
{
  "event": "document.created",
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440002",
    "title": "Quarterly Report Q4 2023",
    "description": "Q4 financial performance report",
    "category": {
      "id": "550e8400-e29b-41d4-a716-446655440001",
      "name": "Financial Documents"
    },
    "status": "active",
    "file_size": 2048576,
    "file_type": "pdf",
    "created_at": "2023-12-01T12:00:00Z",
    "created_by": {
      "id": "550e8400-e29b-41d4-a716-446655440004",
      "name": "John Doe"
    }
  },
  "timestamp": "2023-12-01T12:00:00Z"
}
```

### Webhook Verification

To verify webhook authenticity, compute the HMAC-SHA256 hash of the payload using your webhook secret:

```bash
echo -n "$(cat payload.json)" | openssl dgst -sha256 -hmac "your-webhook-secret"
```

## SDKs and Libraries

### JavaScript/TypeScript

```javascript
import { RivaAshClient } from '@riva-ash/sdk';

const client = new RivaAshClient({
  apiKey: 'your-api-key',
  baseUrl: 'https://api.riva-ash.com/v1'
});

// Get documents
const documents = await client.documents.list({
  page: 1,
  perPage: 20,
  search: 'quarterly'
});

// Create document
const document = await client.documents.create({
  title: 'New Document',
  description: 'Document description',
  categoryId: 'category-id'
});

// Upload file
await client.documents.upload(document.id, file);
```

### Python

```python
from riva_ash import RivaAshClient

client = RivaAshClient(
    api_key='your-api-key',
    base_url='https://api.riva-ash.com/v1'
)

# Get documents
documents = client.documents.list(
    page=1,
    per_page=20,
    search='quarterly'
)

# Create document
document = client.documents.create(
    title='New Document',
    description='Document description',
    category_id='category-id'
)

# Upload file
client.documents.upload(document.id, file)
```

### Ruby

```ruby
require 'riva_ash'

client = RivaAsh::Client.new(
  api_key: 'your-api-key',
  base_url: 'https://api.riva-ash.com/v1'
)

# Get documents
documents = client.documents.list(
  page: 1,
  per_page: 20,
  search: 'quarterly'
)

# Create document
document = client.documents.create(
  title: 'New Document',
  description: 'Document description',
  category_id: 'category-id'
)

# Upload file
client.documents.upload(document.id, file)
```

## Examples

### Complete Document Workflow

```bash
#!/bin/bash
# Create document workflow example

# 1. Login
LOGIN_RESPONSE=$(curl -s -X POST \
  -H "Content-Type: application/json" \
  -d '{
    "email": "user@example.com",
    "password": "password123"
  }' \
  https://api.riva-ash.com/v1/auth/login)

ACCESS_TOKEN=$(echo $LOGIN_RESPONSE | jq -r '.data.access_token')

# 2. Create document
DOC_RESPONSE=$(curl -s -X POST \
  -H "Authorization: Bearer $ACCESS_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "title": "Quarterly Report Q4 2023",
    "description": "Q4 financial performance report",
    "category_id": "550e8400-e29b-41d4-a716-446655440001",
    "tags": ["q4", "financial", "2023"]
  }' \
  https://api.riva-ash.com/v1/documents)

DOC_ID=$(echo $DOC_RESPONSE | jq -r '.data.id')

# 3. Upload file
curl -s -X POST \
  -H "Authorization: Bearer $ACCESS_TOKEN" \
  -H "Content-Type: multipart/form-data" \
  -F "file=@/path/to/document.pdf" \
  https://api.riva-ash.com/v1/documents/$DOC_ID/upload

# 4. Share document
curl -s -X POST \
  -H "Authorization: Bearer $ACCESS_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "emails": ["colleague@example.com"],
    "permission": "read",
    "expires_at": "2023-12-31T23:59:59Z"
  }' \
  https://api.riva-ash.com/v1/documents/$DOC_ID/share

echo "Document workflow completed successfully"
```

### Batch Document Processing

```python
import requests
import os
from pathlib import Path

class RivaAshBatchProcessor:
    def __init__(self, api_key, base_url):
        self.api_key = api_key
        self.base_url = base_url
        self.headers = {
            'Authorization': f'Bearer {api_key}',
            'Content-Type': 'application/json'
        }
    
    def process_directory(self, directory_path, category_id):
        """Process all files in a directory"""
        directory = Path(directory_path)
        
        for file_path in directory.glob('*'):
            if file_path.is_file():
                self.process_file(file_path, category_id)
    
    def process_file(self, file_path, category_id):
        """Process a single file"""
        # Create document
        doc_data = {
            'title': file_path.stem,
            'description': f'Auto-generated document for {file_path.name}',
            'category_id': category_id,
            'tags': ['auto-imported']
        }
        
        response = requests.post(
            f'{self.base_url}/documents',
            json=doc_data,
            headers=self.headers
        )
        
        if response.status_code == 201:
            doc_id = response.json()['data']['id']
            
            # Upload file
            with open(file_path, 'rb') as f:
                files = {'file': f}
                upload_response = requests.post(
                    f'{self.base_url}/documents/{doc_id}/upload',
                    files=files,
                    headers=self.headers
                )
                
                if upload_response.status_code == 200:
                    print(f'Successfully processed: {file_path.name}')
                else:
                    print(f'Upload failed for: {file_path.name}')
        else:
            print(f'Document creation failed for: {file_path.name}')

# Usage
processor = RivaAshBatchProcessor(
    api_key='your-api-key',
    base_url='https://api.riva-ash.com/v1'
)

processor.process_directory('/path/to/documents', 'category-id')
```

## Changelog

### Version 1.0.0 (2023-12-01)

- Initial API release
- Authentication endpoints
- Document management endpoints
- Category management endpoints
- User management endpoints
- Audit trail endpoints
- System endpoints
- Rate limiting
- Webhook support
- SDKs for JavaScript, Python, and Ruby

### Version 0.9.0 (2023-11-15)

- Beta release
- Core functionality
- Authentication
- Document management
- Basic audit logging

### Version 0.8.0 (2023-10-01)

- Alpha release
- Basic API structure
- Authentication prototype
- Document upload prototype

This API reference provides comprehensive documentation for the Riva Ash document management system. For additional questions or support, please contact our API support team.