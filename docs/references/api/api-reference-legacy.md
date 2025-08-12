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
  -F "file='path/to/document.pdf'" (see below for file content) \
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
      "description": "Financial reports and statements",
      "color": "#3B82F6",
      "created_at": "2023-12-01T12:00:00Z",
      "updated_at": "2023-12-01T12:00:00Z",
      "document_count": 45
    },
    {
      "id": "550e8400-e29b-41d4-a716-446655440002",
      "name": "Legal Documents",
      "description": "Contracts and legal agreements",
      "color": "#EF4444",
      "created_at": "2023-12-01T12:00:00Z",
      "updated_at": "2023-12-01T12:00:00Z",
      "document_count": 23
    }
  ]
}
```

#### POST /categories

Create a new document category.

**Request**:
```json
{
  "name": "Marketing Materials",
  "description": "Marketing campaigns and materials",
  "color": "#10B981"
}
```

**Response**:
```json
{
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440003",
    "name": "Marketing Materials",
    "description": "Marketing campaigns and materials",
    "color": "#10B981",
    "created_at": "2023-12-01T12:30:00Z",
    "updated_at": "2023-12-01T12:30:00Z",
    "document_count": 0
  }
}
```

#### GET /categories/{id}

Get category details.

**Response**:
```json
{
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440001",
    "name": "Financial Documents",
    "description": "Financial reports and statements",
    "color": "#3B82F6",
    "created_at": "2023-12-01T12:00:00Z",
    "updated_at": "2023-12-01T12:00:00Z",
    "document_count": 45,
    "permissions": {
      "read": true,
      "write": true,
      "delete": true
    }
  }
}
```

#### PUT /categories/{id}

Update category details.

**Request**:
```json
{
  "name": "Financial Reports",
  "description": "Financial reports and statements",
  "color": "#2563EB"
}
```

**Response**:
```json
{
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440001",
    "name": "Financial Reports",
    "description": "Financial reports and statements",
    "color": "#2563EB",
    "created_at": "2023-12-01T12:00:00Z",
    "updated_at": "2023-12-01T12:30:00Z",
    "document_count": 45
  }
}
```

#### DELETE /categories/{id}

Delete a category.

**Response**:
```json
{
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440001",
    "message": "Category deleted successfully"
  }
}
```

### User Endpoints

#### GET /users

List users with filtering and pagination.

**Query Parameters**:
- `page`: Page number (default: 1)
- `per_page`: Items per page (default: 20, max: 100)
- `search`: Search term
- `role`: Filter by role (admin, user, guest)
- `status`: Filter by status (active, inactive, suspended)
- `created_after`: Filter by creation date (ISO 8601)
- `created_before`: Filter by creation date (ISO 8601)
- `sort`: Sort field (created_at, updated_at, name, email)
- `order`: Sort order (asc, desc)

**Response**:
```json
{
  "data": [
    {
      "id": "550e8400-e29b-41d4-a716-446655440004",
      "name": "John Doe",
      "email": "john.doe@example.com",
      "role": "admin",
      "status": "active",
      "avatar": null,
      "created_at": "2023-12-01T12:00:00Z",
      "updated_at": "2023-12-01T12:00:00Z",
      "last_login": "2023-12-01T11:30:00Z"
    }
  ],
  "meta": {
    "total": 25,
    "page": 1,
    "per_page": 20,
    "total_pages": 2
  }
}
```

#### POST /users

Create a new user.

**Request**:
```json
{
  "name": "Jane Smith",
  "email": "jane.smith@example.com",
  "password": "securepassword123",
  "role": "user",
  "status": "active"
}
```

**Response**:
```json
{
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440005",
    "name": "Jane Smith",
    "email": "jane.smith@example.com",
    "role": "user",
    "status": "active",
    "avatar": null,
    "created_at": "2023-12-01T12:30:00Z",
    "updated_at": "2023-12-01T12:30:00Z",
    "last_login": null
  }
}
```

#### GET /users/{id}

Get user details.

**Response**:
```json
{
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440004",
    "name": "John Doe",
    "email": "john.doe@example.com",
    "role": "admin",
    "status": "active",
    "avatar": null,
    "created_at": "2023-12-01T12:00:00Z",
    "updated_at": "2023-12-01T12:00:00Z",
    "last_login": "2023-12-01T11:30:00Z",
    "profile": {
      "department": "IT",
      "position": "System Administrator",
      "phone": "+1-555-0123",
      "bio": "System administrator with 10+ years of experience"
    },
    "preferences": {
      "theme": "dark",
      "language": "en",
      "timezone": "UTC"
    },
    "permissions": {
      "can_manage_users": true,
      "can_manage_documents": true,
      "can_view_reports": true,
      "can_manage_settings": true
    }
  }
}
```

#### PUT /users/{id}

Update user details.

**Request**:
```json
{
  "name": "Johnathan Doe",
  "role": "admin",
  "status": "active",
  "profile": {
    "department": "IT",
    "position": "Senior System Administrator",
    "phone": "+1-555-0124",
    "bio": "Senior system administrator with 15+ years of experience"
  },
  "preferences": {
    "theme": "light",
    "language": "en",
    "timezone": "America/New_York"
  }
}
```

**Response**:
```json
{
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440004",
    "name": "Johnathan Doe",
    "email": "john.doe@example.com",
    "role": "admin",
    "status": "active",
    "avatar": null,
    "created_at": "2023-12-01T12:00:00Z",
    "updated_at": "2023-12-01T12:30:00Z",
    "last_login": "2023-12-01T11:30:00Z",
    "profile": {
      "department": "IT",
      "position": "Senior System Administrator",
      "phone": "+1-555-0124",
      "bio": "Senior system administrator with 15+ years of experience"
    },
    "preferences": {
      "theme": "light",
      "language": "en",
      "timezone": "America/New_York"
    }
  }
}
```

#### DELETE /users/{id}

Delete a user.

**Response**:
```json
{
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440005",
    "message": "User deleted successfully"
  }
}
```

#### POST /users/{id}/password

Change user password.

**Request**:
```json
{
  "current_password": "oldpassword123",
  "new_password": "newsecurepassword456"
}
```

**Response**:
```json
{
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440004",
    "message": "Password updated successfully"
  }
}
```

#### POST /users/{id}/reset-password

Send password reset email.

**Request**:
```json
{
  "reset_url": "https://app.riva-ash.com/reset-password"
}
```

**Response**:
```json
{
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440004",
    "message": "Password reset email sent successfully"
  }
}
```

### Search Endpoints

#### GET /search

Perform global search across documents, users, and categories.

**Query Parameters**:
- `q`: Search query (required)
- `type`: Filter by type (documents, users, categories, all)
- `page`: Page number (default: 1)
- `per_page`: Items per page (default: 20, max: 100)
- `sort`: Sort field (relevance, created_at, updated_at, name)
- `order`: Sort order (asc, desc)

**Response**:
```json
{
  "data": {
    "documents": [
      {
        "id": "550e8400-e29b-41d4-a716-446655440002",
        "title": "Quarterly Report Q4 2023",
        "description": "Q4 financial performance report",
        "category": {
          "id": "550e8400-e29b-41d4-a716-446655440001",
          "name": "Financial Documents"
        },
        "score": 0.95,
        "highlight": {
          "title": "Quarterly <mark>Report</mark> Q4 2023",
          "description": "Q4 financial performance <mark>report</mark>"
        }
      }
    ],
    "users": [
      {
        "id": "550e8400-e29b-41d4-a716-446655440004",
        "name": "John Doe",
        "email": "john.doe@example.com",
        "score": 0.85,
        "highlight": {
          "name": "John <mark>Doe</mark>",
          "email": "john.<mark>doe</mark>@example.com"
        }
      }
    ],
    "categories": [
      {
        "id": "550e8400-e29b-41d4-a716-446655440001",
        "name": "Financial Documents",
        "description": "Financial reports and statements",
        "score": 0.75,
        "highlight": {
          "name": "Financial <mark>Documents</mark>",
          "description": "Financial reports and statements"
        }
      }
    ]
  },
  "meta": {
    "total": 15,
    "page": 1,
    "per_page": 20,
    "query": "report",
    "execution_time": 0.045
  }
}
```

#### GET /search/suggestions

Get search suggestions based on query.

**Query Parameters**:
- `q`: Search query (required)
- `limit`: Maximum number of suggestions (default: 10, max: 50)

**Response**:
```json
{
  "data": [
    {
      "text": "quarterly report",
      "type": "document",
      "count": 5
    },
    {
      "text": "annual report",
      "type": "document",
      "count": 3
    },
    {
      "text": "financial report",
      "type": "document",
      "count": 8
    },
    {
      "text": "report template",
      "type": "document",
      "count": 2
    }
  ]
}
```

### Audit Log Endpoints

#### GET /audit-logs

Get audit logs with filtering and pagination.

**Query Parameters**:
- `page`: Page number (default: 1)
- `per_page`: Items per page (default: 20, max: 100)
- `user_id`: Filter by user ID
- `action`: Filter by action (create, update, delete, login, logout)
- `resource_type`: Filter by resource type (document, user, category)
- `resource_id`: Filter by resource ID
- `created_after`: Filter by creation date (ISO 8601)
- `created_before`: Filter by creation date (ISO 8601)
- `sort`: Sort field (created_at, updated_at)
- `order`: Sort order (asc, desc)

**Response**:
```json
{
  "data": [
    {
      "id": "550e8400-e29b-41d4-a716-446655440006",
      "user": {
        "id": "550e8400-e29b-41d4-a716-446655440004",
        "name": "John Doe",
        "email": "john.doe@example.com"
      },
      "action": "create",
      "resource_type": "document",
      "resource_id": "550e8400-e29b-41d4-a716-446655440002",
      "resource_name": "Quarterly Report Q4 2023",
      "ip_address": "192.168.1.100",
      "user_agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
      "metadata": {
        "changes": {
          "title": "Quarterly Report Q4 2023",
          "description": "Q4 financial performance report",
          "category_id": "550e8400-e29b-41d4-a716-446655440001"
        }
      },
      "created_at": "2023-12-01T12:30:00Z"
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

#### GET /audit-logs/{id}

Get audit log entry details.

**Response**:
```json
{
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440006",
    "user": {
      "id": "550e8400-e29b-41d4-a716-446655440004",
      "name": "John Doe",
      "email": "john.doe@example.com"
    },
    "action": "create",
    "resource_type": "document",
    "resource_id": "550e8400-e29b-41d4-a716-446655440002",
    "resource_name": "Quarterly Report Q4 2023",
    "ip_address": "192.168.1.100",
    "user_agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
    "metadata": {
      "changes": {
        "title": "Quarterly Report Q4 2023",
        "description": "Q4 financial performance report",
        "category_id": "550e8400-e29b-41d4-a716-446655440001"
      }
    },
    "created_at": "2023-12-01T12:30:00Z"
  }
}
```

### System Endpoints

#### GET /health

Check system health status.

**Response**:
```json
{
  "data": {
    "status": "healthy",
    "version": "1.0.0",
    "timestamp": "2023-12-01T12:00:00Z",
    "checks": {
      "database": {
        "status": "healthy",
        "response_time": 5
      },
      "storage": {
        "status": "healthy",
        "available_space": "85%"
      },
      "cache": {
        "status": "healthy",
        "hit_rate": "95%"
      }
    }
  }
}
```

#### GET /metrics

Get system metrics.

**Response**:
```json
{
  "data": {
    "system": {
      "uptime": 86400,
      "cpu_usage": 25.5,
      "memory_usage": 65.2,
      "disk_usage": 45.8
    },
    "application": {
      "requests_per_minute": 150,
      "response_time_avg": 120,
      "error_rate": 0.5
    },
    "database": {
      "connections": 15,
      "queries_per_second": 45,
      "slow_queries": 2
    }
  }
}
```

#### GET /version

Get application version information.

**Response**:
```json
{
  "data": {
    "version": "1.0.0",
    "build": "20231201.1",
    "commit": "a1b2c3d4e5f6",
    "branch": "main",
    "build_date": "2023-12-01T12:00:00Z"
  }
}
```

## Data Models

### Document Model

```json
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "title": "Document Title",
  "description": "Document description",
  "category": {
    "id": "550e8400-e29b-41d4-a716-446655440001",
    "name": "Category Name"
  },
  "status": "active",
  "file_size": 2048576,
  "file_type": "pdf",
  "mime_type": "application/pdf",
  "file_path": "/documents/550e8400-e29b-41d4-a716-446655440000.pdf",
  "tags": ["tag1", "tag2", "tag3"],
  "metadata": {
    "key": "value"
  },
  "versions": [
    {
      "id": "550e8400-e29b-41d4-a716-446655440001",
      "version": 1,
      "file_size": 2048576,
      "file_type": "pdf",
      "created_at": "2023-12-01T12:00:00Z",
      "created_by": {
        "id": "550e8400-e29b-41d4-a716-446655440002",
        "name": "John Doe"
      }
    }
  ],
  "permissions": {
    "read": true,
    "write": true,
    "delete": true,
    "share": true
  },
  "created_at": "2023-12-01T12:00:00Z",
  "updated_at": "2023-12-01T12:00:00Z",
  "created_by": {
    "id": "550e8400-e29b-41d4-a716-446655440002",
    "name": "John Doe"
  }
}
```

### User Model

```json
{
  "id": "550e8400-e29b-41d4-a716-446655440