For general API information including authentication, base URLs, and common patterns, see the main [API Reference](API_REFERENCE.md).

# JSON:API Usage Guide

This guide provides comprehensive instructions for using the JSON:API implementation in Riva Ash. It covers resource identification, relationship handling, filtering, sorting, pagination, and error handling.

## Table of Contents

1. [Overview](#overview)
2. [Resource Structure](#resource-structure)
3. [Relationships](#relationships)
4. [Filtering](#filtering)
5. [Sorting](#sorting)
6. [Pagination](#pagination)
7. [Sparse Fieldsets](#sparse-fieldsets)
8. [Include Relationships](#include-relationships)
9. [Error Handling](#error-handling)
10. [Best Practices](#best-practices)
11. [Examples](#examples)

## Overview

Riva Ash implements JSON:API specification (version 1.0) for consistent and efficient API communication. JSON:API is a standardized format that minimizes both the number of requests and the amount of data transmitted between clients and servers.

### Key Features

- **Resource-oriented**: All data is represented as resources
- **Relationships**: Clear relationship definitions between resources
- **Consistent**: Standardized request/response format
- **Efficient**: Optimized data transfer with sparse fieldsets
- **Extensible**: Support for custom metadata and extensions

### Base URL

For the complete list of API endpoints and base URLs, see the [API Reference](API_REFERENCE.md#api-endpoints).

```
https://api.riva-ash.com/api/v1
```

## Resource Structure

### Resource Object

A JSON:API resource object has the following structure:

```json
{
  "data": {
    "type": "documents",
    "id": "550e8400-e29b-41d4-a716-446655440000",
    "attributes": {
      "title": "Annual Report 2023",
      "description": "Company annual financial report",
      "status": "active",
      "fileSize": 2048576,
      "fileType": "pdf",
      "createdAt": "2023-12-01T12:00:00Z",
      "updatedAt": "2023-12-01T12:00:00Z"
    },
    "relationships": {
      "category": {
        "data": {
          "type": "categories",
          "id": "550e8400-e29b-41d4-a716-446655440001"
        }
      },
      "createdBy": {
        "data": {
          "type": "users",
          "id": "550e8400-e29b-41d4-a716-446655440002"
        }
      }
    },
    "links": {
      "self": "/api/v1/documents/550e8400-e29b-41d4-a716-446655440000"
    }
  }
}
```

### Resource Identifier Object

A resource identifier object identifies a resource:

```json
{
  "type": "documents",
  "id": "550e8400-e29b-41d4-a716-446655440000"
}
```

### Resource Collection

A collection of resources:

```json
{
  "data": [
    {
      "type": "documents",
      "id": "550e8400-e29b-41d4-a716-446655440000",
      "attributes": {
        "title": "Annual Report 2023",
        "status": "active"
      },
      "relationships": {
        "category": {
          "data": {
            "type": "categories",
            "id": "550e8400-e29b-41d4-a716-446655440001"
          }
        }
      }
    },
    {
      "type": "documents",
      "id": "550e8400-e29b-41d4-a716-446655440001",
      "attributes": {
        "title": "Quarterly Report Q4 2023",
        "status": "active"
      },
      "relationships": {
        "category": {
          "data": {
            "type": "categories",
            "id": "550e8400-e29b-41d4-a716-446655440002"
          }
        }
      }
    }
  ],
  "links": {
    "self": "/api/v1/documents",
    "next": "/api/v1/documents?page=2"
  },
  "meta": {
    "total": 150,
    "page": 1,
    "perPage": 20
  }
}
```

## Relationships

### Relationship Objects

Relationships define connections between resources:

```json
{
  "data": {
    "type": "documents",
    "id": "550e8400-e29b-41d4-a716-446655440000",
    "relationships": {
      "category": {
        "data": {
          "type": "categories",
          "id": "550e8400-e29b-41d4-a716-446655440001"
        },
        "links": {
          "self": "/api/v1/documents/550e8400-e29b-41d4-a716-446655440000/relationships/category",
          "related": "/api/v1/documents/550e8400-e29b-41d4-a716-446655440000/category"
        }
      },
      "createdBy": {
        "data": {
          "type": "users",
          "id": "550e8400-e29b-41d4-a716-446655440002"
        },
        "links": {
          "self": "/api/v1/documents/550e8400-e29b-41d4-a716-446655440000/relationships/createdBy",
          "related": "/api/v1/documents/550e8400-e29b-41d4-a716-446655440000/createdBy"
        }
      }
    }
  }
}
```

### To-Many Relationships

For relationships that can have multiple related resources:

```json
{
  "data": {
    "type": "categories",
    "id": "550e8400-e29b-41d4-a716-446655440001",
    "relationships": {
      "documents": {
        "data": [
          {
            "type": "documents",
            "id": "550e8400-e29b-41d4-a716-446655440000"
          },
          {
            "type": "documents",
            "id": "550e8400-e29b-41d4-a716-446655440001"
          }
        ],
        "links": {
          "self": "/api/v1/categories/550e8400-e29b-41d4-a716-446655440001/relationships/documents",
          "related": "/api/v1/categories/550e8400-e29b-41d4-a716-446655440001/documents"
        }
      }
    }
  }
}
```

### Including Related Resources

Use the `include` parameter to include related resources:

```
GET /api/v1/documents?include=category,createdBy
```

```json
{
  "data": [
    {
      "type": "documents",
      "id": "550e8400-e29b-41d4-a716-446655440000",
      "attributes": {
        "title": "Annual Report 2023",
        "status": "active"
      },
      "relationships": {
        "category": {
          "data": {
            "type": "categories",
            "id": "550e8400-e29b-41d4-a716-446655440001"
          }
        },
        "createdBy": {
          "data": {
            "type": "users",
            "id": "550e8400-e29b-41d4-a716-446655440002"
          }
        }
      }
    }
  ],
  "included": [
    {
      "type": "categories",
      "id": "550e8400-e29b-41d4-a716-446655440001",
      "attributes": {
        "name": "Financial Documents",
        "description": "Financial and accounting documents"
      }
    },
    {
      "type": "users",
      "id": "550e8400-e29b-41d4-a716-446655440002",
      "attributes": {
        "name": "John Doe",
        "email": "john.doe@example.com"
      }
    }
  ]
}
```

## Filtering

### Basic Filtering

Use the `filter` parameter to filter resources:

```
GET /api/v1/documents?filter[status]=active
```

### Available Filters

| Resource | Filter | Type | Description |
|----------|--------|------|-------------|
| `documents` | `status` | string | Filter by status (active, archived, deleted) |
| `documents` | `category` | string | Filter by category ID |
| `documents` | `createdBy` | string | Filter by creator user ID |
| `documents` | `createdAt` | string | Filter by creation date (ISO 8601) |
| `documents` | `updatedAt` | string | Filter by update date (ISO 8601) |
| `documents` | `title` | string | Filter by title (case-insensitive) |
| `documents` | `tags` | array | Filter by tags |
| `users` | `role` | string | Filter by role (admin, manager, user, viewer) |
| `users` | `status` | string | Filter by status (active, inactive) |
| `categories` | `parent` | string | Filter by parent category ID |

### Filter Operators

#### Equality

```
GET /api/v1/documents?filter[status]=active
GET /api/v1/documents?filter[category]=550e8400-e29b-41d4-a716-446655440001
```

#### Inclusion

```
GET /api/v1/documents?filter[status][in]=active,archived
GET /api/v1/documents?filter[category][in]=550e8400-e29b-41d4-a716-446655440001,550e8400-e29b-41d4-a716-446655440002
```

#### Exclusion

```
GET /api/v1/documents?filter[status][not]=deleted
GET /api/v1/documents?filter[category][not]=550e8400-e29b-41d4-a716-446655440001
```

#### Greater Than/Less Than

```
GET /api/v1/documents?filter[fileSize][gt]=1048576
GET /api/v1/documents?filter[fileSize][lt]=10485760
GET /api/v1/documents?filter[createdAt][gt]=2023-01-01T00:00:00Z
GET /api/v1/documents?filter[createdAt][lt]=2023-12-31T23:59:59Z
```

#### Contains

```
GET /api/v1/documents?filter[title][contains]=Report
GET /api/v1/documents?filter[tags][contains]=financial
```

#### StartsWith/EndsWith

```
GET /api/v1/documents?filter[title][startsWith]=Annual
GET /api/v1/documents?filter[title][endsWith]=2023
```

### Complex Filtering

Combine multiple filters:

```
GET /api/v1/documents?filter[status]=active&filter[category]=550e8400-e29b-41d4-a716-446655440001&filter[createdAt][gt]=2023-01-01T00:00:00Z
```

Use OR logic:

```
GET /api/v1/documents?filter[or][0][status]=active&filter[or][1][status]=archived
```

## Sorting

### Basic Sorting

Use the `sort` parameter to sort resources:

```
GET /api/v1/documents?sort=createdAt
GET /api/v1/documents?sort=-createdAt
```

### Available Sort Fields

| Resource | Field | Description |
|----------|-------|-------------|
| `documents` | `createdAt` | Creation date |
| `documents` | `updatedAt` | Update date |
| `documents` | `title` | Title |
| `documents` | `fileSize` | File size |
| `users` | `createdAt` | Creation date |
| `users` | `updatedAt` | Update date |
| `users` | `name` | Name |
| `users` | `email` | Email |
| `categories` | `createdAt` | Creation date |
| `categories` | `updatedAt` | Update date |
| `categories` | `name` | Name |

### Multiple Sort Fields

Sort by multiple fields:

```
GET /api/v1/documents?sort=category,-createdAt
```

### Custom Sorting

For complex sorting requirements, use custom sort parameters:

```
GET /api/v1/documents?sort[custom]=documentCount
```

## Pagination

### Offset Pagination

Use `page[number]` and `page[size]` for offset pagination:

```
GET /api/v1/documents?page[number]=1&page[size]=20
```

### Cursor Pagination

For large datasets, use cursor pagination:

```
GET /api/v1/documents?page[cursor]=cursor-value&page[size]=20
```

### Pagination Links

```json
{
  "data": [...],
  "links": {
    "self": "/api/v1/documents?page[number]=1&page[size]=20",
    "first": "/api/v1/documents?page[number]=1&page[size]=20",
    "prev": null,
    "next": "/api/v1/documents?page[number]=2&page[size]=20",
    "last": "/api/v1/documents?page[number]=8&page[size]=20"
  },
  "meta": {
    "total": 150,
    "page": 1,
    "perPage": 20,
    "totalPages": 8
  }
}
```

### Pagination Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `page[number]` | integer | 1 | Page number |
| `page[size]` | integer | 20 | Items per page (max: 100) |
| `page[cursor]` | string | - | Cursor for cursor pagination |

## Sparse Fieldsets

### Requesting Specific Fields

Use the `fields` parameter to request only specific fields:

```
GET /api/v1/documents?fields[documents]=title,status,createdAt
GET /api/v1/users?fields[users]=name,email
```

### Including Related Fields

```
GET /api/v1/documents?include=category&fields[documents]=title,status&fields[categories]=name
```

### Response with Sparse Fieldsets

```json
{
  "data": [
    {
      "type": "documents",
      "id": "550e8400-e29b-41d4-a716-446655440000",
      "attributes": {
        "title": "Annual Report 2023",
        "status": "active",
        "createdAt": "2023-12-01T12:00:00Z"
      },
      "relationships": {
        "category": {
          "data": {
            "type": "categories",
            "id": "550e8400-e29b-41d4-a716-446655440001"
          }
        }
      }
    }
  ],
  "included": [
    {
      "type": "categories",
      "id": "550e8400-e29b-41d4-a716-446655440001",
      "attributes": {
        "name": "Financial Documents"
      }
    }
  ]
}
```

## Include Relationships

### Including Single Level Relationships

```
GET /api/v1/documents?include=category
```

### Including Multiple Relationships

```
GET /api/v1/documents?include=category,createdBy
```

### Including Nested Relationships

```
GET /api/v1/documents?include=category.parent,createdBy.role
```

### Including To-Many Relationships

```
GET /api/v1/categories?include=documents
```

### Conditional Including

```
GET /api/v1/documents?include=category,createdBy&include[documents]=category
```

## Error Handling

### Error Object Structure

```json
{
  "errors": [
    {
      "id": "error-1",
      "status": "400",
      "code": "VALIDATION_ERROR",
      "title": "Validation Error",
      "detail": "Title is required",
      "source": {
        "pointer": "/data/attributes/title"
      },
      "meta": {
        "validation": {
          "rule": "required",
          "value": null
        }
      }
    }
  ]
}
```

### Common Error Types

| Status Code | Error Type | Description |
|-------------|------------|-------------|
| 400 | `VALIDATION_ERROR` | Request validation failed |
| 401 | `UNAUTHORIZED` | Authentication required |
| 403 | `FORBIDDEN` | Insufficient permissions |
| 404 | `NOT_FOUND` | Resource not found |
| 409 | `CONFLICT` | Resource conflict |
| 422 | `UNPROCESSABLE_ENTITY` | Semantic error in request |
| 429 | `RATE_LIMITED` | Rate limit exceeded |
| 500 | `INTERNAL_ERROR` | Internal server error |
| 503 | `SERVICE_UNAVAILABLE` | Service temporarily unavailable |

### Error Response Examples

#### Validation Error

```json
{
  "errors": [
    {
      "id": "error-1",
      "status": "400",
      "code": "VALIDATION_ERROR",
      "title": "Validation Error",
      "detail": "Title is required",
      "source": {
        "pointer": "/data/attributes/title"
      }
    },
    {
      "id": "error-2",
      "status": "400",
      "code": "VALIDATION_ERROR",
      "title": "Validation Error",
      "detail": "File size exceeds maximum allowed size",
      "source": {
        "pointer": "/data/attributes/fileSize"
      },
      "meta": {
        "maxSize": 104857600
      }
    }
  ]
}
```

#### Relationship Error

```json
{
  "errors": [
    {
      "id": "error-1",
      "status": "404",
      "code": "NOT_FOUND",
      "title": "Related Resource Not Found",
      "detail": "Category with ID '550e8400-e29b-41d4-a716-446655440999' not found",
      "source": {
        "pointer": "/data/relationships/category/data/id"
      }
    }
  ]
}
```

#### Rate Limit Error

```json
{
  "errors": [
    {
      "id": "error-1",
      "status": "429",
      "code": "RATE_LIMITED",
      "title": "Rate Limit Exceeded",
      "detail": "You have exceeded the rate limit for this endpoint",
      "source": {
        "pointer": "/data"
      },
      "meta": {
        "limit": 100,
        "window": "60s",
        "resetAt": "2023-12-31T23:59:59Z"
      }
    }
  ]
}
```

## Best Practices

### 1. Use Efficient Queries

- Use sparse fieldsets to minimize data transfer
- Include only necessary relationships
- Use appropriate filtering to reduce result sets
- Implement client-side caching for frequently accessed data

### 2. Handle Errors Gracefully

- Always check for error responses
- Implement retry logic for rate-limited requests
- Provide meaningful error messages to users
- Log errors for debugging purposes

### 3. Implement Proper Caching

- Use ETag headers for conditional requests
- Implement client-side caching strategies
- Respect cache control headers
- Use appropriate cache invalidation strategies

### 4. Use Pagination Correctly

- Always use pagination for large result sets
- Implement infinite scrolling for better UX
- Use cursor pagination for large datasets
- Provide clear pagination controls

### 5. Optimize Network Requests

- Batch related requests when possible
- Use parallel requests for independent data
- Implement request deduplication
- Use compression for large payloads

## Examples

### Complete Document Query Example

```bash
#!/bin/bash
# Comprehensive document query example

# Get documents with filtering, sorting, pagination, and includes
curl -s -X GET \
  -H "Authorization: Bearer YOUR_ACCESS_TOKEN" \
  -H "Content-Type: application/vnd.api+json" \
  -H "Accept: application/vnd.api+json" \
  "https://api.riva-ash.com/api/v1/documents?\
filter[status]=active&\
filter[category]=550e8400-e29b-41d4-a716-446655440001&\
filter[createdAt][gt]=2023-01-01T00:00:00Z&\
sort=-createdAt&\
page[number]=1&\
page[size]=10&\
include=category,createdBy&\
fields[documents]=title,status,createdAt,fileSize&\
fields[categories]=name&\
fields[users]=name,email" | jq '.'
```

### Create Document with Relationships

```bash
#!/bin/bash
# Create document with relationships

curl -s -X POST \
  -H "Authorization: Bearer YOUR_ACCESS_TOKEN" \
  -H "Content-Type: application/vnd.api+json" \
  -H "Accept: application/vnd.api+json" \
  -d '{
    "data": {
      "type": "documents",
      "attributes": {
        "title": "Quarterly Report Q4 2023",
        "description": "Q4 financial performance report",
        "status": "active"
      },
      "relationships": {
        "category": {
          "data": {
            "type": "categories",
            "id": "550e8400-e29b-41d4-a716-446655440001"
          }
        },
        "createdBy": {
          "data": {
            "type": "users",
            "id": "550e8400-e29b-41d4-a716-446655440002"
          }
        }
      }
    }
  }' \
  "https://api.riva-ash.com/api/v1/documents" | jq '.'
```

### Update Document with Relationships

```bash
#!/bin/bash
# Update document with relationships

curl -s -X PATCH \
  -H "Authorization: Bearer YOUR_ACCESS_TOKEN" \
  -H "Content-Type: application/vnd.api+json" \
  -H "Accept: application/vnd.api+json" \
  -d '{
    "data": {
      "type": "documents",
      "id": "550e8400-e29b-41d4-a716-446655440000",
      "attributes": {
        "title": "Updated Quarterly Report Q4 2023",
        "description": "Updated Q4 financial performance report"
      },
      "relationships": {
        "category": {
          "data": {
            "type": "categories",
            "id": "550e8400-e29b-41d4-a716-446655440002"
          }
        }
      }
    }
  }' \
  "https://api.riva-ash.com/api/v1/documents/550e8400-e29b-41d4-a716-446655440000" | jq '.'
```

### JavaScript/TypeScript Example

```javascript
import { JSONAPI } from '@riva-ash/json-api-client';

const client = new JSONAPI({
  baseURL: 'https://api.riva-ash.com/api/v1',
  headers: {
    'Authorization': 'Bearer YOUR_ACCESS_TOKEN',
    'Content-Type': 'application/vnd.api+json',
    'Accept': 'application/vnd.api+json'
  }
});

// Get documents with filtering and includes
async function getDocuments() {
  try {
    const response = await client.get('documents', {
      filter: {
        status: 'active',
        category: '550e8400-e29b-41d4-a716-446655440001',
        createdAt: {
          gt: '2023-01-01T00:00:00Z'
        }
      },
      sort: '-createdAt',
      page: {
        number: 1,
        size: 10
      },
      include: ['category', 'createdBy'],
      fields: {
        documents: ['title', 'status', 'createdAt', 'fileSize'],
        categories: ['name'],
        users: ['name', 'email']
      }
    });

    console.log('Documents:', response.data);
    console.log('Total:', response.meta.total);
    console.log('Included:', response.included);
  } catch (error) {
    console.error('Error:', error.errors);
  }
}

// Create document
async function createDocument() {
  try {
    const document = {
      type: 'documents',
      attributes: {
        title: 'Quarterly Report Q4 2023',
        description: 'Q4 financial performance report',
        status: 'active'
      },
      relationships: {
        category: {
          data: {
            type: 'categories',
            id: '550e8400-e29b-41d4-a716-446655440001'
          }
        },
        createdBy: {
          data: {
            type: 'users',
            id: '550e8400-e29b-41d4-a716-446655440002'
          }
        }
      }
    };

    const response = await client.post('documents', { data: document });
    console.log('Created document:', response.data);
  } catch (error) {
    console.error('Error:', error.errors);
  }
}

// Update document
async function updateDocument(documentId) {
  try {
    const document = {
      type: 'documents',
      id: documentId,
      attributes: {
        title: 'Updated Quarterly Report Q4 2023',
        description: 'Updated Q4 financial performance report'
      },
      relationships: {
        category: {
          data: {
            type: 'categories',
            id: '550e8400-e29b-41d4-a716-446655440002'
          }
        }
      }
    };

    const response = await client.patch(`documents/${documentId}`, { data: document });
    console.log('Updated document:', response.data);
  } catch (error) {
    console.error('Error:', error.errors);
  }
}

// Execute functions
getDocuments();
createDocument();
updateDocument('550e8400-e29b-41d4-a716-446655440000');
```

### Python Example

```python
import requests
import json

class JSONAPIClient:
    def __init__(self, base_url, access_token):
        self.base_url = base_url
        self.headers = {
            'Authorization': f'Bearer {access_token}',
            'Content-Type': 'application/vnd.api+json',
            'Accept': 'application/vnd.api+json'
        }
    
    def get(self, resource, params=None):
        url = f'{self.base_url}/{resource}'
        response = requests.get(url, headers=self.headers, params=params)
        response.raise_for_status()
        return response.json()
    
    def post(self, resource, data):
        url = f'{self.base_url}/{resource}'
        response = requests.post(url, headers=self.headers, json=data)
        response.raise_for_status()
        return response.json()
    
    def patch(self, resource, data):
        url = f'{self.base_url}/{resource}'
        response = requests.patch(url, headers=self.headers, json=data)
        response.raise_for_status()
        return response.json()
    
    def delete(self, resource):
        url = f'{self.base_url}/{resource}'
        response = requests.delete(url, headers=self.headers)
        response.raise_for_status()
        return response.json()

# Usage
client = JSONAPIClient(
    base_url='https://api.riva-ash.com/api/v1',
    access_token='YOUR_ACCESS_TOKEN'
)

# Get documents with filtering and includes
documents = client.get('documents', params={
    'filter[status]': 'active',
    'filter[category]': '550e8400-e29b-41d4-a716-446655440001',
    'filter[createdAt][gt]': '2023-01-01T00:00:00Z',
    'sort': '-createdAt',
    'page[number]': 1,
    'page[size]': 10,
    'include': 'category,createdBy',
    'fields[documents]': 'title,status,createdAt,fileSize',
    'fields[categories]': 'name',
    'fields[users]': 'name,email'
})

print('Documents:', json.dumps(documents, indent=2))

# Create document
document_data = {
    'data': {
        'type': 'documents',
        'attributes': {
            'title': 'Quarterly Report Q4 2023',
            'description': 'Q4 financial performance report',
            'status': 'active'
        },
        'relationships': {
            'category': {
                'data': {
                    'type': 'categories',
                    'id': '550e8400-e29b-41d4-a716-446655440001'
                }
            },
            'createdBy': {
                'data': {
                    'type': 'users',
                    'id': '550e8400-e29b-41d4-a716-446655440002'
                }
            }
        }
    }
}

created_document = client.post('documents', document_data)
print('Created document:', json.dumps(created_document, indent=2))

# Update document
update_data = {
    'data': {
        'type': 'documents',
        'id': '550e8400-e29b-41d4-a716-446655440000',
        'attributes': {
            'title': 'Updated Quarterly Report Q4 2023',
            'description': 'Updated Q4 financial performance report'
        },
        'relationships': {
            'category': {
                'data': {
                    'type': 'categories',
                    'id': '550e8400-e29b-41d4-a716-446655440002'
                }
            }
        }
    }
}

updated_document = client.patch('documents/550e8400-e29b-41d4-a716-446655440000', update_data)
print('Updated document:', json.dumps(updated_document, indent=2))
```

This JSON:API usage guide provides comprehensive instructions for working with the JSON:API implementation in Riva Ash. Follow these best practices to build efficient and maintainable client applications.