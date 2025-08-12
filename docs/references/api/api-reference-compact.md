# API Quick Reference

This quick reference provides essential API information. For comprehensive documentation with detailed examples, see [API_REFERENCE.md](./API_REFERENCE.md).

## Base URLs

- **Production**: `https://api.riva-ash.com/v1`
- **Staging**: `https://staging-api.riva-ash.com/v1`
- **Development**: `http://localhost:4000/api/v1`

## Authentication

### API Key
```bash
curl -H "Authorization: Bearer YOUR_API_KEY" \
     -H "Content-Type: application/json" \
     https://api.riva-ash.com/v1/documents
```

### OAuth 2.0
```bash
curl -H "Authorization: Bearer YOUR_ACCESS_TOKEN" \
     -H "Content-Type: application/json" \
     https://api.riva-ash.com/v1/documents
```

## Key Endpoints

### Authentication
- `POST /auth/login` - User authentication
- `POST /auth/refresh` - Token refresh
- `POST /auth/logout` - User logout

### Documents
- `GET /documents` - List documents
- `POST /documents` - Create document
- `GET /documents/{id}` - Get document
- `PATCH /documents/{id}` - Update document
- `DELETE /documents/{id}` - Delete document

### Users
- `GET /users` - List users
- `POST /users` - Create user
- `GET /users/{id}` - Get user
- `PATCH /users/{id}` - Update user

## Response Format

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

## Common Query Parameters

### Filtering
```
?filter[archived_at][is_nil]=true
?filter[category_id]=550e8400-e29b-41d4-a716-446655440001
```

### Sorting
```
?sort=-inserted_at
?sort=title
```

### Pagination
```
?page[number]=1&page[size]=20
```

## GraphQL Endpoint

- **URL**: `/graphql`
- **Query Example**: `{ users(limit: 20) { id email } }`
- **Mutation Example**: `mutation($input: CreateInput!) { create(input: $input) { id } }`

## Error Codes

- `400` - Bad Request
- `401` - Unauthorized
- `403` - Forbidden
- `404` - Not Found
- `422` - Unprocessable Entity
- `500` - Internal Server Error

## Rate Limiting

- **Default**: 100 requests per minute
- **API Keys**: 1000 requests per minute
- **Headers**: `X-RateLimit-Limit`, `X-RateLimit-Remaining`, `X-RateLimit-Reset`

## Webhooks

Available events: `document.created`, `document.updated`, `document.deleted`, `user.created`, `user.updated`

## SDKs and Libraries

- **JavaScript**: `@riva-ash/sdk`
- **Python**: `riva-ash-python`
- **Ruby**: `riva-ash-ruby`
- **Java**: `riva-ash-java`

## See Also

- [Complete API Reference](./API_REFERENCE.md) - Detailed documentation with examples
- [GraphQL API Guide](./GRAPHQL_API_GUIDE.md) - GraphQL-specific documentation
- [JSON:API Guide](./JSON_API_GUIDE.md) - JSON:API implementation guide