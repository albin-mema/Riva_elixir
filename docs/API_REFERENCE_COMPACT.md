# API Reference (Compact)

Use this as a quick map; deep details live in the full API_REFERENCE.md and specific guides (JSON_API_GUIDE.md, GRAPHQL_API_GUIDE.md, AUTHENTICATION_API.md).

## Base
- Dev: http://localhost:4000
- JSON:API: /api
- GraphQL: /graphql
- Health: /health

## Auth
- Bearer token via AshAuthentication (session/cookie for web; token for API)
- Include Authorization: Bearer <token>

## JSON:API samples
- List users: GET /api/users?filter[archived_at][is_nil]=true&sort=-inserted_at
- Create reservation: POST /api/reservations { data: { type: "reservation", attributes: { ... } } }
- Pagination: page[number]=1&page[size]=20; use sort and filter params

## GraphQL samples
- Query users:
```
query { users(limit: 20, sort: {inserted_at: DESC}) { id email inserted_at } }
```
- Mutation createReservation:
```
mutation($input: CreateReservationInput!) {
  createReservation(input: $input) { id status }
}
```

## Error shape
```json
{ "errors": [{ "code": "FORBIDDEN", "message": "...", "request_id": "..." }] }
```

## Rate limits
- Defaults via reverse proxy; keep API idempotent where possible

## Webhooks
- Optional for audit events; sign with HMAC; verify signature header

## SDKs
- Use generated clients or simple HTTP with Bearer tokens

## Notes
- Timestamps: inserted_at/updated_at (ISO8601 UTC)
- Always filter at DB level; avoid in-memory filtering
- Prefer pagination + sorting; no unbounded lists

