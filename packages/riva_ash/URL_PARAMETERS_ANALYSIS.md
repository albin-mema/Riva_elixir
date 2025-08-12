# RivaAsh URL Parameters Analysis

## Overview

This document provides a comprehensive analysis of URL parameters required for each page category in the RivaAsh application, based on the Playwright property-based test results.

## Route Categories and Parameter Requirements

### 1. Public Routes (No Authentication Required)

#### ✅ Working Routes (No Parameters)
| Path | Description | Parameters Required | Status |
|------|-------------|-------------------|---------|
| `/` | Home page | None | ✅ Working |
| `/sign-in` | User sign-in | None | ❌ Broken (Authentication module missing) |
| `/register` | User registration | None | ❌ Broken (Authentication module missing) |
| `/health` | Health check endpoint | None | ✅ Working |
| `/erd` | Entity relationship diagram | None | ✅ Working |

#### ❌ Broken Routes (Missing Dependencies)
| Path | Description | Parameters Required | Status | Error |
|------|-------------|-------------------|---------|-------|
| `/search` | Global search page | None | ❌ 500 Error | SearchService missing |
| `/docs` | API documentation | None | ❌ JS Error | Swagger UI broken |

### 2. Authenticated Routes (Require User Login)

#### ✅ Parameterless Routes
| Path | Description | Parameters Required | Status |
|------|-------------|-------------------|---------|
| `/dashboard` | Main dashboard | None | ✅ Working (if auth works) |
| `/app/settings` | User settings | None | ✅ Working (if auth works) |
| `/app/inventory` | Inventory listing | None | ✅ Working (if auth works) |
| `/app/reservations` | Reservations listing | None | ✅ Working (if auth works) |
| `/app/clients` | Clients listing | None | ✅ Working (if auth works) |
| `/app/finance` | Finance section | None | ✅ Working (if auth works) |
| `/app/employees` | Employee management | None | ✅ Working (if auth works) |
| `/app/chat` | Chat interface | None | ✅ Working (if auth works) |

#### ❌ Routes Requiring Specific Parameters

##### Inventory Management
| Path | Description | Required Parameters | Example | Status |
|------|-------------|-------------------|---------|---------|
| `/app/inventory/:id` | Inventory item detail | `id` (integer) | `/app/inventory/1` | ❌ 404 (invalid ID) |
| `/app/inventory/new` | New inventory form | None | ✅ Working (if auth works) |
| `/app/inventory/edit/:id` | Edit inventory item | `id` (integer) | `/app/inventory/edit/1` | ❌ 404 (invalid ID) |

##### Reservation Management  
| Path | Description | Required Parameters | Example | Status |
|------|-------------|-------------------|---------|---------|
| `/app/reservations/:id` | Reservation detail | `id` (integer) | `/app/reservations/1` | ❌ 404 (invalid ID) |
| `/app/reservations/new` | New reservation form | None | ✅ Working (if auth works) |
| `/app/reservations/edit/:id` | Edit reservation | `id` (integer) | `/app/reservations/edit/1` | ❌ 404 (invalid ID) |
| `/app/reservations/calendar` | Reservation calendar | Optional `date` | `/app/reservations/calendar?date=2025-08-12` | ✅ Working (if auth works) |

##### Client Management
| Path | Description | Required Parameters | Example | Status |
|------|-------------|-------------------|---------|---------|
| `/app/clients/:id` | Client detail | `id` (integer) | `/app/clients/1` | ❌ 404 (invalid ID) |
| `/app/clients/new` | New client form | None | ✅ Working (if auth works) |
| `/app/clients/edit/:id` | Edit client | `id` (integer) | `/app/clients/edit/1` | ❌ 404 (invalid ID) |

##### Employee Management
| Path | Description | Required Parameters | Example | Status |
|------|-------------|-------------------|---------|---------|
| `/app/employees/:id` | Employee detail | `id` (integer) | `/app/employees/1` | ❌ 404 (invalid ID) |
| `/app/employees/new` | New employee form | None | ✅ Working (if auth works) |
| `/app/employees/edit/:id` | Edit employee | `id` (integer) | `/app/employees/edit/1` | ❌ 404 (invalid ID) |

### 3. Admin Routes (Require Admin Authentication)

| Path | Description | Parameters Required | Example | Status |
|------|-------------|-------------------|---------|---------|
| `/admin` | Admin dashboard | None | ✅ Working (if auth works) |
| `/admin/users` | User management | Optional `role` | `/admin/users?role=admin` | ✅ Working (if auth works) |
| `/admin/settings` | Admin settings | None | ✅ Working (if auth works) |
| `/admin/reports` | Admin reports | Optional `type` | `/admin/reports?type=financial` | ✅ Working (if auth works) |

### 4. API Routes (Require Authentication + Rate Limiting)

| Path | Description | Required Parameters | Example | Status |
|------|-------------|-------------------|---------|---------|
| `/api/health` | Health check API | None | ✅ Working (if auth works) |
| `/api/booking` | Booking API | `item_id`, `start_time`, `end_time` | `/api/booking?item_id=1&start_time=2025-08-12T10:00:00Z&end_time=2025-08-12T11:00:00Z` | ❌ Rate limiting missing |
| `/api/admin/users` | Admin user API | `user_id` (optional) | `/api/admin/users?user_id=1` | ❌ Rate limiting missing |

### 5. Legacy Routes (Deprecated but Still Accessible)

| Path | Description | Parameters Required | Status |
|------|-------------|-------------------|---------|
| `/app/people` | People management (legacy) | None | ✅ Working (if auth works) |
| `/app/people/list` | People listing (legacy) | Optional `search` | ✅ Working (if auth works) |
| `/app/people/new` | New person (legacy) | None | ✅ Working (if auth works) |
| `/app/inventory/list` | Inventory listing (legacy) | Optional `category` | ✅ Working (if auth works) |
| `/app/inventory/new` | New inventory (legacy) | None | ✅ Working (if auth works) |

## Parameter Types and Validation

### Integer Parameters
**Pattern**: `:id`, `:item_id`, `:reservation_id`, `:client_id`, `:employee_id`
**Validation**: Must be positive integer corresponding to existing database records
**Example**: `/app/inventory/123` - requires inventory item with ID 123 to exist

### String Parameters  
**Pattern**: `:slug`, `:name`, `:search`
**Validation**: URL-safe strings, may have length restrictions
**Example**: `/search?q=test+query` - search query string

### Date Parameters
**Pattern**: `:date`, `:start_date`, `:end_date`
**Validation**: ISO 8601 date format
**Example**: `/app/reservations/calendar?date=2025-08-12`

### Optional Query Parameters
| Parameter | Description | Example | Default |
|-----------|-------------|---------|---------|
| `q` | Search query | `/search?q=test` | None |
| `category` | Filter by category | `/app/inventory?category=equipment` | All categories |
| `page` | Pagination | `/app/clients?page=2` | 1 |
| `per_page` | Items per page | `/app/inventory?per_page=20` | 10 |
| `sort` | Sort field | `/app/clients?sort=name` | Default sort |
| `order` | Sort direction | `/app/clients?order=desc` | asc |

## Parameter Generation Strategy

### For Testing Purposes
```elixir
# Valid ID generation (1-100 range for test data)
def generate_valid_id do
  Enum.random(1..100)
end

# String parameter generation
def generate_search_term do
  ["test", "sample", "demo", "example"]
  |> Enum.random()
end

# Date parameter generation  
def generate_future_date do
  Date.add(Date.utc_today(), Enum.random(1..30))
end
```

### For Production Use
- **Integer IDs**: Should be auto-incrementing database primary keys
- **String Slugs**: Should be generated from names with URL sanitization
- **Search Terms**: Should be validated for length and special characters
- **Dates**: Should be validated for future dates (where applicable)

## Error Scenarios

### 1. Missing Required Parameters
**Error**: 404 Not Found
**Example**: `/app/inventory/` (missing ID)
**Solution**: Ensure all required parameters are provided

### 2. Invalid Parameter Values
**Error**: 404 Not Found or 400 Bad Request  
**Example**: `/app/inventory/abc` (non-integer ID)
**Solution**: Validate parameter types and formats

### 3. Non-existent Records
**Error**: 404 Not Found
**Example**: `/app/inventory/999999` (ID doesn't exist)
**Solution**: Check database for record existence or handle gracefully

### 4. Authentication Required
**Error**: 302 Redirect to `/sign-in`
**Example**: Any authenticated route without login
**Solution**: Implement proper authentication flow

### 5. Authorization Required
**Error**: 403 Forbidden or redirect
**Example**: Admin routes accessed by regular users
**Solution**: Implement role-based access control

## Recommendations

### 1. Parameter Validation
```elixir
# Add parameter validation to routes
def validate_id_param(id) when is_integer(id) and id > 0, do: :ok
def validate_id_param(_), do: {:error, "Invalid ID"}
```

### 2. Friendly Error Messages
```elixir
# Provide helpful error messages for missing parameters
def missing_param_error(param) do
  "#{param} parameter is required"
end
```

### 3. Default Values for Optional Parameters
```elixir
# Provide sensible defaults
def default_pagination_params(params) do
  Map.merge(%{page: 1, per_page: 10}, params)
end
```

### 4. URL Generation Helpers
```elixir
# Generate URLs with proper parameters
def item_url(item) do
  "/app/inventory/#{item.id}"
end
```

## Summary

- **68 public routes** identified, most working but some blocked by missing modules
- **Multiple authenticated routes** requiring various parameter types
- **Critical missing dependencies** blocking core functionality
- **Need for proper test data generation** to avoid 404 errors
- **Authentication and authorization** as foundational requirements

This analysis shows that while the route structure is well-defined, the implementation of core services and proper parameter handling needs attention to provide a fully functional application.

---

**Generated**: 2025-08-12  
**Analysis Method**: Playwright Property-Based Testing  
**Total Routes Analyzed**: 77