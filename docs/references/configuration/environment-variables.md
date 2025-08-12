# Environment Variables Reference

This document provides a comprehensive reference for all environment variables used by the Riva Ash document management system. It includes variable descriptions, default values, and configuration examples for different deployment scenarios.

## Table of Contents

1. [Overview](#overview)
2. [Application Variables](#application-variables)
3. [Database Variables](#database-variables)
4. [Authentication Variables](#authentication-variables)
5. [File Storage Variables](#file-storage-variables)
6. [Email Variables](#email-variables)
7. [Security Variables](#security-variables)
8. [Monitoring Variables](#monitoring-variables)
9. [Third-Party Service Variables](#third-party-service-variables)
10. [Development Variables](#development-variables)
11. [Production Variables](#production-variables)
12. [Environment-Specific Examples](#environment-specific-examples)
13. [Validation and Best Practices](#validation-and-best-practices)

## Overview

Environment variables provide a flexible way to configure the Riva Ash application without modifying configuration files. This approach is particularly useful for different deployment environments, containerized deployments, and managing sensitive information.

### Variable Naming Convention

- Use uppercase letters and underscores
- Prefix with `RIVA_ASH_` for application-specific variables
- Use descriptive names that clearly indicate the purpose
- Group related variables with common prefixes

### Variable Types

- **String**: Text values (most common)
- **Integer**: Numeric values
- **Boolean**: `true`/`false` values
- **List**: Comma-separated values
- **JSON**: JSON-formatted values

### Loading Priority

Environment variables are loaded in the following order:
1. Default values in configuration files
2. Values from `config/*.exs` files
3. Values from `.env` files (if present)
4. System environment variables
5. Runtime configuration from database

## Application Variables

### Core Application Settings

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_HOSTNAME` | String | `localhost` | Application hostname |
| `RIVA_ASH_PORT` | Integer | `4000` | Application port |
| `RIVA_ASH_SSL_PORT` | Integer | `443` | SSL port for HTTPS |
| `RIVA_ASH_MIX_ENV` | String | `dev` | Mix environment (dev/test/prod) |
| `RIVA_ASH_SECRET_KEY_BASE` | String | - | Secret key for session encryption |
| `RIVA_ASH_LIVE_VIEW_SALT` | String | - | Salt for LiveView signing |
| `RIVA_ASH_NODE_COOKIE` | String | - | Erlang node cookie for clustering |

### Application Behavior

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_ENABLE_METRICS` | Boolean | `true` | Enable metrics collection |
| `RIVA_ASH_ENABLE_HEALTH_CHECKS` | Boolean | `true` | Enable health checks |
| `RIVA_ASH_ENABLE_DEBUG_LOGGING` | Boolean | `false` | Enable debug logging |
| `RIVA_ASH_ENABLE_DEV_TOOLS` | Boolean | `false` | Enable development tools |
| `RIVA_ASH_ENABLE_MAINTENANCE_MODE` | Boolean | `false` | Enable maintenance mode |
| `RIVA_ASH_MAINTENANCE_MESSAGE` | String | "System under maintenance" | Maintenance mode message |

### Performance Settings

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_POOL_SIZE` | Integer | `10` | Database connection pool size |
| `RIVA_ASH_MAX_OVERFLOW` | Integer | `5` | Database connection max overflow |
| `RIVA_ASH_TIMEOUT` | Integer | `30000` | Database connection timeout (ms) |
| `RIVA_ASH_CACHE_SIZE` | Integer | `1000` | Query cache size |
| `RIVA_ASH_CACHE_TTL` | Integer | `3600` | Cache TTL (seconds) |
| `RIVA_ASH_WORKER_COUNT` | Integer | `4` | Number of background workers |

## Database Variables

### PostgreSQL Configuration

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_DB_URL` | String | - | Database connection URL |
| `RIVA_ASH_DB_HOSTNAME` | String | `localhost` | Database hostname |
| `RIVA_ASH_DB_PORT` | Integer | `5432` | Database port |
| `RIVA_ASH_DB_NAME` | String | `riva_ash` | Database name |
| `RIVA_ASH_DB_USERNAME` | String | `postgres` | Database username |
| `RIVA_ASH_DB_PASSWORD` | String | - | Database password |
| `RIVA_ASH_DB_SSL_MODE` | String | `prefer` | SSL mode (disable/allow/require/prefer/verify-ca/verify-full) |
| `RIVA_ASH_DB_SSL_CERT` | String | - | SSL certificate path |
| `RIVA_ASH_DB_SSL_KEY` | String | - | SSL private key path |
| `RIVA_ASH_DB_SSL_CA` | String | - | SSL CA certificate path |

### Database Connection Pool

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_DB_POOL_SIZE` | Integer | `10` | Connection pool size |
| `RIVA_ASH_DB_MAX_OVERFLOW` | Integer | `5` | Maximum overflow connections |
| `RIVA_ASH_DB_TIMEOUT` | Integer | `30000` | Connection timeout (ms) |
| `RIVA_ASH_DB_OWNERSHIP_TIMEOUT` | Integer | `30000` | Ownership timeout (ms) |
| `RIVA_ASH_DB_QUEUE_TARGET` | Integer | `1000` | Queue target size |
| `RIVA_ASH_DB_QUEUE_INTERVAL` | Integer | `1000` | Queue check interval (ms) |

### Database Backup

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_DB_BACKUP_ENABLED` | Boolean | `true` | Enable database backups |
| `RIVA_ASH_DB_BACKUP_INTERVAL` | String | `daily` | Backup interval (hourly/daily/weekly) |
| `RIVA_ASH_DB_BACKUP_RETENTION` | Integer | `30` | Backup retention period (days) |
| `RIVA_ASH_DB_BACKUP_PATH` | String | `/var/backups/riva_ash` | Backup storage path |
| `RIVA_ASH_DB_BACKUP_COMPRESSION` | Boolean | `true` | Enable backup compression |

## Authentication Variables

### JWT Configuration

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_JWT_SECRET` | String | - | JWT signing secret |
| `RIVA_ASH_JWT_ALGORITHM` | String | `HS256` | JWT signing algorithm |
| `RIVA_ASH_JWT_EXPIRY` | Integer | `3600` | JWT token expiry (seconds) |
| `RIVA_ASH_JWT_REFRESH_EXPIRY` | Integer | `86400` | Refresh token expiry (seconds) |
| `RIVA_ASH_JWT_MAX_REFRESH_TOKENS` | Integer | `5` | Maximum refresh tokens per user |
| `RIVA_ASH_JWT_ISSUER` | String | `riva_ash` | JWT issuer |
| `RIVA_ASH_JWT_AUDIENCE` | String | `riva_ash_users` | JWT audience |

### Password Hashing

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_PASSWORD_HASHING_ALGORITHM` | String | `argon2` | Password hashing algorithm |
| `RIVA_ASH_ARGON2_MEMORY_COST` | Integer | `32768` | Argon2 memory cost |
| `RIVA_ASH_ARGON2_PARALLELISM` | Integer | `4` | Argon2 parallelism |
| `RIVA_ASH_ARGON2_ITERATIONS` | Integer | `3` | Argon2 iterations |

### Session Management

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_SESSION_TIMEOUT` | Integer | `1800` | Session timeout (seconds) |
| `RIVA_ASH_SESSION_COOKIE_NAME` | String | `riva_ash_session` | Session cookie name |
| `RIVA_ASH_SESSION_COOKIE_SECURE` | Boolean | `true` | Secure session cookie |
| `RIVA_ASH_SESSION_COOKIE_HTTP_ONLY` | Boolean | `true` | HTTP-only session cookie |
| `RIVA_ASH_SESSION_COOKIE_SAME_SITE` | String | `Lax` | Session cookie SameSite policy |

### OAuth Configuration

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_GOOGLE_CLIENT_ID` | String | - | Google OAuth client ID |
| `RIVA_ASH_GOOGLE_CLIENT_SECRET` | String | - | Google OAuth client secret |
| `RIVA_ASH_GOOGLE_REDIRECT_URI` | String | - | Google OAuth redirect URI |
| `RIVA_ASH_GITHUB_CLIENT_ID` | String | - | GitHub OAuth client ID |
| `RIVA_ASH_GITHUB_CLIENT_SECRET` | String | - | GitHub OAuth client secret |
| `RIVA_ASH_GITHUB_REDIRECT_URI` | String | - | GitHub OAuth redirect URI |
| `RIVA_ASH_MICROSOFT_CLIENT_ID` | String | - | Microsoft OAuth client ID |
| `RIVA_ASH_MICROSOFT_CLIENT_SECRET` | String | - | Microsoft OAuth client secret |
| `RIVA_ASH_MICROSOFT_REDIRECT_URI` | String | - | Microsoft OAuth redirect URI |

## File Storage Variables

### Local Storage Configuration

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_STORAGE_BACKEND` | String | `local` | Storage backend (local/s3/azure) |
| `RIVA_ASH_STORAGE_PATH` | String | `/var/riva_ash/uploads` | Local storage path |
| `RIVA_ASH_STORAGE_URL` | String | - | Custom storage URL |
| `RIVA_ASH_STORAGE_PUBLIC_READ` | Boolean | `false` | Enable public read access |

### S3 Storage Configuration

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_S3_BUCKET` | String | - | S3 bucket name |
| `RIVA_ASH_S3_REGION` | String | `us-east-1` | AWS region |
| `RIVA_ASH_S3_ACCESS_KEY_ID` | String | - | AWS access key ID |
| `RIVA_ASH_S3_SECRET_ACCESS_KEY` | String | - | AWS secret access key |
| `RIVA_ASH_S3_ENDPOINT` | String | - | Custom S3 endpoint |
| `RIVA_ASH_S3_FORCE_PATH_STYLE` | Boolean | `false` | Force path style URLs |
| `RIVA_ASH_S3_ACL` | String | `private` | S3 ACL policy |

### Azure Storage Configuration

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_AZURE_CONTAINER` | String | - | Azure container name |
| `RIVA_ASH_AZURE_ACCOUNT_NAME` | String | - | Azure account name |
| `RIVA_ASH_AZURE_ACCOUNT_KEY` | String | - | Azure account key |
| `RIVA_ASH_AZURE_CONNECTION_STRING` | String | - | Azure connection string |
| `RIVA_ASH_AZURE_PUBLIC_ACCESS` | String | `blob` | Public access level |

### File Upload Settings

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_MAX_FILE_SIZE` | Integer | `104857600` | Maximum file size (bytes) |
| `RIVA_ASH_ALLOWED_EXTENSIONS` | String | `.pdf,.doc,.docx,.txt,.jpg,.png` | Comma-separated allowed extensions |
| `RIVA_ASH_ALLOWED_MIME_TYPES` | String | `application/pdf,application/msword,application/vnd.openxmlformats-officedocument.wordprocessingml.document,text/plain,image/jpeg,image/png` | Comma-separated allowed MIME types |
| `RIVA_ASH_ENABLE_FILE_VALIDATION` | Boolean | `true` | Enable file validation |
| `RIVA_ASH_ENABLE_VIRUS_SCANNING` | Boolean | `false` | Enable virus scanning |
| `RIVA_ASH_ENABLE_OCR` | Boolean | `false` | Enable OCR processing |

## Email Variables

### SMTP Configuration

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_SMTP_HOST` | String | `localhost` | SMTP hostname |
| `RIVA_ASH_SMTP_PORT` | Integer | `587` | SMTP port |
| `RIVA_ASH_SMTP_USERNAME` | String | - | SMTP username |
| `RIVA_ASH_SMTP_PASSWORD` | String | - | SMTP password |
| `RIVA_ASH_SMTP_TLS` | Boolean | `true` | Enable TLS |
| `RIVA_ASH_SMTP_SSL` | Boolean | `false` | Enable SSL |
| `RIVA_ASH_SMTP_AUTH` | String | `if_available` | Authentication mode |
| `RIVA_ASH_SMTP_RETRIES` | Integer | `2` | Number of retry attempts |
| `RIVA_ASH_SMTP_TIMEOUT` | Integer | `30000` | SMTP timeout (ms) |

### SendGrid Configuration

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_SENDGRID_API_KEY` | String | - | SendGrid API key |
| `RIVA_ASH_SENDGRID_FROM_EMAIL` | String | - | From email address |
| `RIVA_ASH_SENDGRID_FROM_NAME` | String | `Riva Ash` | From name |
| `RIVA_ASH_SENDGRID_REPLY_TO` | String | - | Reply-to email address |

### Email Templates

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_EMAIL_TEMPLATE_WELCOME` | String | `welcome.html` | Welcome email template |
| `RIVA_ASH_EMAIL_TEMPLATE_PASSWORD_RESET` | String | `password_reset.html` | Password reset template |
| `RIVA_ASH_EMAIL_TEMPLATE_DOCUMENT_UPLOADED` | String | `document_uploaded.html` | Document uploaded template |
| `RIVA_ASH_EMAIL_TEMPLATE_DOCUMENT_APPROVED` | String | `document_approved.html` | Document approved template |
| `RIVA_ASH_EMAIL_TEMPLATE_DOCUMENT_REJECTED` | String | `document_rejected.html` | Document rejected template |
| `RIVA_ASH_EMAIL_TEMPLATE_ARCHIVAL_NOTIFICATION` | String | `archival_notification.html` | Archival notification template |

## Security Variables

### Rate Limiting

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_RATE_LIMITING_ENABLED` | Boolean | `true` | Enable rate limiting |
| `RIVA_ASH_RATE_LIMIT_MAX_REQUESTS` | Integer | `100` | Maximum requests per time window |
| `RIVA_ASH_RATE_LIMIT_TIME_WINDOW` | Integer | `60` | Time window (seconds) |
| `RIVA_ASH_RATE_LIMIT_EXCLUDED_PATHS` | String | `/health,/metrics` | Comma-excluded paths |
| `RIVA_ASH_RATE_LIMIT_STORAGE_BACKEND` | String | `ets` | Rate limit storage backend |

### CORS Configuration

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_CORS_ENABLED` | Boolean | `true` | Enable CORS |
| `RIVA_ASH_CORS_ORIGINS` | String | `*` | Comma-separated allowed origins |
| `RIVA_ASH_CORS_METHODS` | String | `GET,POST,PUT,DELETE,OPTIONS` | Comma-separated allowed methods |
| `RIVA_ASH_CORS_HEADERS` | String | `Authorization,Content-Type,Accept,Origin` | Comma-separated allowed headers |
| `RIVA_ASH_CORS_MAX_AGE` | Integer | `86400` | CORS max age (seconds) |

### Security Headers

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_SECURITY_HEADERS_ENABLED` | Boolean | `true` | Enable security headers |
| `RIVA_ASH_SECURITY_X_FRAME_OPTIONS` | String | `DENY` | X-Frame-Options header |
| `RIVA_ASH_SECURITY_X_CONTENT_TYPE_OPTIONS` | String | `nosniff` | X-Content-Type-Options header |
| `RIVA_ASH_SECURITY_X_XSS_PROTECTION` | String | `1; mode=block` | X-XSS-Protection header |
| `RIVA_ASH_SECURITY_STRICT_TRANSPORT_SECURITY` | String | `max-age=31536000; includeSubDomains` | Strict-Transport-Security header |
| `RIVA_ASH_SECURITY_CONTENT_SECURITY_POLICY` | String | `default-src 'self'` | Content-Security-Policy header |

### Authentication Security

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_MAX_LOGIN_ATTEMPTS` | Integer | `5` | Maximum login attempts |
| `RIVA_ASH_LOGIN_LOCKOUT_DURATION` | Integer | `900` | Lockout duration (seconds) |
| `RIVA_ASH_ENABLE_CAPTCHA` | Boolean | `false` | Enable CAPTCHA |
| `RIVA_ASH_CAPTCHA_SECRET_KEY` | String | - | CAPTCHA secret key |
| `RIVA_ASH_ENABLE_TWO_FACTOR` | Boolean | `false` | Enable two-factor authentication |
| `RIVA_ASH_TWO_FACTOR_ISSUER` | String | `Riva Ash` | Two-factor issuer |

## Monitoring Variables

### Application Monitoring

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_MONITORING_ENABLED` | Boolean | `true` | Enable monitoring |
| `RIVA_ASH_MONITORING_METRICS_ENABLED` | Boolean | `true` | Enable metrics collection |
| `RIVA_ASH_MONITORING_HEALTH_CHECK_INTERVAL` | Integer | `30000` | Health check interval (ms) |
| `RIVA_ASH_MONITORING_ALERT_CPU_THRESHOLD` | Integer | `80` | CPU usage alert threshold (%) |
| `RIVA_ASH_MONITORING_ALERT_MEMORY_THRESHOLD` | Integer | `85` | Memory usage alert threshold (%) |
| `RIVA_ASH_MONITORING_ALERT_DISK_THRESHOLD` | Integer | `90` | Disk usage alert threshold (%) |
| `RIVA_ASH_MONITORING_ALERT_RESPONSE_TIME_THRESHOLD` | Integer | `5000` | Response time alert threshold (ms) |

### Logging Configuration

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_LOG_LEVEL` | String | `info` | Log level (debug/info/warn/error) |
| `RIVA_ASH_LOG_FORMAT` | String | `$time $metadata[$level] $message\n` | Log format |
| `RIVA_ASH_LOG_METADATA` | String | `request_id,user_id,document_id` | Comma-separated metadata fields |
| `RIVA_ASH_LOG_FILE` | String | - | Log file path |
| `RIVA_ASH_LOG_ROTATION_SIZE` | String | `100MB` | Log rotation size |
| `RIVA_ASH_LOG_ROTATION_COUNT` | Integer | `5` | Number of rotated log files to keep |

### Error Tracking

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_SENTRY_DSN` | String | - | Sentry DSN |
| `RIVA_ASH_SENTRY_ENVIRONMENT` | String | `development` | Sentry environment |
| `RIVA_ASH_SENTRY_RELEASE` | String | - | Sentry release version |
| `RIVA_ASH_SENTRY_TRACES_SAMPLE_RATE` | Float | `1.0` | Sentry traces sample rate |
| `RIVA_ASH_NEW_RELIC_LICENSE_KEY` | String | - | New Relic license key |
| `RIVA_ASH_NEW_RELIC_APP_NAME` | String | `Riva Ash` | New Relic application name |

## Third-Party Service Variables

### Webhook Configuration

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_WEBHOOK_ENABLED` | Boolean | `true` | Enable webhooks |
| `RIVA_ASH_WEBHOOK_TIMEOUT` | Integer | `30000` | Webhook timeout (ms) |
| `RIVA_ASH_WEBHOOK_MAX_RETRIES` | Integer | `3` | Maximum webhook retries |
| `RIVA_ASH_WEBHOOK_RETRY_DELAY` | Integer | `5000` | Webhook retry delay (ms) |
| `RIVA_ASH_WEBHOOK_SLACK_URL` | String | - | Slack webhook URL |
| `RIVA_ASH_WEBHOOK_DISCORD_URL` | String | - | Discord webhook URL |

### External API Services

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_OCR_API_KEY` | String | - | OCR service API key |
| `RIVA_ASH_OCR_API_URL` | String | - | OCR service API URL |
| `RIVA_ASH_OCR_TIMEOUT` | Integer | `60000` | OCR timeout (ms) |
| `RIVA_ASH_VIRUS_TOTAL_API_KEY` | String | - | VirusTotal API key |
| `RIVA_ASH_VIRUS_TOTAL_API_URL` | String | `https://www.virustotal.com/vtapi/v2` | VirusTotal API URL |
| `RIVA_ASH_VIRUS_TOTAL_TIMEOUT` | Integer | `120000` | VirusTotal timeout (ms) |

### Search Engine Integration

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_SEARCH_ENGINE_ENABLED` | Boolean | `false` | Enable search engine integration |
| `RIVA_ASH_SEARCH_ENGINE_TYPE` | String | `elasticsearch` | Search engine type |
| `RIVA_ASH_ELASTICSEARCH_HOSTS` | String | `localhost:9200` | Elasticsearch hosts |
| `RIVA_ASH_ELASTICSEARCH_USERNAME` | String | - | Elasticsearch username |
| `RIVA_ASH_ELASTICSEARCH_PASSWORD` | String | - | Elasticsearch password |
| `RIVA_ASH_ELASTICSEARCH_SSL` | Boolean | `false` | Enable SSL for Elasticsearch |

## Development Variables

### Development Environment

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_DEV_HOST` | String | `localhost` | Development host |
| `RIVA_ASH_DEV_PORT` | Integer | `4000` | Development port |
| `RIVA_ASH_DEV_SSL` | Boolean | `false` | Enable SSL in development |
| `RIVA_ASH_DEV_CODE_RELOADER` | Boolean | `true` | Enable code reloader |
| `RIVA_ASH_DEV_DEBUG_ERRORS` | Boolean | `true` | Enable debug error pages |
| `RIVA_ASH_DEV_WATCHERS` | String | `npm:run,build,--watch,--stdin,cd:../assets` | Development watchers |

### Development Tools

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_DEV_ENABLE_LIVE_RELOAD` | Boolean | `true` | Enable live reload |
| `RIVA_ASH_DEV_ENABLE_PHOENIX_WATCHER` | Boolean | `true` | Enable Phoenix watcher |
| `RIVA_ASH_DEV_ENABLE_NODE_WATCHER` | Boolean | `true` | Enable Node.js watcher |
| `RIVA_ASH_DEV_CONSOLIDATE_COMPILATIONS` | Boolean | `true` | Consolidate compilations |
| `RIVA_ASH_DEV_CACHE_DIGEST` | String | `full` | Cache digest mode |

### Testing Environment

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_TEST_HOST` | String | `localhost` | Test host |
| `RIVA_ASH_TEST_PORT` | Integer | `4002` | Test port |
| `RIVA_ASH_TEST_DATABASE_URL` | String | - | Test database URL |
| `RIVA_ASH_TEST_POOL_SIZE` | Integer | `10` | Test database pool size |
| `RIVA_ASH_TEST_SEED_DATA` | Boolean | `true` | Enable test seed data |
| `RIVA_ASH_TEST_MOCK_EXTERNAL_SERVICES` | Boolean | `true` | Mock external services in tests |

## Production Variables

### Production Environment

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_PROD_HOST` | String | `localhost` | Production host |
| `RIVA_ASH_PROD_PORT` | Integer | `4000` | Production port |
| `RIVA_ASH_PROD_SSL` | Boolean | `true` | Enable SSL in production |
| `RIVA_ASH_PROD_SSL_CERT_PATH` | String | - | SSL certificate path |
| `RIVA_ASH_PROD_SSL_KEY_PATH` | String | - | SSL private key path |
| `RIVA_ASH_PROD_CACHE_STATIC_MANIFEST` | String | `priv/static/cache_manifest.json` | Cache static manifest |

### Production Security

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_PROD_FORCE_SSL` | Boolean | `true` | Force SSL redirects |
| `RIVA_ASH_PROD_CHECK_ORIGIN` | Boolean | `false` | Check HTTP origin |
| `RIVA_ASH_PROD_SESSION_SECURE` | Boolean | `true` | Secure session cookies |
| `RIVA_ASH_PROD_HTTP_ONLY_COOKIES` | Boolean | `true` | HTTP-only cookies |
| `RIVA_ASH_PROD_SAME_SITE_COOKIE` | String | `Lax` | SameSite cookie policy |

### Production Performance

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `RIVA_ASH_PROD_COMPRESSION` | Boolean | `true` | Enable compression |
| `RIVA_ASH_PROD_STATIC_GZIP` | Boolean | `true` | Enable static file gzip |
| `RIVA_ASH_PROD_CACHE_CONTROL_PUBLIC` | String | `public, max-age=31536000` | Cache control for public assets |
| `RIVA_ASH_PROD_CACHE_CONTROL_PRIVATE` | String | `private, max-age=0, must-revalidate` | Cache control for private assets |
| `RIVA_ASH_PROD_ENABLE_CDN` | Boolean | `false` | Enable CDN integration |

## Environment-Specific Examples

### Development Environment

```bash
# .env.development
export RIVA_ASH_MIX_ENV="dev"
export RIVA_ASH_HOSTNAME="localhost"
export RIVA_ASH_PORT="4000"
export RIVA_ASH_DB_HOSTNAME="localhost"
export RIVA_ASH_DB_NAME="riva_ash_dev"
export RIVA_ASH_DB_USERNAME="postgres"
export RIVA_ASH_DB_PASSWORD="postgres"
export RIVA_ASH_STORAGE_BACKEND="local"
export RIVA_ASH_STORAGE_PATH="/tmp/riva_ash_uploads"
export RIVA_ASH_MAX_FILE_SIZE="52428800" # 50MB
export RIVA_ASH_ENABLE_DEBUG_LOGGING="true"
export RIVA_ASH_ENABLE_DEV_TOOLS="true"
export RIVA_ASH_LOG_LEVEL="debug"
```

### Testing Environment

```bash
# .env.test
export RIVA_ASH_MIX_ENV="test"
export RIVA_ASH_HOSTNAME="localhost"
export RIVA_ASH_PORT="4002"
export RIVA_ASH_DB_URL="ecto://postgres:postgres@localhost/riva_ash_test"
export RIVA_ASH_DB_POOL_SIZE="10"
export RIVA_ASH_STORAGE_BACKEND="memory"
export RIVA_ASH_MAX_FILE_SIZE="10485760" # 10MB
export RIVA_ASH_SEED_DATA="true"
export RIVA_ASH_MOCK_EXTERNAL_SERVICES="true"
export RIVA_ASH_LOG_LEVEL="warn"
```

### Production Environment

```bash
# .env.production
export RIVA_ASH_MIX_ENV="prod"
export RIVA_ASH_HOSTNAME="your-domain.com"
export RIVA_ASH_PORT="4000"
export RIVA_ASH_SSL_PORT="443"
export RIVA_ASH_DB_URL="ecto://riva_ash:secure_password@prod-db/riva_ash_prod"
export RIVA_ASH_DB_POOL_SIZE="20"
export RIVA_ASH_DB_SSL_MODE="require"
export RIVA_ASH_STORAGE_BACKEND="s3"
export RIVA_ASH_S3_BUCKET="riva-ash-documents"
export RIVA_ASH_S3_REGION="us-east-1"
export RIVA_ASH_S3_ACCESS_KEY_ID="your-access-key"
export RIVA_ASH_S3_SECRET_ACCESS_KEY="your-secret-key"
export RIVA_ASH_MAX_FILE_SIZE="104857600" # 100MB
export RIVA_ASH_SECRET_KEY_BASE="your-secret-key-base-here"
export RIVA_ASH_LIVE_VIEW_SALT="your-live-view-salt-here"
export RIVA_ASH_JWT_SECRET="your-jwt-secret-here"
export RIVA_ASH_ENABLE_METRICS="true"
export RIVA_ASH_ENABLE_HEALTH_CHECKS="true"
export RIVA_ASH_SSL_CERT_PATH="/etc/letsencrypt/live/your-domain.com/fullchain.pem"
export RIVA_ASH_SSL_KEY_PATH="/etc/letsencrypt/live/your-domain.com/privkey.pem"
export RIVA_ASH_SENTRY_DSN="your-sentry-dsn"
export RIVA_ASH_NEW_RELIC_LICENSE_KEY="your-new-relic-key"
```

### Docker Environment

```bash
# docker.env
export RIVA_ASH_MIX_ENV="prod"
export RIVA_ASH_HOSTNAME="riva-ash-container"
export RIVA_ASH_DB_HOSTNAME="postgres"
export RIVA_ASH_DB_NAME="riva_ash"
export RIVA_ASH_DB_USERNAME="riva_ash"
export RIVA_ASH_DB_PASSWORD="secure_password"
export RIVA_ASH_STORAGE_BACKEND="s3"
export RIVA_ASH_S3_BUCKET="riva-ash-documents"
export RIVA_ASH_S3_REGION="us-east-1"
export RIVA_ASH_SENTRY_DSN="your-sentry-dsn"
export RIVA_ASH_NEW_RELIC_LICENSE_KEY="your-new-relic-key"
```

## Validation and Best Practices

### Variable Validation

#### Required Variables

The following variables are required for proper operation:

```elixir
# lib/riva_ash/environment_validator.ex
defmodule RivaAsh.EnvironmentValidator do
  @required_vars [
    "RIVA_ASH_SECRET_KEY_BASE",
    "RIVA_ASH_DB_URL"
  ]

  def validate_required_vars do
    missing_vars = 
      @required_vars
      |> Enum.filter(fn var -> System.get_env(var) == nil end)

    case missing_vars do
      [] -> :ok
      _ -> {:error, "Missing required environment variables: #{inspect(missing_vars)}"}
    end
  end

  def validate_var_types do
    validations = [
      {"RIVA_ASH_PORT", &is_integer/1},
      {"RIVA_ASH_MAX_FILE_SIZE", &is_integer/1},
      {"RIVA_ASH_DB_POOL_SIZE", &is_integer/1},
      {"RIVA_ASH_LOG_LEVEL", &is_log_level/1},
      {"RIVA_ASH_STORAGE_BACKEND", &is_storage_backend/1}
    ]

    Enum.reduce(validations, :ok, fn {var, validator}, acc ->
      case acc do
        :ok ->
          value = System.get_env(var)
          if value && validator.(String.to_integer(value)) do
            :ok
          else
            {:error, "Invalid value for #{var}: #{value}"}
          end
        {:error, _} = error ->
          error
      end
    end)
  end

  defp is_log_level(value) do
    value in ["debug", "info", "warn", "error"]
  end

  defp is_storage_backend(value) do
    value in ["local", "s3", "azure"]
  end
end
```

### Security Best Practices

#### Sensitive Data Protection

1. **Never commit secrets to version control**
   - Use `.env` files with `.gitignore`
   - Use environment variables in production
   - Use secret management services

2. **Use secure values for secrets**
   - Generate cryptographically secure random values
   - Use appropriate length for secrets
   - Rotate secrets regularly

3. **Restrict access to configuration files**
   - Set proper file permissions
   - Use encrypted configuration storage
   - Audit access to configuration files

#### Environment Variable Security

1. **Use descriptive variable names**
   - Avoid generic names like `SECRET` or `KEY`
   - Use application-specific prefixes
   - Group related variables

2. **Document environment variables**
   - Maintain documentation for all variables
   - Include default values and descriptions
   - Document validation rules

3. **Validate environment variables**
   - Validate required variables on startup
   - Validate variable types and formats
   - Provide clear error messages

### Performance Optimization

#### Database Configuration

1. **Optimize connection pooling**
   - Set appropriate pool size based on workload
   - Configure max overflow for peak loads
   - Monitor connection usage

2. **Use SSL for production databases**
   - Enable SSL encryption
   - Configure proper SSL mode
   - Use SSL certificates

3. **Configure timeouts appropriately**
   - Set reasonable connection timeouts
   - Configure query timeouts
   - Set appropriate ownership timeouts

#### File Storage Configuration

1. **Choose appropriate storage backend**
   - Use local storage for development
   - Use cloud storage for production
   - Consider CDN for static assets

2. **Configure file size limits**
   - Set appropriate maximum file sizes
   - Configure file validation
   - Implement file cleanup procedures

3. **Optimize file serving**
   - Use proper cache headers
   - Implement compression
   - Use CDN for static files

### Monitoring and Maintenance

#### Environment Monitoring

1. **Monitor environment variables**
   - Track configuration changes
   - Monitor for missing variables
   - Alert on configuration issues

2. **Regular configuration reviews**
   - Review configuration regularly
   - Update deprecated variables
   - Remove unused variables

3. **Maintain configuration documentation**
   - Keep documentation up to date
   - Document configuration changes
   - Provide examples for common scenarios

#### Backup and Recovery

1. **Backup configuration**
   - Regular backup of configuration files
   - Backup environment variables
   - Document recovery procedures

2. **Test configuration recovery**
   - Test restoration from backups
   - Validate restored configuration
   - Document recovery procedures

3. **Implement configuration versioning**
   - Track configuration changes
   - Maintain change history
   - Implement rollback procedures

This environment variables reference provides comprehensive guidance for configuring the Riva Ash application across different environments. Follow these best practices to ensure secure, performant, and maintainable deployments.