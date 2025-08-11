# Configuration Guide

This document provides comprehensive guidance for configuring the Riva Ash document management system. It covers configuration options, environment setup, customization guidelines, and best practices for system configuration.

## Table of Contents

1. [Overview](#overview)
2. [Configuration Files](#configuration-files)
3. [Environment Configuration](#environment-configuration)
4. [Database Configuration](#database-configuration)
5. [Authentication Configuration](#authentication-configuration)
6. [File Storage Configuration](#file-storage-configuration)
7. [Email Configuration](#email-configuration)
8. [Security Configuration](#security-configuration)
9. [Performance Configuration](#performance-configuration)
10. [Monitoring Configuration](#monitoring-configuration)

## Overview

The Riva Ash system provides extensive configuration options to customize the application according to organizational requirements. Configuration can be managed through environment variables, configuration files, and the administrative interface.

### Configuration Principles

- **Environment-Specific**: Different configurations for development, staging, and production
- **Security-Sensitive**: Secure handling of sensitive configuration data
- **Version Control**: Track configuration changes in version control
- **Documentation**: Maintain clear documentation of configuration options
- **Validation**: Validate configuration changes before deployment

### Configuration Hierarchy

Configuration is loaded in the following order (later files override earlier ones):

1. Default configuration in the application
2. Environment-specific configuration files (`config/{environment}.exs`)
3. Release configuration (if applicable)
4. Runtime environment variables
5. Administrative interface overrides

## Configuration Files

### 1. Main Configuration File

**File**: `config/config.exs`

```elixir
import Config

# General Application Configuration
config :riva_ash, RivaAshWeb.Endpoint,
  url: [host: "localhost"],
  render_errors: [view: RivaAshWeb.ErrorView, accepts: ~w(html json)],
  pubsub_server: RivaAsh.PubSub,
  live_view: [signing_salt: "random_salt"]

# Database Configuration
config :riva_ash, RivaAsh.Repo,
  ssl: false,
  pool_size: 10,
  stacktrace: true,
  show_sensitive_data_on_connection_error: true

# File Upload Configuration
config :riva_ash, RivaAsh.Upload,
  max_file_size: 100_000_000, # 100MB
  allowed_extensions: ~w(.pdf .doc .docx .txt .jpg .png .xlsx .pptx),
  storage_backend: "local"

# Email Configuration
config :riva_ash, RivaAsh.Mailer,
  adapter: Swoosh.Adapters.Local

# Security Configuration
config :riva_ash, RivaAsh.Auth,
  token_expiry: 3600, # 1 hour
  refresh_token_expiry: 86400, # 24 hours
  max_login_attempts: 5,
  lockout_duration: 900 # 15 minutes

# Audit Configuration
config :riva_ash, RivaAsh.Audit,
  enabled: true,
  log_level: :info,
  retention_days: 365,
  sensitive_fields: ["password", "token", "secret"]

# Cache Configuration
config :riva_ash, RivaAsh.Cache,
  adapter: RivaAsh.Cache.Adapter.Memory,
  ttl: 3600 # 1 hour
```

### 2. Development Configuration

**File**: `config/dev.exs`

```elixir
import Config

# Development-specific settings
config :riva_ash, RivaAshWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4000],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  watchers: [
    npm: ["run", "build", "--", "--watch", "--stdin"],
    cd: Path.expand("../assets", __DIR__)
  ]

# Database configuration for development
config :riva_ash, RivaAsh.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "riva_ash_dev",
  stacktrace: true,
  show_sensitive_data_on_connection_error: true,
  pool_size: 10

# Development mailer configuration
config :riva_ash, RivaAsh.Mailer,
  adapter: Swoosh.Adapters.Local,
  # Print only the text part of emails
  format: :text

# Development authentication
config :riva_ash, RivaAsh.Auth,
  token_expiry: 86400, # 24 hours for development
  refresh_token_expiry: 604800, # 7 days for development
  bypass_mfa: true

# Development audit configuration
config :riva_ash, RivaAsh.Audit,
  enabled: true,
  log_level: :debug,
  retention_days: 30,
  sensitive_fields: ["password", "token", "secret"]

# Development cache configuration
config :riva_ash, RivaAsh.Cache,
  adapter: RivaAsh.Cache.Adapter.Memory,
  ttl: 1800 # 30 minutes for development
```

### 3. Test Configuration

**File**: `config/test.exs`

```elixir
import Config

# Test-specific settings
config :riva_ash, RivaAshWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  server: false,
  check_origin: false

# Test database configuration
config :riva_ash, RivaAsh.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "riva_ash_test#{System.get("MIX_TEST_PARTITION")}",
  pool: Ecto.Adapters.SQL.Sandbox,
  pool_size: 10

# Test mailer configuration
config :riva_ash, RivaAsh.Mailer,
  adapter: Swoosh.Adapters.Test,
  # Do not deliver emails in test
  adapter: Swoosh.Adapters.Storage

# Test authentication
config :riva_ash, RivaAsh.Auth,
  token_expiry: 300, # 5 minutes for tests
  refresh_token_expiry: 1800, # 30 minutes for tests
  bypass_mfa: true

# Test audit configuration
config :riva_ash, RivaAsh.Audit,
  enabled: true,
  log_level: :warn,
  retention_days: 7,
  sensitive_fields: ["password", "token", "secret"]

# Test cache configuration
config :riva_ash, RivaAsh.Cache,
  adapter: RivaAsh.Cache.Adapter.Memory,
  ttl: 60 # 1 minute for tests
```

### 4. Production Configuration

**File**: `config/prod.exs`

```elixir
import Config

# Production-specific settings
config :riva_ash, RivaAshWeb.Endpoint,
  url: [host: System.get_env("HOSTNAME"), port: 443],
  http: [ip: {0, 0, 0, 0}, port: 80],
  https: [
    port: 443,
    cipher_suite: :strong,
    keyfile: System.get_env("SSL_KEY_PATH"),
    certfile: System.get_env("SSL_CERT_PATH"),
    transport_options: [socket_opts: [:inet6]]
  ],
  cache_static_manifest: "priv/static/cache_manifest.json",
  server: true,
  check_origin: false,
  force_ssl: [rewrite_on: [:x_forwarded_proto]],
  cache_static_lookup: []

# Production database configuration
config :riva_ash, RivaAsh.Repo,
  ssl: true,
  url: System.get_env("DATABASE_URL"),
  pool_size: String.to_integer(System.get_env("POOL_SIZE") || "20"),
  ssl: [
    verify: :verify_none,
    versions: [:"tlsv1.2", :"tlsv1.3"],
    ciphers: :ssl.cipher_suites(:all, :"tlsv1.2"),
    server_name_indication: :disable,
    secure_renegotiate: true,
    client_renegotiation: false
  ],
  pool_timeout: 30_000,
  timeout: 30_000,
  ownership_timeout: 30_000,
  log: false

# Production mailer configuration
config :riva_ash, RivaAsh.Mailer,
  adapter: Swoosh.Adapters.SMTP,
  relay: System.get_env("SMTP_RELAY"),
  port: String.to_integer(System.get_env("SMTP_PORT") || "587"),
  username: System.get_env("SMTP_USERNAME"),
  password: System.get_env("SMTP_PASSWORD"),
  tls: :if_available,
  ssl: false,
  retries: 2,
  no_mx_lookups: false,
  auth: :if_available

# Production authentication
config :riva_ash, RivaAsh.Auth,
  token_expiry: String.to_integer(System.get_env("TOKEN_EXPIRY") || "3600"), # 1 hour
  refresh_token_expiry: String.to_integer(System.get_env("REFRESH_TOKEN_EXPIRY") || "86400"), # 24 hours
  max_login_attempts: String.to_integer(System.get_env("MAX_LOGIN_ATTEMPTS") || "5"),
  lockout_duration: String.to_integer(System.get_env("LOCKOUT_DURATION") || "900"), # 15 minutes
  mfa_required: true

# Production audit configuration
config :riva_ash, RivaAsh.Audit,
  enabled: true,
  log_level: :info,
  retention_days: String.to_integer(System.get_env("AUDIT_RETENTION_DAYS") || "365"),
  sensitive_fields: ["password", "token", "secret", "key", "credential"]

# Production cache configuration
config :riva_ash, RivaAsh.Cache,
  adapter: RivaAsh.Cache.Adapter.Redis,
  redis_url: System.get_env("REDIS_URL"),
  pool_size: String.to_integer(System.get_env("REDIS_POOL_SIZE") || "10"),
  ttl: String.to_integer(System.get_env("CACHE_TTL") || "3600") # 1 hour
```

## Environment Configuration

### 1. Environment Variables

Create a `.env` file for development:

```bash
# Application Configuration
HOSTNAME=localhost
PORT=4000
SECRET_KEY_BASE=your-secret-key-base-here

# Database Configuration
DATABASE_URL=ecto://postgres:postgres@localhost/riva_ash_dev
POOL_SIZE=10

# Authentication Configuration
JWT_SECRET=your-jwt-secret-here
ENCRYPTION_KEY=your-encryption-key-here

# File Storage Configuration
STORAGE_BACKEND=local
UPLOAD_PATH=./uploads
MAX_FILE_SIZE=104857600

# Email Configuration
SMTP_RELAY=localhost
SMTP_PORT=587
SMTP_USERNAME=your-email@example.com
SMTP_PASSWORD=your-email-password
EMAIL_FROM=noreply@your-domain.com

# Security Configuration
ENABLE_SSL=false
FORCE_SSL=false
CORS_ORIGIN=http://localhost:4000

# Monitoring Configuration
SENTRY_DSN=your-sentry-dsn-here
NEW_RELIC_LICENSE_KEY=your-new-relic-key-here
```

### 2. Production Environment Variables

Create a production environment configuration:

```bash
# Application Configuration
HOSTNAME=your-domain.com
PORT=80
SECRET_KEY_BASE=your-production-secret-key-base-here

# Database Configuration
DATABASE_URL=ecto://user:password@prod-db-host/riva_ash_prod
POOL_SIZE=20

# Authentication Configuration
JWT_SECRET=your-production-jwt-secret-here
ENCRYPTION_KEY=your-production-encryption-key-here

# File Storage Configuration
STORAGE_BACKEND=s3
AWS_ACCESS_KEY_ID=your-aws-access-key
AWS_SECRET_ACCESS_KEY=your-aws-secret-key
AWS_REGION=us-east-1
S3_BUCKET=riva-ash-documents
MAX_FILE_SIZE=104857600

# Email Configuration
SMTP_RELAY=smtp.gmail.com
SMTP_PORT=587
SMTP_USERNAME=your-email@gmail.com
SMTP_PASSWORD=your-app-password
EMAIL_FROM=noreply@your-domain.com

# Security Configuration
ENABLE_SSL=true
FORCE_SSL=true
CORS_ORIGIN=https://your-domain.com

# Monitoring Configuration
SENTRY_DSN=your-production-sentry-dsn-here
NEW_RELIC_LICENSE_KEY=your-production-new-relic-key-here
```

### 3. Docker Environment Variables

Create a `.env.docker` file for Docker deployment:

```bash
# Application Configuration
HOSTNAME=localhost
PORT=4000
SECRET_KEY_BASE=your-docker-secret-key-base-here

# Database Configuration
DATABASE_URL=ecto://postgres:postgres@db/riva_ash_dev
POOL_SIZE=10

# Authentication Configuration
JWT_SECRET=your-docker-jwt-secret-here
ENCRYPTION_KEY=your-docker-encryption-key-here

# File Storage Configuration
STORAGE_BACKEND=local
UPLOAD_PATH=/app/uploads
MAX_FILE_SIZE=104857600

# Email Configuration
SMTP_RELAY=mailhog
SMTP_PORT=1025
SMTP_USERNAME=
SMTP_PASSWORD=
EMAIL_FROM=noreply@localhost

# Security Configuration
ENABLE_SSL=false
FORCE_SSL=false
CORS_ORIGIN=http://localhost:4000

# Monitoring Configuration
SENTRY_DNS=
NEW_RELIC_LICENSE_KEY=
```

## Database Configuration

### 1. PostgreSQL Configuration

**Basic Configuration**:

```elixir
config :riva_ash, RivaAsh.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "riva_ash_dev",
  pool_size: 10,
  ssl: false
```

**Production Configuration**:

```elixir
config :riva_ash, RivaAsh.Repo,
  adapter: Ecto.Adapters.Postgres,
  url: System.get_env("DATABASE_URL"),
  pool_size: 20,
  ssl: true,
  ssl_opts: [
    verify: :verify_none,
    versions: [:"tlsv1.2", :"tlsv1.3"],
    ciphers: :ssl.cipher_suites(:all, :"tlsv1.2"),
    server_name_indication: :disable,
    secure_renegotiate: true,
    client_renegotiation: false
  ],
  pool_timeout: 30_000,
  timeout: 30_000,
  ownership_timeout: 30_000,
  log: false
```

**Connection Pool Configuration**:

```elixir
config :riva_ash, RivaAsh.Repo,
  pool_size: 20,
  max_overflow: 5,
  timeout: 30_000,
  checkout_timeout: 30_000,
  pool_timeout: 30_000,
  ownership_timeout: 30_000
```

### 2. Database Indexes

Create indexes for optimal performance:

```elixir
# In a migration file
defmodule RivaAsh.Repo.Migrations.AddIndexes do
  use Ecto.Migration

  def change do
    # Document indexes
    create index(:documents, [:user_id])
    create index(:documents, [:category_id])
    create index(:documents, [:created_at])
    create index(:documents, [:status])
    create index(:documents, [:updated_at])

    # User indexes
    create index(:users, [:email], unique: true)
    create index(:users, [:username], unique: true)
    create index(:users, [:created_at])
    create index(:users, [:last_login_at])

    # Audit trail indexes
    create index(:audit_entries, [:entity_id])
    create index(:audit_entries, [:entity_type])
    create index(:audit_entries, [:action])
    create index(:audit_entries, [:user_id])
    create index(:audit_entries, [:inserted_at])

    # Category indexes
    create index(:categories, [:parent_id])
    create index(:categories, [:created_at])
  end
end
```

### 3. Database Backup Configuration

Configure automated backups:

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Backup,
  enabled: true,
  schedule: "0 2 * * *", # Daily at 2 AM
  retention_days: 30,
  storage_backend: "s3",
  s3_bucket: "riva-ash-backups",
  s3_region: "us-east-1",
  encryption: true,
  compression: true
```

## Authentication Configuration

### 1. JWT Configuration

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Auth.Jwt,
  issuer: "riva_ash",
  audience: "riva_ash_users",
  secret_key_base: System.get_env("JWT_SECRET"),
  access_token_expiry: 3600, # 1 hour
  refresh_token_expiry: 86400, # 24 hours
  algorithm: "HS256",
  leeway: 30,
  max_refresh_attempts: 5
```

### 2. OAuth Configuration

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Auth.OAuth,
  providers: [
    %{
      name: :google,
      strategy: Ueberauth.Strategy.Google.OAuth,
      uid_field: "sub",
      client_id: System.get_env("GOOGLE_CLIENT_ID"),
      client_secret: System.get_env("GOOGLE_CLIENT_SECRET"),
      redirect_uri: System.get_env("GOOGLE_REDIRECT_URI"),
      scopes: ["email", "profile"],
      response_type: "code"
    },
    %{
      name: :github,
      strategy: Ueberauth.Strategy.GitHub.OAuth,
      uid_field: "id",
      client_id: System.get_env("GITHUB_CLIENT_ID"),
      client_secret: System.get_env("GITHUB_CLIENT_SECRET"),
      redirect_uri: System.get_env("GITHUB_REDIRECT_URI"),
      scopes: ["user:email"],
      response_type: "code"
    }
  ]
```

### 3. Multi-Factor Authentication

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Auth.MFA,
  enabled: true,
  totp_issuer: "Riva Ash",
  totp_period: 30,
  totp_window: 1,
  backup_codes_count: 10,
  backup_codes_expiry: 365, # days
  sms_provider: System.get_env("SMS_PROVIDER"),
  email_provider: System.get_env("EMAIL_PROVIDER"),
  rate_limit: %{
    login_attempts: 5,
    window: 900 # 15 minutes
  }
```

## File Storage Configuration

### 1. Local Storage

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Upload,
  storage_backend: "local",
  upload_path: System.get_env("UPLOAD_PATH") || "./uploads",
  max_file_size: 100_000_000, # 100MB
  allowed_extensions: ~w(.pdf .doc .docx .txt .jpg .png .xlsx .pptx),
  generate_thumbnails: true,
  thumbnail_sizes: [
    small: {150, 150},
    medium: {300, 300},
    large: {800, 800}
  ]
```

### 2. S3 Storage

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Upload,
  storage_backend: "s3",
  aws_access_key_id: System.get_env("AWS_ACCESS_KEY_ID"),
  aws_secret_access_key: System.get_env("AWS_SECRET_ACCESS_KEY"),
  aws_region: System.get_env("AWS_REGION") || "us-east-1",
  s3_bucket: System.get_env("S3_BUCKET"),
  max_file_size: 100_000_000, # 100MB
  allowed_extensions: ~w(.pdf .doc .docx .txt .jpg .png .xlsx .pptx),
  generate_thumbnails: true,
  thumbnail_sizes: [
    small: {150, 150},
    medium: {300, 300},
    large: {800, 800}
  ],
  s3_opts: [
    scheme: "https://",
    host: "s3.amazonaws.com",
    virtual_host: true
  ]
```

### 3. Azure Blob Storage

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Upload,
  storage_backend: "azure",
  azure_storage_account: System.get_env("AZURE_STORAGE_ACCOUNT"),
  azure_storage_access_key: System.get_env("AZURE_STORAGE_ACCESS_KEY"),
  azure_container: System.get_env("AZURE_CONTAINER") || "documents",
  max_file_size: 100_000_000, # 100MB
  allowed_extensions: ~w(.pdf .doc .docx .txt .jpg .png .xlsx .pptx),
  generate_thumbnails: true,
  thumbnail_sizes: [
    small: {150, 150},
    medium: {300, 300},
    large: {800, 800}
  ]
```

## Email Configuration

### 1. SMTP Configuration

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Mailer,
  adapter: Swoosh.Adapters.SMTP,
  relay: System.get_env("SMTP_RELAY") || "localhost",
  port: String.to_integer(System.get_env("SMTP_PORT") || "587"),
  username: System.get_env("SMTP_USERNAME"),
  password: System.get_env("SMTP_PASSWORD"),
  tls: :if_available,
  ssl: false,
  retries: 2,
  no_mx_lookups: false,
  auth: :if_available
```

### 2. SendGrid Configuration

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Mailer,
  adapter: Swoosh.Adapters.SendGrid,
  api_key: System.get_env("SENDGRID_API_KEY"),
  domain: System.get_env("SENDGRID_DOMAIN")
```

### 3. Email Templates

Configure email templates:

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Email,
  templates: %{
    welcome: "welcome.html",
    password_reset: "password_reset.html",
    document_shared: "document_shared.html",
    audit_alert: "audit_alert.html",
    security_alert: "security_alert.html"
  },
  from: System.get_env("EMAIL_FROM") || "noreply@your-domain.com",
  reply_to: System.get_env("EMAIL_REPLY_TO") || "support@your-domain.com"
```

## Security Configuration

### 1. SSL/TLS Configuration

```elixir
# config/config.exs
config :riva_ash, RivaAshWeb.Endpoint,
  https: [
    port: 443,
  cipher_suite: :strong,
  keyfile: System.get_env("SSL_KEY_PATH"),
  certfile: System.get_env("SSL_CERT_PATH"),
  transport_options: [socket_opts: [:inet6]]
],
  force_ssl: [rewrite_on: [:x_forwarded_proto]],
  check_origin: false,
  http: [
    port: 80,
    transport_options: [socket_opts: [:inet6]]
  ]
```

### 2. CORS Configuration

```elixir
# config/config.exs
config :cors_plug, RivaAshWeb.CORSPlug,
  origin: ["https://your-domain.com", "https://app.your-domain.com"],
  max_age: 86400,
  methods: ["GET", "POST", "PUT", "DELETE", "OPTIONS"],
  headers: ["Authorization", "Content-Type", "Accept", "Origin"],
  expose: ["X-Request-ID", "X-Rate-Limit"],
  credentials: true,
  max_incoming_age: 600
```

### 3. Rate Limiting

```elixir
# config/config.exs
config :riva_ash, RivaAshWeb.Plugs.RateLimit,
  max_requests: 100,
  interval: 60,
  by: :ip,
  exclude: ["/health", "/metrics"],
  storage: RivaAshWeb.Plugs.RateLimit.Storage.ETS
```

## Performance Configuration

### 1. Caching Configuration

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Cache,
  adapter: RivaAsh.Cache.Adapter.Redis,
  redis_url: System.get_env("REDIS_URL"),
  pool_size: 10,
  max_queue: 5000,
  timeout: 5000,
  reconnect_delay: 1000,
  reconnect_attempts: 3,
  backoff_factor: 2,
  ttl: 3600 # 1 hour
```

### 2. Connection Pooling

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Repo,
  pool_size: 20,
  max_overflow: 5,
  timeout: 30_000,
  checkout_timeout: 30_000,
  pool_timeout: 30_000,
  ownership_timeout: 30_000
```

### 3. Compression Configuration

```elixir
# config/config.exs
config :riva_ash, RivaAshWeb.Endpoint,
  http_compression: true,
  http_compression_min_length: 1024,
  http_compression_level: 6,
  http_compression_threshold: 1000
```

## Monitoring Configuration

### 1. Sentry Configuration

```elixir
# config/config.exs
config :sentry,
  dsn: System.get_env("SENTRY_DSN"),
  environment_name: Mix.env(),
  enable_source_code_context: true,
  root_source_code_path: File.cwd!(),
  tags: %{
    env: Mix.env(),
    version: Application.spec(:riva_ash, :vsn)
  },
  included_environments: [:prod],
  traces_sample_rate: 0.2,
  profiles_sample_rate: 1.0
```

### 2. Prometheus Configuration

```elixir
# config/config.exs
config :prometheus_ex, 
  metrics: [
    RivaAshWeb.PrometheusMetrics
  ]

config :prometheus_plugs, RivaAshWeb.PrometheusPlug,
  metrics_path: "/metrics",
  format: :text,
  registry: :default
```

### 3. New Relic Configuration

```elixir
# config/config.exs
config :new_relic,
  license_key: System.get_env("NEW_RELIC_LICENSE_KEY"),
  app_name: "Riva Ash Production",
  host: "collector.newrelic.com",
  port: 443,
  secure: true,
  proxy_host: nil,
  proxy_port: nil,
  proxy_user: nil,
  proxy_pass: nil
```

## Configuration Validation

### 1. Runtime Validation

Add configuration validation:

```elixir
# config/config.exs
import Config

# Validate required environment variables
required_env_vars = [
  "SECRET_KEY_BASE",
  "DATABASE_URL",
  "JWT_SECRET"
]

for var <- required_env_vars do
  if System.get_env(var) == nil do
    raise "Required environment variable #{var} is not set"
  end
end

# Validate configuration values
config :riva_ash, RivaAsh.Upload,
  max_file_size: System.get_env("MAX_FILE_SIZE") |> String.to_integer() |> max(1024 * 1024) # Minimum 1MB
```

### 2. Configuration Health Checks

Add health checks for configuration:

```elixir
# lib/riva_ash_web/health_check.ex
defmodule RivaAshWeb.HealthCheck do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    schedule_health_check()
    {:ok, %{}}
  end

  def handle_info(:health_check, state) do
    check_configuration()
    schedule_health_check()
    {:noreply, state}
  end

  defp schedule_health_check do
    Process.send_after(self(), :health_check, 60_000) # Check every minute
  end

  defp check_configuration do
    # Check database connection
    case RivaAsh.Repo.query("SELECT 1") do
      {:ok, _result} -> :ok
      {:error, _reason} -> log_error("Database connection failed")
    end

    # Check file storage
    case RivaAsh.Upload.health_check() do
      :ok -> :ok
      {:error, reason} -> log_error("File storage check failed: #{reason}")
    end

    # Check email configuration
    case RivaAsh.Mailer.health_check() do
      :ok -> :ok
      {:error, reason} -> log_error("Email configuration check failed: #{reason}")
    end
  end

  defp log_error(message) do
    Logger.error("Health check failed: #{message}")
  end
end
```

## Configuration Best Practices

### 1. Security Best Practices

- **Never commit secrets**: Use environment variables for sensitive data
- **Use secure storage**: Store secrets in secure vaults or secret managers
- **Regular rotation**: Rotate secrets regularly
- **Principle of least privilege**: Grant minimum required permissions
- **Audit configuration changes**: Log all configuration changes

### 2. Performance Best Practices

- **Monitor performance**: Regularly monitor configuration impact on performance
- **Optimize settings**: Adjust configuration based on usage patterns
- **Load testing**: Test configuration changes with load testing
- **Documentation**: Document performance implications of configuration changes
- **Regular review**: Regularly review and optimize configuration

### 3. Maintenance Best Practices

- **Version control**: Track configuration changes in version control
- **Documentation**: Maintain clear documentation of configuration options
- **Testing**: Test configuration changes in staging before production
- **Backup**: Maintain configuration backups
- **Rollback**: Plan for configuration rollback procedures

This configuration guide provides comprehensive guidance for configuring the Riva Ash document management system. Follow these best practices to ensure secure, performant, and maintainable configuration.