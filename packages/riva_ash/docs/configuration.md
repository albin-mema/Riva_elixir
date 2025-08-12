# Riva Ash Configuration Guide

[![Elixir Version](https://img.shields.io/badge/Elixir-1.18+-blue.svg)](https://elixir-lang.org)
[![Phoenix Framework](https://img.shields.io/badge/Phoenix-1.7+-orange.svg)](https://www.phoenixframework.org)
[![Ash Framework](https://img.shields.io/badge/Ash-3.5+-green.svg)](https://ash-hq.org)

This guide provides comprehensive documentation for configuring Riva Ash, including environment variables, configuration files, and integration requirements.

## 📋 Configuration Overview

Riva Ash uses a layered configuration system that supports different environments (development, test, production) and allows for both compile-time and runtime configuration.

### Configuration Hierarchy

1. **Default Configuration** - Hardcoded defaults in `config/config.exs`
2. **Environment Configuration** - Environment-specific settings in `config/{env}.exs`
3. **Runtime Configuration** - Dynamic configuration from environment variables in `config/runtime.exs`
4. **Application Configuration** - Override via `config/releases.exs` or environment

## 🔧 Environment Variables

### Database Configuration

| Variable | Description | Default | Required | Environment |
|----------|-------------|---------|----------|-------------|
| `DATABASE_URL` | Full database connection URL | - | Production | Runtime |
| `DB_USERNAME` | PostgreSQL username | `postgres` | - | All |
| `DB_PASSWORD` | PostgreSQL password | `postgres` | - | All |
| `DB_HOSTNAME` | PostgreSQL host | `localhost` | - | All |
| `DB_NAME` | PostgreSQL database name | `riva_ash_dev` | - | All |
| `DB_PORT` | PostgreSQL port | `5432` | - | All |
| `DB_POOL_SIZE` | Database connection pool size | `10` | - | All |
| `DB_SSL` | Enable SSL connection | `false` | - | All |
| `DB_SSL_OPTS` | Additional SSL options | - | - | All |

#### Database URL Format

```bash
# PostgreSQL format
DATABASE_URL=ecto://username:password@hostname:port/database_name

# Example
DATABASE_URL=ecto://user:password@localhost:5432/riva_ash_prod
```

### Application Configuration

| Variable | Description | Default | Required | Environment |
|----------|-------------|---------|----------|-------------|
| `SECRET_KEY_BASE` | Phoenix secret key for encryption | - | Production | Runtime |
| `AUTH_TOKEN_SECRET` | Authentication token signing secret | - | Production | Runtime |
| `PHX_HOST` | Phoenix host URL | `localhost` | - | All |
| `PHX_PORT` | Phoenix port | `4000` | - | All |
| `PHX_SERVER` | Enable Phoenix server mode | `false` | - | Runtime |
| `DEV_ROUTES` | Enable development routes | `false` | - | Development |

### Authentication Configuration

| Variable | Description | Default | Required | Environment |
|----------|-------------|---------|----------|-------------|
| `AUTH_TOKEN_LIFETIME_DAYS` | Token lifetime in days | `7` | - | All |
| `AUTH_SIGN_IN_TOKENS_ENABLED` | Enable sign-in tokens | `true` | - | All |
| `AUTH_PROVIDER` | Authentication provider | `ash` | - | All |
| `AUTH_LDAP_URL` | LDAP server URL | - | LDAP | All |
| `AUTH_LDAP_BIND_DN` | LDAP bind DN | - | LDAP | All |
| `AUTH_LDAP_BIND_PASSWORD` | LDAP bind password | - | LDAP | All |

### External Services Configuration

| Variable | Description | Default | Required | Environment |
|----------|-------------|---------|----------|-------------|
| `REDIS_URL` | Redis connection URL | - | Cache | All |
| `SMTP_HOST` | SMTP server host | - | Email | All |
| `SMTP_PORT` | SMTP server port | `587` | Email | All |
| `SMTP_USERNAME` | SMTP username | - | Email | All |
| `SMTP_PASSWORD` | SMTP password | - | Email | All |
| `SMTP_TLS` | Enable SMTP TLS | `true` | Email | All |
| `AWS_ACCESS_KEY_ID` | AWS access key | - | S3 | All |
| `AWS_SECRET_ACCESS_KEY` | AWS secret key | - | S3 | All |
| `AWS_REGION` | AWS region | `us-east-1` | S3 | All |
| `AWS_S3_BUCKET` | S3 bucket name | - | S3 | All |

### Monitoring & Logging Configuration

| Variable | Description | Default | Required | Environment |
|----------|-------------|---------|----------|-------------|
| `LOG_LEVEL` | Application log level | `info` | - | All |
| `LOG_FORMAT` | Log format | `$time $metadata[$level] $message\n` | - | All |
| `SENTRY_DSN` | Sentry DSN for error tracking | - | Production | All |
| `NEW_RELIC_LICENSE_KEY` | New Relic license key | - | Production | All |
| `PROMETHEUS_ENABLED` | Enable Prometheus metrics | `false` | - | All |
| `GRAFANA_URL` | Grafana dashboard URL | - | Monitoring | All |

### Development Configuration

| Variable | Description | Default | Required | Environment |
|----------|-------------|---------|----------|-------------|
| `DEV_PORT` | Development server port | `4000` | - | Development |
| `DEV_HOST` | Development server host | `localhost` | - | Development |
| `LIVE_DEBUGGER_PORT` | Live debugger port | `4007` | - | Development |
| `ASSET_WATCHER_ENABLED` | Enable asset watching | `true` | - | Development |
| `CODE_RELOADING_ENABLED` | Enable code reloading | `true` | - | Development |

## 📁 Configuration Files

### Base Configuration (`config/config.exs`)

```elixir
import Config

# Database Configuration
config :riva_ash, RivaAsh.Repo,
  username: System.get_env("DB_USERNAME", "postgres"),
  password: System.get_env("DB_PASSWORD", "postgres"),
  hostname: System.get_env("DB_HOSTNAME", "localhost"),
  database: System.get_env("DB_NAME", "riva_ash_dev"),
  port: String.to_integer(System.get_env("DB_PORT", "5432")),
  stacktrace: true,
  show_sensitive_data_on_connection_error: true,
  pool_size: Application.compile_env(:riva_ash, :db_pool_size, 10)

# Phoenix Endpoint Configuration
config :riva_ash, RivaAshWeb.Endpoint,
  url: [host: "localhost"],
  adapter: Bandit.PhoenixAdapter,
  render_errors: [
    formats: [json: RivaAshWeb.ErrorJSON],
    layout: false
  ],
  pubsub_server: RivaAsh.PubSub,
  live_view: [signing_salt: "riva_ash_live"]

# Ash Configuration
config :riva_ash, ash_domains: [RivaAsh.Domain, RivaAsh.Accounts]
config :ash, :use_all_identities_in_manage_relationship?, false
config :ash, :sat_solver, {SimpleSat, []}
config :ash, :policies, show_policy_breakdowns?: true

# AshAuthentication Configuration
config :ash_authentication, :token_lifetime, days: 7
config :ash_authentication, :sign_in_tokens_enabled, true
config :ash_authentication,
  :token_secret,
  System.get_env("AUTH_TOKEN_SECRET") ||
    Application.compile_env(:riva_ash, :auth_token_secret, "default_secret_change_me_in_prod")

# AshJsonApi Configuration
config :ash_json_api,
  json_library: Jason,
  remove_blank_fields: true,
  remove_blank_values: true
config :ash_json_api, :domains, [RivaAsh.Domain]

# AshAdmin Configuration
config :ash_admin,
  domains: [RivaAsh.Domain],
  show_sensitive_fields: [:change, :create],
  actor: {RivaAshWeb.AshAdminConfig, :actor, []},
  set_actor: {RivaAshWeb.AshAdminConfig, :set_actor, []}

# Tailwind CSS Configuration
config :tailwind,
  version: Application.compile_env(:riva_ash, :tailwind_version, "3.3.0"),
  default: [
    args: ~w(
      --config=tailwind.config.js
      --input=css/app.css
      --output=../priv/static/assets/app.css
    ),
    cd: Path.expand("../assets", __DIR__)
  ],
  storybook: [
    args: ~w(
      --config=tailwind.config.js
      --input=css/storybook.css
      --output=../priv/static/assets/storybook.css
    ),
    cd: Path.expand("../assets", __DIR__)
  ]

# Esbuild Configuration
config :esbuild,
  version: Application.compile_env(:riva_ash, :esbuild_version, "0.19.0"),
  default: [
    args: ~w(
      js/app.js
      js/storybook.js
      --bundle
      --target=es2017
      --outdir=../priv/static/assets
      --external:/fonts/*
      --external:/images/*
    ),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ],
  storybook: [
    args: ~w(
      js/storybook.js
      --bundle
      --target=es2017
      --outdir=../priv/static/assets
      --external:/fonts/*
      --external:/images/*
    ),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

# Import environment specific config
import_config "#{config_env()}.exs"
```

### Development Configuration (`config/dev.exs`)

```elixir
import Config

# Development-specific database settings
config :riva_ash, RivaAsh.Repo,
  log: :debug,
  show_sensitive_data_on_connection_error: true

# Development endpoint configuration
config :riva_ash, RivaAshWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: Application.compile_env(:riva_ash, :dev_port, 4000)],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  secret_key_base:
    System.get_env("SECRET_KEY_BASE") ||
      Application.compile_env(
        :riva_ash,
        :secret_key_base,
        "EuaQggrb3gfhrDAQUZqTsTMUt7zf9voCI2frB3kuyBabOCHiEue48mXJiMtL7QLj"
      ),
  watchers: [
    tailwind: {Tailwind, :install_and_run, [:default, ~w(--watch)]},
    storybook_tailwind: {Tailwind, :install_and_run, [:storybook, ~w(--watch)]},
    esbuild: {Esbuild, :install_and_run, [:default, ~w(--sourcemap=inline --watch)]}
  ],
  live_reload: [
    patterns: [
      ~r"priv/static/(?!uploads/).*(js|css|png|jpeg|jpg|gif|svg)$",
      ~r"priv/gettext/.*(po)$",
      ~r"lib/riva_ash_web/(controllers|live|components)/.*(ex|heex)$",
      ~r"storybook/.*(exs)$"
    ]
  ]

# Enable dev routes
config :riva_ash,
  dev_routes: String.downcase(System.get_env("DEV_ROUTES", "false")) in ["1", "true", "yes", "on"]

# Development logging
config :logger, :console,
  format: "[$level] $message\n",
  level: :debug

# Ash debug logging
config :ash, :log_level, :debug
config :ash_authentication, :log_level, :debug

# Log SQL queries
config :riva_ash, RivaAsh.Repo, log_level: :debug

# Stacktrace configuration
config :phoenix, :stacktrace_depth, Application.compile_env(:riva_ash, :stacktrace_depth, 20)

# Initialize plugs at runtime
config :phoenix, :plug_init_mode, :runtime

# LiveDebugger configuration
config :live_debugger,
  ip: {127, 0, 0, 1},
  port: Application.compile_env(:riva_ash, :live_debugger_port, 4007),
  secret_key_base:
    Application.compile_env(
      :riva_ash,
      :live_debugger_secret_key_base,
      "EuaQggrb3gfhrDAQUZqTsTMUt7zf9voCI2frB3kuyBabOCHiEue48mXJiMtL7QLj"
    ),
  signing_salt: Application.compile_env(:riva_ash, :live_debugger_signing_salt, "live_debugger_salt"),
  adapter: Bandit.PhoenixAdapter,
  server: true
```

### Production Configuration (`config/prod.exs`)

```elixir
import Config

# Production-specific database settings
config :riva_ash, RivaAsh.Repo,
  pool_size: String.to_integer(System.get_env("POOL_SIZE", "20")),
  ssl: System.get_env("DB_SSL", "false") == "true",
  ssl_opts: [
    verify: :verify_none,
    versions: [:"tlsv1.2", :"tlsv1.3"],
    ciphers: :ssl.cipher_suites(:all, :"tlsv1.2"),
    secure_renegotiate: true,
    server_name_indication: :disable,
    partial_chain: fn _ -> :unknown_ca end
  ]

# Production endpoint configuration
config :riva_ash, RivaAshWeb.Endpoint,
  url: [host: System.get_env("PHX_HOST", "example.com"), port: 443, scheme: "https"],
  http: [
    ip: {0, 0, 0, 0, 0, 0, 0, 0},
    port: String.to_integer(System.get_env("PORT", "4000"))
  ],
  secret_key_base: System.get_env("SECRET_KEY_BASE"),
  force_ssl: [hsts: true, ssl_redirect: true],
  cache_static_manifest: "priv/static/cache_manifest.json",
  server: true

# Production logging
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  level: :info

# Ash production logging
config :ash, :log_level, :info
config :ash_authentication, :log_level, :info

# Set the signing secret for authentication tokens
config :ash_authentication, :token_secret, System.get_env("AUTH_TOKEN_SECRET")

# Cache configuration
config :riva_ash, RivaAsh.Cache,
  adapter: :ets,
  ttl: :timer.hours(24)

# Rate limiting
config :riva_ash, RivaAsh.RateLimiter,
  max_requests: 100,
  interval: :timer.minutes(1)

# CORS configuration for production
config :cors_plug,
  origin: ["https://#{System.get_env("PHX_HOST", "example.com")}"],
  max_age: 86400,
  methods: ["GET", "POST", "PUT", "DELETE", "OPTIONS"],
  headers: ["Content-Type", "Authorization", "Accept"]
```

### Runtime Configuration (`config/runtime.exs`)

```elixir
import Config

# Using releases
if System.get_env("PHX_SERVER") do
  config :riva_ash, RivaAshWeb.Endpoint, server: true
end

if config_env() == :prod do
  # Database configuration with proper error handling
  database_url =
    System.get_env("DATABASE_URL") ||
      raise """
      environment variable DATABASE_URL is missing.
      For example: ecto://USER:PASS@HOST/DATABASE
      """

  # IPv6 configuration
  maybe_ipv6 =
    case System.get_env("ECTO_IPV6") do
      v when is_binary(v) ->
        enable_ipv6? =
          v
          |> String.downcase()
          |> (&(&1 in ["true", "1", "yes", "on"])).()

        case enable_ipv6? do
          true -> [:inet6]
          false -> []
        end

      _ecto_ipv6_empty ->
        []
    end

  # Database repository configuration
  config :riva_ash, RivaAsh.Repo,
    url: database_url,
    pool_size: String.to_integer(System.get_env("POOL_SIZE", "20")),
    socket_options: maybe_ipv6

  # The secret key base
  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
      raise """
      environment variable SECRET_KEY_BASE is missing.
      You can generate one by calling: mix phx.gen.secret
      """

  # Authentication token secret
  auth_token_secret =
    System.get_env("AUTH_TOKEN_SECRET") ||
      raise """
      environment variable AUTH_TOKEN_SECRET is missing.
      It is used to sign authentication tokens.
      """

  # Host and port configuration
  host = System.get_env("PHX_HOST") || Application.compile_env(:riva_ash, :phx_host, "example.com")
  port = String.to_integer(System.get_env("PORT", "4000"))

  # Endpoint configuration for production
  config :riva_ash, RivaAshWeb.Endpoint,
    url: [host: host, port: 443, scheme: "https"],
    http: [
      ip: {0, 0, 0, 0, 0, 0, 0, 0},
      port: port
    ],
    secret_key_base: secret_key_base,
    force_ssl: [hsts: true]

  # Set the signing secret for authentication tokens
  config :ash_authentication, :token_secret, auth_token_secret

  # SSL Configuration
  if System.get_env("SSL_CERT_PATH") && System.get_env("SSL_KEY_PATH") do
    config :riva_ash, RivaAshWeb.Endpoint,
      https: [
        port: 443,
        cipher_suite: :strong,
        keyfile: System.get_env("SSL_KEY_PATH"),
        certfile: System.get_env("SSL_CERT_PATH"),
        transport_options: [socket_opts: [:inet6]]
      ]
  end

  # Email configuration
  if System.get_env("SMTP_HOST") do
    config :riva_ash, RivaAsh.Mailer,
      adapter: Swoosh.Adapters.SMTP,
      relay: System.get_env("SMTP_HOST"),
      port: String.to_integer(System.get_env("SMTP_PORT", "587")),
      username: System.get_env("SMTP_USERNAME"),
      password: System.get_env("SMTP_PASSWORD"),
      tls: System.get_env("SMTP_TLS", "true") == "true",
      ssl: false,
      retries: 1,
      no_mx_lookup: false
  end

  # External storage configuration
  if System.get_env("AWS_ACCESS_KEY_ID") do
    config :riva_ash, RivaAsh.Storage,
      adapter: RivaAsh.Storage.S3,
      bucket: System.get_env("AWS_S3_BUCKET"),
      region: System.get_env("AWS_REGION", "us-east-1"),
      access_key_id: System.get_env("AWS_ACCESS_KEY_ID"),
      secret_access_key: System.get_env("AWS_SECRET_ACCESS_KEY")
  end

  # Monitoring configuration
  if System.get_env("SENTRY_DSN") do
    config :sentry,
      dsn: System.get_env("SENTRY_DSN"),
      environment_name: config_env(),
      enable_source_code_context: true,
      root_source_code_path: File.cwd!()
  end

  if System.get_env("NEW_RELIC_LICENSE_KEY") do
    config :new_relic,
      license_key: System.get_env("NEW_RELIC_LICENSE_KEY"),
      app_name: System.get_env("NEW_RELIC_APP_NAME", "Riva Ash")
  end

  # Prometheus configuration
  if System.get_env("PROMETHEUS_ENABLED") == "true" do
    config :prometheus_ex,
      metrics_path: "/metrics",
      metric_storage: :ets,
      metric_storage_options: [
        :public,
        named_table: :riva_ash_metrics,
        compressed: true,
        write_concurrency: true,
        read_concurrency: true
      ]
  end
end
```

## 🔧 Integration Requirements

### Database Integration

#### PostgreSQL Setup

```bash
# Install PostgreSQL
sudo apt-get install postgresql postgresql-contrib

# Create database and user
sudo -u postgres createdb riva_ash_dev
sudo -u postgres createuser riva_ash_user
sudo -u postgres psql -c "ALTER USER riva_ash_user PASSWORD 'your_password';"
sudo -u postgres psql -c "GRANT ALL PRIVILEGES ON DATABASE riva_ash_dev TO riva_ash_user;"

# Configure PostgreSQL for production
echo "host all all 0.0.0.0/0 md5" | sudo tee -a /etc/postgresql/14/main/pg_hba.conf
sudo systemctl restart postgresql
```

#### Database Migrations

```bash
# Run migrations
mix ecto.migrate

# Generate new migration
mix ecto.gen.migration migration_name

# Rollback migration
mix ecto.rollback

# Reset database
mix ecto.reset
```

### Authentication Integration

#### AshAuthentication Setup

```elixir
# Configure authentication
config :riva_ash, RivaAsh.Accounts.User,
  attributes: [:email, :hashed_password],
  code_interface: true,
  require_primary_key?: false,
  actions: [
    sign_in: [
      args: [:email, :password],
      get: [RivaAsh.Accounts.User, by: :email]
    ],
    register: [
      args: [:email, :password],
      change: fn changeset, _ ->
        changeset
        |> Ash.Changeset.require_attribute(:password)
        |> Ash.Changeset.set_attribute(:hashed_password, &Bcrypt.hash_pwd_salt(&1))
      end
    ]
  ]
```

#### LDAP Integration

```elixir
# Configure LDAP authentication
config :riva_ash, RivaAsh.Auth.LdapProvider,
  host: System.get_env("AUTH_LDAP_HOST"),
  port: String.to_integer(System.get_env("AUTH_LDAP_PORT", "389")),
  base: System.get_env("AUTH_LDAP_BASE"),
  bind_dn: System.get_env("AUTH_LDAP_BIND_DN"),
  bind_password: System.get_env("AUTH_LDAP_BIND_PASSWORD"),
  uid: "uid",
  ssl: System.get_env("AUTH_LDAP_SSL", "false") == "true",
  tls: :if_requested,
  tls_options: [verify: :verify_none]
```

### Email Integration

#### SMTP Configuration

```elixir
# Configure email
config :riva_ash, RivaAsh.Mailer,
  adapter: Swoosh.Adapters.SMTP,
  relay: System.get_env("SMTP_HOST"),
  port: String.to_integer(System.get_env("SMTP_PORT", "587")),
  username: System.get_env("SMTP_USERNAME"),
  password: System.get_env("SMTP_PASSWORD"),
  tls: System.get_env("SMTP_TLS", "true") == "true",
  ssl: false,
  retries: 1,
  no_mx_lookup: false
```

#### SendGrid Integration

```elixir
# Configure SendGrid
config :riva_ash, RivaAsh.Mailer,
  adapter: Swoosh.Adapters.SendGrid,
  api_key: System.get_env("SENDGRID_API_KEY")
```

### Storage Integration

#### S3 Storage

```elixir
# Configure S3 storage
config :riva_ash, RivaAsh.Storage,
  adapter: RivaAsh.Storage.S3,
  bucket: System.get_env("AWS_S3_BUCKET"),
  region: System.get_env("AWS_REGION", "us-east-1"),
  access_key_id: System.get_env("AWS_ACCESS_KEY_ID"),
  secret_access_key: System.get_env("AWS_SECRET_ACCESS_KEY")
```

#### Local Storage

```elixir
# Configure local storage
config :riva_ash, RivaAsh.Storage,
  adapter: RivaAsh.Storage.Local,
  storage_path: System.get_env("STORAGE_PATH", "/var/riva_ash/uploads")
```

### Monitoring Integration

#### Sentry Integration

```elixir
# Configure Sentry
config :sentry,
  dsn: System.get_env("SENTRY_DSN"),
  environment_name: config_env(),
  enable_source_code_context: true,
  root_source_code_path: File.cwd!(),
  tags: %{
    env: config_env(),
    version: Application.spec(:riva_ash, :vsn)
  }
```

#### Prometheus Integration

```elixir
# Configure Prometheus
config :prometheus_ex,
  metrics_path: "/metrics",
  metric_storage: :ets,
  metric_storage_options: [
    :public,
    named_table: :riva_ash_metrics,
    compressed: true,
    write_concurrency: true,
    read_concurrency: true
  ]
```

## 🛠️ Configuration Examples

### Development Environment

```bash
# .env.development
DB_USERNAME=postgres
DB_PASSWORD=postgres
DB_HOSTNAME=localhost
DB_NAME=riva_ash_dev
DB_PORT=5432
SECRET_KEY_BASE=your-secret-key-base-here
AUTH_TOKEN_SECRET=your-auth-token-secret-here
PHX_HOST=localhost
PHX_PORT=4000
DEV_ROUTES=true
LOG_LEVEL=debug
```

### Production Environment

```bash
# .env.production
DATABASE_URL=ecto://user:password@prod-db-host:5432/riva_ash_prod
SECRET_KEY_BASE=your-production-secret-key-base
AUTH_TOKEN_SECRET=your-production-auth-token-secret
PHX_HOST=your-domain.com
PHX_PORT=443
PHX_SERVER=true
SSL_CERT_PATH=/etc/ssl/certs/your-domain.com.crt
SSL_KEY_PATH=/etc/ssl/private/your-domain.com.key
POOL_SIZE=20
LOG_LEVEL=info
SENTRY_DSN=your-sentry-dsn
NEW_RELIC_LICENSE_KEY=your-new-relic-key
AWS_ACCESS_KEY_ID=your-aws-access-key
AWS_SECRET_ACCESS_KEY=your-aws-secret-key
AWS_S3_BUCKET=your-s3-bucket
AWS_REGION=us-east-1
SMTP_HOST=smtp.gmail.com
SMTP_PORT=587
SMTP_USERNAME=your-email@gmail.com
SMTP_PASSWORD=your-app-password
SMTP_TLS=true
```

### Docker Environment

```dockerfile
# Dockerfile
FROM elixir:1.18-alpine AS build

# Install build dependencies
RUN apk add --no-cache build-base nodejs npm postgresql-client

# Install hex and rebar
RUN mix local.hex --force && \
    mix local.rebar --force

WORKDIR /app

# Copy mix files
COPY mix.exs mix.lock ./

# Install dependencies
RUN mix deps.get --only=prod && \
    mix deps.compile

# Copy application code
COPY lib ./lib
COPY config ./config
COPY priv ./priv

# Compile application
RUN mix compile

# Build assets
COPY assets ./assets
RUN mix assets.deploy

# Release
FROM elixir:1.18-alpine AS app

# Install runtime dependencies
RUN apk add --no-cache ncurses-libs openssl postgresql-client

WORKDIR /app

# Copy compiled application
COPY --from=build /app/_build ./_build
COPY --from=build /app/deps ./deps
COPY --from=build /app/config ./config

# Copy entrypoint script
COPY entrypoint.sh .
RUN chmod +x entrypoint.sh

# Expose port
EXPOSE 4000

# Start application
CMD ["./entrypoint.sh"]
```

```bash
# docker-compose.yml
version: '3.8'

services:
  app:
    build: .
    ports:
      - "4000:4000"
    environment:
      - DATABASE_URL=ecto://postgres:postgres@db:5432/riva_ash_dev
      - SECRET_KEY_BASE=your-secret-key-base
      - AUTH_TOKEN_SECRET=your-auth-token-secret
      - PHX_HOST=localhost
      - PHX_PORT=4000
    depends_on:
      - db
      - redis
    volumes:
      - ./uploads:/app/uploads

  db:
    image: postgres:14
    environment:
      - POSTGRES_DB=riva_ash_dev
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
    volumes:
      - postgres_data:/var/lib/postgresql/data
    ports:
      - "5432:5432"

  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"

volumes:
  postgres_data:
```

## 🚨 Troubleshooting

### Common Configuration Issues

#### Database Connection Issues

**Problem**: Application fails to connect to database
```bash
# Check database connection
mix ecto.create

# Check database logs
sudo tail -f /var/log/postgresql/postgresql-14-main.log

# Test database connection
psql -h localhost -U postgres -d riva_ash_dev
```

**Solution**:
1. Verify database is running: `sudo systemctl status postgresql`
2. Check database credentials in environment variables
3. Verify database user has proper permissions
4. Check firewall settings: `sudo ufw status`

#### Authentication Issues

**Problem**: Authentication tokens are invalid
```bash
# Check token configuration
mix phx.server

# Verify token secret is set
echo $AUTH_TOKEN_SECRET

# Check token expiration
grep "token_lifetime" config/config.exs
```

**Solution**:
1. Ensure `AUTH_TOKEN_SECRET` is set in production
2. Verify token lifetime configuration
3. Check for proper token signing and verification

#### Asset Compilation Issues

**Problem**: Assets fail to compile
```bash
# Clean and rebuild assets
mix assets.clean
mix assets.setup
mix assets.build

# Check Node.js version
node --version

# Install Node.js dependencies
cd assets && npm install
```

**Solution**:
1. Ensure Node.js 18+ is installed
2. Clean asset cache and rebuild
3. Check for missing npm dependencies
4. Verify asset configuration in `config/config.exs`

#### SSL/TLS Issues

**Problem**: SSL certificate errors
```bash
# Check SSL certificate
openssl x509 -in /path/to/cert.crt -text

# Test SSL connection
openssl s_client -connect your-domain.com:443

# Check certificate expiration
openssl x509 -enddate -noout -in /path/to/cert.crt
```

**Solution**:
1. Verify SSL certificate is valid and not expired
2. Ensure private key matches certificate
3. Check certificate chain configuration
4. Verify SSL configuration in endpoint

### Performance Issues

#### Database Performance

**Problem**: Slow database queries
```bash
# Enable query logging
config :riva_ash, RivaAsh.Repo, log: :debug

# Check slow queries
SELECT query, mean_time, calls 
FROM pg_stat_statements 
ORDER BY mean_time DESC 
LIMIT 10;

# Analyze table
ANALYZE your_table;
```

**Solution**:
1. Add database indexes for frequently queried columns
2. Optimize Ash resource queries
3. Increase database pool size
4. Consider read replicas for high traffic

#### Memory Issues

**Problem**: High memory usage
```bash
# Check memory usage
free -h

# Check Elixir memory
iex> :erlang.memory()

# Check BEAM processes
iex> :erlang.processes()
```

**Solution**:
1. Monitor and optimize garbage collection
2. Increase heap size if needed
3. Consider clustering for distributed systems
4. Optimize data structures and algorithms

### Security Issues

#### Environment Variable Security

**Problem**: Sensitive data exposed in logs
```bash
# Check for sensitive data in logs
grep -i "password\|secret\|key" /var/log/riva_ash/*.log

# Audit environment variables
env | grep -i "secret\|password\|key"
```

**Solution**:
1. Use proper secret management (HashiCorp Vault, AWS Secrets Manager)
2. Avoid logging sensitive data
3. Use environment variable encryption
4. Implement proper access controls

#### CORS Configuration

**Problem**: CORS errors in development
```bash
# Check CORS configuration
grep -r "cors" config/

# Test CORS headers
curl -H "Origin: http://localhost:3000" -H "Access-Control-Request-Method: GET" -H "Access-Control-Request-Headers: Content-Type" -X OPTIONS http://localhost:4000
```

**Solution**:
1. Configure CORS properly for development
2. Use specific allowed origins in production
3. Test CORS headers with curl
4. Consider using a proxy for development

### Debugging Configuration

#### Enable Debug Logging

```elixir
# Enable debug logging in development
config :logger, :console,
  level: :debug,
  format: "$time $metadata[$level] $message\n"

# Enable Ash debug logging
config :ash, :log_level, :debug
config :ash_authentication, :log_level, :debug
```

#### Configuration Validation

```elixir
# Add configuration validation
defmodule RivaAsh.Config do
  @moduledoc """
  Configuration validation module
  """

  def validate_config do
    required_env_vars = [
      "SECRET_KEY_BASE",
      "AUTH_TOKEN_SECRET"
    ]

    missing_vars = required_env_vars
    |> Enum.filter(fn var -> System.get_env(var) == nil end)

    if length(missing_vars) > 0 do
      raise "Missing required environment variables: #{Enum.join(missing_vars, ", ")}"
    end
  end
end
```

#### Health Checks

```elixir
# Add health check endpoint
defmodule RivaAshWeb.HealthController do
  use RivaAshWeb, :controller

  def health(conn, _params) do
    json(conn, %{
      status: "healthy",
      timestamp: DateTime.utc_now(),
      version: Application.spec(:riva_ash, :vsn),
      database: check_database(),
      redis: check_redis()
    })
  end

  defp check_database do
    try do
      RivaAsh.Repo.query("SELECT 1", [])
      %{status: "healthy"}
    rescue
      _ -> %{status: "unhealthy"}
    end
  end

  defp check_redis do
    try do
      # Redis health check implementation
      %{status: "healthy"}
    rescue
      _ -> %{status: "unhealthy"}
    end
  end
end
```

## 📊 Configuration Monitoring

### Configuration Health Checks

```elixir
# Add configuration monitoring
defmodule RivaAsh.Monitoring do
  @moduledoc """
  Configuration monitoring module
  """

  def monitor_config do
    %{
      database: monitor_database(),
      authentication: monitor_authentication(),
      ssl: monitor_ssl(),
      performance: monitor_performance()
    }
  end

  defp monitor_database do
    try do
      RivaAsh.Repo.query("SELECT 1", [])
      %{status: "healthy", pool_size: get_pool_size()}
    rescue
      _ -> %{status: "unhealthy"}
    end
  end

  defp monitor_authentication do
    token_secret = System.get_env("AUTH_TOKEN_SECRET")
    %{status: if(token_secret, do: "healthy", else: "unhealthy")}
  end

  defp monitor_ssl do
    # SSL monitoring implementation
    %{status: "healthy"}
  end

  defp monitor_performance do
    # Performance monitoring implementation
    %{memory_usage: :erlang.memory(:total)}
  end

  defp get_pool_size do
    Application.get_env(:riva_ash, RivaAsh.Repo)[:pool_size] || 10
  end
end
```

### Configuration Alerts

```elixir
# Add configuration alerts
defmodule RivaAsh.Alerts do
  @moduledoc """
  Configuration alerts module
  """

  def check_alerts do
    alerts = []

    # Check for missing environment variables
    missing_vars = ["SECRET_KEY_BASE", "AUTH_TOKEN_SECRET"]
    |> Enum.filter(fn var -> System.get_env(var) == nil end)

    if length(missing_vars) > 0 do
      alerts = [%{
        type: :critical,
        message: "Missing required environment variables: #{Enum.join(missing_vars, ", ")}",
        timestamp: DateTime.utc_now()
      } | alerts]
    end

    # Check for weak secrets
    if String.length(System.get_env("SECRET_KEY_BASE", "")) < 32 do
      alerts = [%{
        type: :warning,
        message: "SECRET_KEY_BASE is too short (minimum 32 characters)",
        timestamp: DateTime.utc_now()
      } | alerts]
    end

    alerts
  end
end
```

---

*This configuration guide should be kept up to date with the project's configuration requirements. For the most current information, please visit the [GitHub repository](https://github.com/your-org/riva-ash).*