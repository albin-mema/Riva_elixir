# Deployment Guide

Prefer the compact guide: [DEPLOYMENT_GUIDE_COMPACT.md](./DEPLOYMENT_GUIDE_COMPACT.md)

This file remains as a deep‑dive reference.

## Prerequisites

### System Requirements

- **Operating System**: Ubuntu 20.04+ or CentOS 8+
- **Memory**: Minimum 4GB RAM (8GB recommended)
- **Storage**: Minimum 50GB SSD storage
- **CPU**: Minimum 2 vCPUs
- **Network**: Stable internet connection with adequate bandwidth

### Software Requirements

- **Elixir** 1.14+
- **Erlang/OTP** 25+
- **PostgreSQL** 14+
- **Node.js** 18+
- **Nginx** 1.18+
- **Certbot** (for SSL certificates)

## Environment Setup

### Production Server Configuration

```bash
# Update system packages
sudo apt update && sudo apt upgrade -y

# Install required dependencies
sudo apt install -y build-essential postgresql postgresql-contrib \
                    nginx certbot python3-certbot-nginx \
                    curl wget git

# Install Elixir (using asdf version manager)
sudo apt install -y asdf
asdf plugin-add elixir
asdf install elixir 1.14.0
asdf global elixir 1.14.0

# Install Node.js
asdf plugin-add nodejs
asdf install nodejs 18.0.0
asdf global nodejs 18.0.0
```

### PostgreSQL Setup

```bash
# Create PostgreSQL user and database
sudo -u postgres psql -c "CREATE USER riva_ash WITH PASSWORD 'secure_password';"
sudo -u postgres psql -c "CREATE DATABASE riva_ash_production OWNER riva_ash;"
sudo -u postgres psql -c "GRANT ALL PRIVILEGES ON DATABASE riva_ash_production TO riva_ash;"

# Configure PostgreSQL for performance
sudo tee -a /etc/postgresql/14/main/postgresql.conf > /dev/null <<EOF
# Performance optimizations
shared_buffers = 256MB
effective_cache_size = 1GB
maintenance_work_mem = 64MB
checkpoint_completion_target = 0.9
wal_buffers = 16MB
default_statistics_target = 100
random_page_cost = 1.1
effective_io_concurrency = 200
work_mem = 4MB
EOF

# Restart PostgreSQL
sudo systemctl restart postgresql
```

### Application Setup

```bash
# Create application user
sudo useradd -m -s /bin/bash riva_ash
sudo usermod -aG sudo riva_ash

# Switch to application user
sudo -u riva_ash bash

# Clone repository
git clone https://github.com/your-org/riva_ash.git
cd Riva_Ash

# Install dependencies
mix deps.get --only prod
cd assets && npm install --only=production && cd ..

# Build release
mix release --path /opt/riva_ash
```

## Configuration

### Environment Variables

Create `/etc/riva_ash/env` with the following content:

```bash
# Application Configuration
export MIX_ENV=prod
export SECRET_KEY_BASE=$(openssl rand -base64 64)
export DATABASE_URL=ecto://riva_ash:secure_password@localhost/riva_ash_production
export HOSTNAME=your-domain.com

# Security Configuration
export JWT_SECRET=$(openssl rand -base64 64)
export ENCRYPTION_KEY=$(openssl rand -base64 32)

# File Storage Configuration
export STORAGE_BACKEND=s3
export AWS_ACCESS_KEY_ID=your-access-key
export AWS_SECRET_ACCESS_KEY=your-secret-key
export AWS_REGION=us-east-1
export S3_BUCKET=riva-ash-documents

# Email Configuration
export SMTP_HOST=smtp.gmail.com
export SMTP_PORT=587
export SMTP_USERNAME=your-email@gmail.com
export SMTP_PASSWORD=your-app-password
export EMAIL_FROM=noreply@your-domain.com

# Monitoring Configuration
export SENTRY_DSN=your-sentry-dsn
export NEW_RELIC_LICENSE_KEY=your-new-relic-key
```

### Systemd Service

Create `/etc/systemd/system/riva_ash.service`:

```ini
[Unit]
Description=Reservo Application
After=network.target postgresql.service

[Service]
Type=simple
User=riva_ash
WorkingDirectory=/opt/riva_ash
EnvironmentFile=/etc/riva_ash/env
ExecStart=/opt/riva_ash/bin/riva_ash start
ExecStop=/opt/riva_ash/bin/riva_ash stop
Restart=always
RestartSec=5
StandardOutput=syslog
StandardError=syslog
SyslogIdentifier=riva_ash

[Install]
WantedBy=multi-user.target
```

### Nginx Configuration

Create `/etc/nginx/sites-available/riva_ash`:

```nginx
server {
    listen 80;
    server_name your-domain.com www.your-domain.com;
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    server_name your-domain.com www.your-domain.com;

    # SSL Configuration
    ssl_certificate /etc/letsencrypt/live/your-domain.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/your-domain.com/privkey.pem;
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers ECDHE-RSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384;
    ssl_prefer_server_ciphers off;
    ssl_session_cache shared:SSL:10m;
    ssl_session_timeout 10m;

    # Security Headers
    add_header X-Frame-Options DENY;
    add_header X-Content-Type-Options nosniff;
    add_header X-XSS-Protection "1; mode=block";
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;

    # Static Files
    location / {
        proxy_pass http://localhost:4000;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_cache_bypass $http_upgrade;
        proxy_read_timeout 86400s;
    }

    # Upload Limit
    client_max_body_size 100M;

    # Gzip Compression
    gzip on;
    gzip_vary on;
    gzip_min_length 1024;
    gzip_proxied any;
    gzip_comp_level 6;
    gzip_types
        text/plain
        text/css
        text/xml
        text/javascript
        application/javascript
        application/xml+rss
        application/json;
}
```

## Deployment Process

### 1. Prepare Release

```bash
# Build production release
mix release --path /opt/riva_ash

# Set proper permissions
sudo chown -R riva_ash:riva_ash /opt/riva_ash
sudo chmod -R 755 /opt/riva_ash
```

### 2. Database Migration

```bash
# Run database migrations
sudo -u riva_ash /opt/riva_ash/bin/riva_ash eval "RivaAsh.Release.migrate()"

# Verify database schema
sudo -u riva_ash /opt/riva_ash/bin/riva_ash eval "RivaAsh.Release.verify_schema()"
```

### 3. SSL Certificate Setup

```bash
# Obtain SSL certificate
sudo certbot --nginx -d your-domain.com -d www.your-domain.com

# Test auto-renewal
sudo certbot renew --dry-run
```

### 4. Start Services

```bash
# Enable and start systemd service
sudo systemctl daemon-reload
sudo systemctl enable riva_ash
sudo systemctl start riva_ash

# Enable and start nginx
sudo systemctl enable nginx
sudo systemctl start nginx

# Check service status
sudo systemctl status riva_ash
sudo systemctl status nginx
```

## Monitoring and Logging

### Application Monitoring

#### Sentry Integration

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
  }
```

#### New Relic Integration

```elixir
# config/config.exs
config :new_relic,
  license_key: System.get_env("NEW_RELIC_LICENSE_KEY"),
  app_name: "Reservo Production"
```

### Logging Configuration

```elixir
# config/config.exs
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id, :user_id, :document_id]

config :logger, backends: [:console, {Sentry.LoggerBackend, []}]
```

### System Monitoring

#### Prometheus Metrics

```elixir
# config/config.exs
config :prometheus_ex, 
  metrics: [
    RivaAshWeb.PrometheusMetrics
  ]

# lib/riva_ash_web/prometheus_metrics.ex
defmodule RivaAshWeb.PrometheusMetrics do
  use Prometheus.Metric.Counter

  @counter_name :riva_ash_requests_total
  @doc_counter_name :riva_ash_document_operations_total
  @error_counter_name :riva_ash_errors_total

  @doc """
  Initialize metrics
  """
  def setup() do
    Counter.declare(
      name: @counter_name,
      labels: [:method, :endpoint, :status],
      help: "Total number of HTTP requests"
    )

    Counter.declare(
      name: @doc_counter_name,
      labels: [:operation, :document_type],
      help: "Total number of document operations"
    )

    Counter.declare(
      name: @error_counter_name,
      labels: [:error_type, :context],
      help: "Total number of errors"
    )
  end
end
```

### Log Rotation

Create `/etc/logrotate.d/riva_ash`:

```
/opt/riva_ash/log/*.log {
    daily
    missingok
    rotate 30
    compress
    delaycompress
    notifempty
    create 644 riva_ash riva_ash
    postrotate
        systemctl reload riva_ash
    endscript
}
```

## Backup and Recovery

### Database Backup

```bash
#!/bin/bash
# /usr/local/bin/backup_riva_ash_db

BACKUP_DIR="/var/backups/riva_ash"
DATE=$(date +%Y%m%d_%H%M%S)
DB_NAME="riva_ash_production"
DB_USER="riva_ash"
DB_PASS="secure_password"

# Create backup directory
mkdir -p "$BACKUP_DIR"

# Create database backup
pg_dump -U "$DB_USER" -h localhost -F c "$DB_NAME" > "$BACKUP_DIR/db_backup_$DATE.dump"

# Compress backup
gzip "$BACKUP_DIR/db_backup_$DATE.dump"

# Keep only last 7 days of backups
find "$BACKUP_DIR" -name "db_backup_*.dump.gz" -mtime +7 -delete

# Upload to S3 (optional)
aws s3 cp "$BACKUP_DIR/db_backup_$DATE.dump.gz" "s3://riva-ash-backups/db/$DATE.dump.gz"
```

### File Storage Backup

```bash
#!/bin/bash
# /usr/local/bin/backup_riva_ash_files

BACKUP_DIR="/var/backups/riva_ash"
DATE=$(date +%Y%m%d_%H%M%S)

# Create backup directory
mkdir -p "$BACKUP_DIR/files"

# Sync S3 files to local backup
aws s3 sync "s3://riva-ash-documents" "$BACKUP_DIR/files/$DATE"

# Compress backup
tar -czf "$BACKUP_DIR/files_backup_$DATE.tar.gz" -C "$BACKUP_DIR/files" "$DATE"

# Clean up
rm -rf "$BACKUP_DIR/files/$DATE"
find "$BACKUP_DIR" -name "files_backup_*.tar.gz" -mtime +7 -delete
```

### Automated Backup Schedule

```bash
# Add to crontab
0 2 * * * /usr/local/bin/backup_riva_ash_db
30 2 * * * /usr/local/bin/backup_riva_ash_files
```

## Performance Optimization

### Database Optimization

```sql
-- Add indexes for frequently queried fields
CREATE INDEX idx_documents_user_id ON documents(user_id);
CREATE INDEX idx_documents_category_id ON documents(category_id);
CREATE INDEX idx_documents_created_at ON documents(created_at);
CREATE INDEX idx_audit_entries_entity_id ON audit_entries(entity_id);
CREATE INDEX idx_audit_entries_action ON audit_entries(action);

-- Create partial indexes for better performance
CREATE INDEX idx_documents_active ON documents(id) WHERE status = 'active';
CREATE INDEX idx_audit_recent ON audit_entries(id) 
  WHERE inserted_at > NOW() - INTERVAL '30 days';
```

### Application Performance

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Repo,
  pool_size: 20,
  pool_timeout: 30_000,
  timeout: 30_000,
  ownership_timeout: 30_000,
  log: false

# Caching configuration
config :riva_ash, RivaAsh.Cache,
  adapter: RivaAsh.Cache.Adapter.Redis,
  redis_url: System.get_env("REDIS_URL"),
  ttl: 3600

# File upload optimization
config :riva_ash, RivaAsh.Upload,
  max_file_size: 100_000_000,  # 100MB
  allowed_extensions: ~w(.pdf .doc .docx .txt .jpg .png .xlsx),
  storage_backend: System.get_env("STORAGE_BACKEND", "s3")
```

### Nginx Optimization

```nginx
# /etc/nginx/nginx.conf
worker_processes auto;
worker_rlimit_nofile 65535;

events {
    worker_connections 4096;
    use epoll;
    multi_accept on;
}

http {
    # Basic Settings
    sendfile on;
    tcp_nopush on;
    tcp_nodelay on;
    keepalive_timeout 65;
    types_hash_max_size 2048;
    server_tokens off;

    # Gzip Settings
    gzip on;
    gzip_vary on;
    gzip_min_length 1024;
    gzip_proxied any;
    gzip_comp_level 6;
    gzip_types text/plain text/css text/xml text/javascript application/javascript application/xml+rss application/json;

    # Rate Limiting
    limit_req_zone $binary_remote_addr zone=api:10m rate=10r/s;
    limit_req_zone $binary_remote_addr zone=login:10m rate=5r/m;

    # Include site configurations
    include /etc/nginx/sites-enabled/*;
}
```

## Security Hardening

### Firewall Configuration

```bash
#!/bin/bash
# /usr/local/bin/setup_firewall

# Allow SSH
ufw allow OpenSSH

# Allow HTTP and HTTPS
ufw allow 'Nginx Full'

# Allow PostgreSQL (if needed)
ufw allow 5432/tcp

# Enable firewall
ufw --force enable

# Show status
ufw status
```

### Application Security

```elixir
# config/config.exs
# Security headers
config :riva_ash, RivaAshWeb.Endpoint,
  https: [
    port: 443,
    cipher_suite: :strong,
    keyfile: "/etc/letsencrypt/live/your-domain.com/privkey.pem",
    certfile: "/etc/letsencrypt/live/your-domain.com/fullchain.pem",
    transport_options: [socket_opts: [:inet6]]
  ],
  force_ssl: [rewrite_on: [:x_forwarded_proto]],
  check_origin: false,
  server: true,
  http: [
    port: 4000,
    transport_options: [socket_opts: [:inet6]]
  ]

# Rate limiting
config :riva_ash, RivaAshWeb.Plugs.RateLimit,
  max_requests: 100,
  interval: 60

# CORS configuration
config :cors_plug, RivaAshWeb.CORSPlug,
  origin: ["https://your-domain.com"],
  max_age: 86400,
  methods: ["GET", "POST", "PUT", "DELETE", "OPTIONS"],
  headers: ["Authorization", "Content-Type", "Accept"]
```

## Maintenance Procedures

### Regular Maintenance Tasks

```bash
#!/bin/bash
# /usr/local/bin/riva_ash_maintenance

LOG_FILE="/var/log/riva_ash_maintenance.log"
DATE=$(date '+%Y-%m-%d %H:%M:%S')

echo "[$DATE] Starting maintenance tasks..." >> "$LOG_FILE"

# Clean up old audit logs
echo "[$DATE] Cleaning up old audit logs..." >> "$LOG_FILE"
psql -U riva_ash -d riva_ash_production -c "
  DELETE FROM audit_entries 
  WHERE inserted_at < NOW() - INTERVAL '1 year';
"

# Optimize database tables
echo "[$DATE] Optimizing database tables..." >> "$LOG_FILE"
psql -U riva_ash -d riva_ash_production -c "
  REINDEX DATABASE riva_ash_production;
  VACUUM ANALYZE;
"

# Clean up temporary files
echo "[$DATE] Cleaning up temporary files..." >> "$LOG_FILE"
find /tmp -name "riva_ash_*" -mtime +1 -delete

# Restart application
echo "[$DATE] Restarting application..." >> "$LOG_FILE"
systemctl restart riva_ash

echo "[$DATE] Maintenance completed successfully." >> "$LOG_FILE"
```

### Health Checks

```bash
#!/bin/bash
# /usr/local/bin/health_check

# Check if application is running
if ! systemctl is-active --quiet reservo; then
    echo "Reservo service is not running"
    exit 1
fi

# Check database connection
if ! timeout 5 bash -c "echo > /dev/tcp/localhost/5432"; then
    echo "Database connection failed"
    exit 1
fi

# Check application health
if ! curl -s -f http://localhost:4000/health > /dev/null; then
    echo "Application health check failed"
    exit 1
fi

echo "All health checks passed"
exit 0
```

## Troubleshooting

### Common Issues

#### Application Won't Start

```bash
# Check logs
journalctl -u riva_ash -f

# Check configuration
sudo -u riva_ash /opt/riva_ash/bin/riva_ash eval "RivaAsh.Release.config()"

# Validate environment variables
sudo -u riva_ash /opt/riva_ash/bin/riva_ash eval "System.get_env(\"DATABASE_URL\")"
```

#### Database Connection Issues

```bash
# Test database connection
sudo -u riva_ash psql -h localhost -U riva_ash -d riva_ash_production

# Check database logs
tail -f /var/log/postgresql/postgresql-14-main.log

# Reset database connection pool
sudo systemctl restart riva_ash
```

#### Performance Issues

```bash
# Check system resources
htop
df -h
free -h

# Check database performance
psql -U riva_ash -d riva_ash_production -c "
  SELECT query, mean_time, calls 
  FROM pg_stat_statements 
  ORDER BY mean_time DESC 
  LIMIT 10;
"

# Check application logs
tail -f /opt/riva_ash/log/elixir.log
```

## Rollback Procedures

### Application Rollback

```bash
#!/bin/bash
# /usr/local/bin/rollback_riva_ash

BACKUP_DIR="/opt/riva_ash/backups"
CURRENT_VERSION=$(cat /opt/riva_ash/VERSION)

# Create backup of current version
sudo -u riva_ash cp -r /opt/riva_ash "$BACKUP_DIR/riva_ash_$CURRENT_VERSION"

# Stop application
sudo systemctl stop riva_ash

# Restore previous version
sudo -u riva_ash cp -r "$BACKUP_DIR/riva_ash_previous" /opt/riva_ash

# Start application
sudo systemctl start riva_ash

# Verify rollback
sudo systemctl status riva_ash
```

This deployment guide provides comprehensive instructions for setting up and maintaining Riva Ash in production environments. Follow these procedures to ensure a secure, reliable, and performant deployment.