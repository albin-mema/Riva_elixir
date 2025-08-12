# Deployment Guide

This document provides comprehensive instructions for deploying Riva Ash in various environments, from development to production.

## Overview

Riva Ash is a document management system built with Phoenix LiveView. This deployment guide covers:

- **Environment Preparation**: Setting up deployment environments
- **Application Deployment**: Step-by-step deployment procedures
- **Database Management**: Database setup and migrations
- **Monitoring & Maintenance**: Ongoing operations and troubleshooting
- **Security Considerations**: Production security best practices

## Deployment Environments

### Development Environment
- **Purpose**: Local development and testing
- **Technology Stack**: Local PostgreSQL, Redis, Phoenix server
- **Access**: `http://localhost:4000`

### Staging Environment
- **Purpose**: Pre-production testing and validation
- **Technology Stack**: Cloud-based infrastructure
- **Access**: `staging.riva-ash.example.com`
- **Configuration**: Mirrors production with test data

### Production Environment
- **Purpose**: Live user-facing application
- **Technology Stack**: High-availability cloud infrastructure
- **Access**: `app.riva-ash.example.com`
- **Configuration**: Optimized for performance and security

## Prerequisites

### Infrastructure Requirements

#### Minimum Production Requirements
- **CPU**: 4 vCPUs
- **Memory**: 8GB RAM
- **Storage**: 100GB SSD
- **Database**: PostgreSQL 14+ with 50GB storage
- **Redis**: Redis 6+ with 2GB memory

#### Recommended Production Requirements
- **CPU**: 8 vCPUs
- **Memory**: 16GB RAM
- **Storage**: 200GB SSD (RAID 10)
- **Database**: PostgreSQL 14+ with 100GB storage (replicated)
- **Redis**: Redis 6+ cluster with 4GB memory

### Software Requirements
- **Elixir**: v1.14 or higher
- **Erlang/OTP**: v26 or higher
- **Node.js**: v18 or higher
- **PostgreSQL**: v14 or higher
- **Redis**: v6 or higher
- **Nginx**: v1.20 or higher (reverse proxy)
- **Certbot**: SSL certificate management

## Deployment Methods

### 1. Docker Deployment (Recommended)

#### Using Docker Compose

```bash
# Clone the repository
git clone https://github.com/your-org/riva-ash.git
cd riva-ash

# Copy environment configuration
cp .env.example .env.production

# Edit production environment
nano .env.production

# Build and start services
docker-compose -f docker-compose.prod.yml up -d --build
```

#### Production Docker Compose Configuration

Create `docker-compose.prod.yml`:

```yaml
version: '3.8'

services:
  app:
    build: .
    ports:
      - "4000:4000"
    environment:
      - DATABASE_URL=ecto://user:password@postgres/riva_ash_prod
      - REDIS_URL=redis://redis:6379
      - SECRET_KEY_BASE=your-production-secret-key
      - PHX_HOST=app.riva-ash.example.com
      - PORT=4000
    depends_on:
      - postgres
      - redis
    restart: unless-stopped

  postgres:
    image: postgres:14
    environment:
      - POSTGRES_DB=riva_ash_prod
      - POSTGRES_USER=user
      - POSTGRES_PASSWORD=password
    volumes:
      - postgres_data:/var/lib/postgresql/data
      - ./backups:/backups
    restart: unless-stopped

  redis:
    image: redis:6-alpine
    volumes:
      - redis_data:/data
    restart: unless-stopped

  nginx:
    image: nginx:alpine
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf
      - ./ssl:/etc/nginx/ssl
    depends_on:
      - app
    restart: unless-stopped

volumes:
  postgres_data:
  redis_data:
```

### 2. Manual Deployment

#### Server Preparation

```bash
# Update system
sudo apt update && sudo apt upgrade -y

# Install dependencies
sudo apt install -y curl wget gnupg

# Install Elixir
wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
sudo dpkg -i erlang-solutions_2.0_all.deb
sudo apt update
sudo apt install -y elixir erlang-dev erlang-parsetools erlang-xmerl

# Install Node.js
curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -
sudo apt install -y nodejs

# Install PostgreSQL
sudo apt install -y postgresql postgresql-contrib

# Install Redis
sudo apt install -y redis-server

# Install Nginx
sudo apt install -y nginx
```

#### Application Setup

```bash
# Create application user
sudo useradd -m -s /bin/bash rivaash
sudo usermod -aG sudo rivaash

# Switch to application user
sudo su - rivaash

# Clone repository
git clone https://github.com/your-org/riva-ash.git
cd riva-ash

# Install dependencies
mix deps.get
npm install --prefix assets

# Build assets
npm run build --prefix assets
mix phx.digest

# Configure environment
cp .env.example .env.production
nano .env.production
```

#### Systemd Service Configuration

Create `/etc/systemd/system/riva-ash.service`:

```ini
[Unit]
Description=Riva Ash Application
After=network.target postgresql.service redis.service

[Service]
Type=simple
User=rivaash
Group=rivaash
WorkingDirectory=/home/rivaash/riva-ash
Environment=PATH=/home/rivaash/riva-ash/.env.production
ExecStart=/usr/local/bin/mix phx.server
Restart=always
RestartSec=10
StandardOutput=syslog
StandardError=syslog
SyslogIdentifier=riva-ash

[Install]
WantedBy=multi-user.target
```

Enable and start the service:

```bash
sudo systemctl daemon-reload
sudo systemctl enable riva-ash
sudo systemctl start riva-ash
```

## Database Setup

### Production Database Configuration

```sql
-- Create production database
CREATE DATABASE riva_ash_prod;

-- Create application user
CREATE USER rivaash_user WITH PASSWORD 'secure_password';

-- Grant permissions
GRANT ALL PRIVILEGES ON DATABASE riva_ash_prod TO rivaash_user;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO rivaash_user;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO rivaash_user;
GRANT ALL PRIVILEGES ON ALL FUNCTIONS IN SCHEMA public TO rivaash_user;

-- Configure connection limits
ALTER ROLE rivaash_user CONNECTION LIMIT 100;
```

### Database Migrations

```bash
# Run migrations
mix ecto.migrate

# Check migration status
mix ecto.migrations

# Rollback if needed (use with caution)
mix ecto.rollback
```

### Database Backup Strategy

#### Automated Backups

Create `/etc/cron.daily/riva-ash-backup`:

```bash
#!/bin/bash
DATE=$(date +%Y%m%d_%H%M%S)
BACKUP_DIR="/backups"
DB_NAME="riva_ash_prod"
DB_USER="rivaash_user"
DB_HOST="localhost"

# Create backup
pg_dump -h $DB_HOST -U $DB_USER -d $DB_NAME > $BACKUP_DIR/riva_ash_$DATE.sql

# Compress backup
gzip $BACKUP_DIR/riva_ash_$DATE.sql

# Keep only last 7 days of backups
find $BACKUP_DIR -name "riva_ash_*.sql.gz" -mtime +7 -delete

# Upload to cloud storage (optional)
# aws s3 sync $BACKUP_DIR s3://your-backup-bucket/riva-ash/
```

Make the script executable:

```bash
sudo chmod +x /etc/cron.daily/riva-ash-backup
```

## SSL/TLS Configuration

### Using Let's Encrypt

```bash
# Install Certbot
sudo apt install certbot python3-certbot-nginx

# Obtain SSL certificate
sudo certbot --nginx -d app.riva-ash.example.com

# Auto-renewal
sudo crontab -e
```

Add to crontab:

```bash
0 12 * * * /usr/bin/certbot renew --quiet
```

### Nginx Configuration

Create `/etc/nginx/sites-available/riva-ash`:

```nginx
server {
    listen 80;
    server_name app.riva-ash.example.com;
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    server_name app.riva-ash.example.com;

    # SSL configuration
    ssl_certificate /etc/letsencrypt/live/app.riva-ash.example.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/app.riva-ash.example.com/privkey.pem;
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers ECDHE-RSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384;
    ssl_prefer_server_ciphers off;
    ssl_session_cache shared:SSL:10m;
    ssl_session_timeout 10m;

    # Security headers
    add_header X-Frame-Options DENY;
    add_header X-Content-Type-Options nosniff;
    add_header X-XSS-Protection "1; mode=block";
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;

    # Application
    location / {
        proxy_pass http://localhost:4000;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }

    # Static files
    location /static/ {
        alias /path/to/your/static/files/;
        expires 1y;
        add_header Cache-Control "public, immutable";
    }

    # Health check
    location /health {
        access_log off;
        return 200 "healthy\n";
        add_header Content-Type text/plain;
    }
}
```

Enable the site:

```bash
sudo ln -s /etc/nginx/sites-available/riva-ash /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl reload nginx
```

## Monitoring and Logging

### Application Monitoring

#### Phoenix Telemetry

Configure `config/prod.exs`:

```elixir
config :riva_ash, RivaAshWeb.Telemetry,
  period_measurements: [
    {:riva_ash_web, :phx_connected_sockets, 1},
    {:riva_ash_web, :phx_active_change_events, 1}
  ],
  metrics: [
    {:riva_ash_web, :phx_connected_sockets, counter: :size},
    {:riva_ash_web, :phx_active_change_events, counter: :size}
  ]
```

#### Logging Configuration

Configure `config/prod.exs`:

```elixir
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

config :logger, :file_log,
  path: "/var/log/riva-ash/application.log",
  level: :info
```

### System Monitoring

#### Monitoring Tools

```bash
# Install monitoring tools
sudo apt install -y htop iotop nethogs

# Install Prometheus Node Exporter
wget https://github.com/prometheus/node_exporter/releases/download/v1.3.1/node_exporter-1.3.1.linux-amd64.tar.gz
tar -xzf node_exporter-1.3.1.linux-amd64.tar.gz
sudo mv node_exporter-1.3.1.linux-amd64/node_exporter /usr/local/bin/
rm -rf node_exporter-1.3.1.linux-amd64
```

Create `/etc/systemd/system/node_exporter.service`:

```ini
[Unit]
Description=Node Exporter
After=network.target

[Service]
User=prometheus
ExecStart=/usr/local/bin/node_exporter

[Install]
WantedBy=multi-user.target
```

## Performance Optimization

### Database Optimization

```sql
-- Configure PostgreSQL for production
ALTER SYSTEM SET shared_preload_libraries = 'pg_stat_statements';
ALTER SYSTEM SET max_connections = 200;
ALTER SYSTEM SET shared_buffers = '2GB';
ALTER SYSTEM SET effective_cache_size = '6GB';
ALTER SYSTEM SET maintenance_work_mem = '512MB';
ALTER SYSTEM SET checkpoint_completion_target = 0.9;
ALTER SYSTEM SET wal_buffers = '16MB';
ALTER SYSTEM SET default_statistics_target = 100;
ALTER SYSTEM SET random_page_cost = 1.1;
ALTER SYSTEM SET effective_io_concurrency = 200;
ALTER SYSTEM SET work_mem = '4MB';
ALTER SYSTEM SET min_wal_size = '1GB';
ALTER SYSTEM SET max_wal_size = '4GB';

-- Reload configuration
SELECT pg_reload_conf();
```

### Application Performance

Configure `config/prod.exs`:

```elixir
# Cache configuration
config :riva_ash, RivaAsh.Cache,
  adapter: Ecto.Adapters.Postgres,
  pool_size: 10,
  pool_timeout: 15000

# Connection pool configuration
config :riva_ash, RivaAsh.Repo,
  pool_size: 20,
  pool_timeout: 15000

# Static file serving
config :riva_ash, RivaAshWeb.Endpoint,
  http: [
    port: 4000,
    transport_options: [socket_opts: [:inet6]]
  ],
  cache_static_manifest: "priv/static/cache_manifest.json"
```

## Security Hardening

### System Security

```bash
# Configure firewall
sudo ufw allow 22/tcp
sudo ufw allow 80/tcp
sudo ufw allow 443/tcp
sudo ufw enable

# Configure fail2ban
sudo apt install -y fail2ban
sudo systemctl enable fail2ban
sudo systemctl start fail2ban
```

### Application Security

```elixir
# Configure security headers
config :riva_ash, RivaAshWeb.Endpoint,
  https: [
    port: 443,
    cipher_suite: :strong,
    keyfile: "/etc/letsencrypt/live/app.riva-ash.example.com/privkey.pem",
    certfile: "/etc/letsencrypt/live/app.riva-ash.example.com/fullchain.pem",
    transport_options: [socket_opts: [:inet6], verify: :verify_none]
  ],
  force_ssl: [rewrite_on: [:x_forwarded_proto]],
  check_origin: false,
  server: true,
  url: [host: "app.riva-ash.example.com", port: 443],
  cache_static_manifest: "priv/static/cache_manifest.json",
  secret_key_base: System.get_env("SECRET_KEY_BASE")
```

## Deployment Checklist

### Pre-Deployment

- [ ] Backup production database
- [ ] Review code changes
- [ ] Run all tests
- [ ] Check security vulnerabilities
- [ ] Update documentation
- [ ] Notify stakeholders

### Deployment Steps

- [ ] Deploy to staging environment
- [ ] Run integration tests
- [ ] Performance testing
- [ ] User acceptance testing
- [ ] Deploy to production
- [ ] Monitor application health
- [ ] Verify functionality

### Post-Deployment

- [ ] Monitor error rates
- [ ] Check performance metrics
- [ ] Verify backups
- [ ] Update monitoring dashboards
- [ ] Document deployment
- [ ] Schedule rollback plan

## Troubleshooting

### Common Issues

#### Application Won't Start

```bash
# Check service status
sudo systemctl status riva-ash

# Check logs
sudo journalctl -u riva-ash -f

# Check environment variables
sudo -u rivaash printenv
```

#### Database Connection Issues

```bash
# Check PostgreSQL status
sudo systemctl status postgresql

# Check database connections
sudo -u postgres psql -c "SELECT count(*) FROM pg_stat_activity;"

# Test database connection
mix ecto.create
```

#### High Memory Usage

```bash
# Check memory usage
free -h
htop

# Check PostgreSQL memory
sudo -u postgres psql -c "SELECT * FROM pg_stat_activity;"

# Restart services if needed
sudo systemctl restart riva-ash
```

### Emergency Procedures

#### Rollback Deployment

```bash
# Stop current deployment
sudo systemctl stop riva-ash

# Restore from backup
sudo -u postgres psql riva_ash_prod < /backups/riva_ash_rollback.sql

# Start previous version
sudo systemctl start riva-ash
```

#### Database Recovery

```bash
# Restore from backup
sudo -u postgres psql riva_ash_prod < /backups/riva_ash_latest.sql

# Run migrations
mix ecto.migrate
```

## Support

For deployment issues and questions:

- **Documentation**: [Project Documentation](./README.md)
- **Issue Tracker**: [GitHub Issues](https://github.com/your-org/riva-ash/issues)
- **Community**: [Project Community](./community.md)
- **Emergency Contact**: [Support Team](./support.md)

## Related Documentation

- [Development Environment Setup](./DEVELOPMENT_ENVIRONMENT.md)
- [Administration Guide](./guides/administration/)
- [API Reference](./api/README.md)
- [Security Guidelines](./security.md)