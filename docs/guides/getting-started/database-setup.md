# Database Setup Guide

This document provides comprehensive instructions for setting up and configuring the database for the Riva Ash document management system. It covers PostgreSQL installation, configuration, migration, backup, and maintenance procedures.

## Table of Contents

1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [PostgreSQL Installation](#postgresql-installation)
4. [Database Creation](#database-creation)
5. [Database Configuration](#database-configuration)
6. [Database Migration](#database-migration)
7. [Database Backup](#database-backup)
8. [Database Maintenance](#database-maintenance)
9. [Performance Optimization](#performance-optimization)
10. [Security Configuration](#security-configuration)
11. [Monitoring and Health Checks](#monitoring-and-health-checks)
12. [Troubleshooting](#troubleshooting)
13. [Best Practices](#best-practices)

## Overview

The Riva Ash application uses PostgreSQL as its primary database for storing application data, including user information, document metadata, audit trails, and system configuration. Proper database setup is crucial for the application's performance, reliability, and security.

### Database Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Application Layer                        │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │   Documents │  │   Users     │  │   Audit     │         │
│  │   Metadata  │  │   Accounts  │  │   Trail     │         │
│  └─────────────┘  └─────────────┘  └─────────────┘         │
└─────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────┐
│                   Database Layer                           │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │   documents │  │   users     │  │   audit     │         │
│  │   table     │  │   table     │  │   table     │         │
│  └─────────────┘  └─────────────┘  └─────────────┘         │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │   versions  │  │   roles     │  │   archives  │         │
│  │   table     │  │   table     │  │   table     │         │
│  └─────────────┘  └─────────────┘  └─────────────┘         │
└─────────────────────────────────────────────────────────────┘
```

### Key Database Components

- **Users Table**: Stores user accounts and authentication data
- **Documents Table**: Stores document metadata and file information
- **Audit Table**: Stores comprehensive audit trail data
- **Categories Table**: Stores document classification categories
- **Versions Table**: Stores document version history
- **Archives Table**: Stores archived document information

## Prerequisites

### System Requirements

- **Operating System**: Ubuntu 20.04+, CentOS 8+, or macOS 10.15+
- **Memory**: Minimum 4GB RAM (8GB recommended for production)
- **Storage**: Minimum 50GB free space (100GB+ recommended for production)
- **CPU**: Minimum 2 vCPUs
- **Network**: Stable network connection

### Software Requirements

- **PostgreSQL**: Version 14 or later
- **Elixir**: Version 1.18 or later
- **Phoenix**: Version 1.7 or later
- **Ecto**: Version 3.10 or later

### Required Ports

- **PostgreSQL**: Default port 5432
- **Application**: Default port 4000

## PostgreSQL Installation

### Ubuntu/Debian Installation

```bash
# Update package lists
sudo apt update

# Install PostgreSQL and related packages
sudo apt install -y postgresql postgresql-contrib

# Start PostgreSQL service
sudo systemctl start postgresql

# Enable PostgreSQL to start on boot
sudo systemctl enable postgresql

# Check PostgreSQL status
sudo systemctl status postgresql
```

### CentOS/RHEL Installation

```bash
# Install PostgreSQL repository
sudo yum install -y https://download.postgresql.org/pub/repos/yum/reporpms/EL-7-x86_64/pgdg-redhat-repo-latest.noarch.rpm

# Install PostgreSQL and related packages
sudo yum install -y postgresql14-server postgresql14-contrib

# Initialize PostgreSQL database
sudo /usr/pgsql-14/bin/postgresql-14-setup initdb

# Start PostgreSQL service
sudo systemctl start postgresql

# Enable PostgreSQL to start on boot
sudo systemctl enable postgresql

# Check PostgreSQL status
sudo systemctl status postgresql
```

### macOS Installation

```bash
# Install PostgreSQL using Homebrew
brew install postgresql

# Initialize PostgreSQL database
initdb /usr/local/var/postgres

# Start PostgreSQL service
brew services start postgresql

# Check PostgreSQL status
brew services list
```

### Docker Installation

```bash
# Pull PostgreSQL image
docker pull postgres:14

# Run PostgreSQL container
docker run -d \
  --name riva-ash-db \
  -e POSTGRES_DB=riva_ash \
  -e POSTGRES_USER=riva_ash \
  -e POSTGRES_PASSWORD=secure_password \
  -p 5432:5432 \
  -v postgres_data:/var/lib/postgresql/data \
  postgres:14

# Check container status
docker ps
```

## Database Creation

### Method 1: Using PostgreSQL CLI

```bash
# Switch to postgres user
sudo -u postgres psql

# Create database
CREATE DATABASE riva_ash;

# Create user
CREATE USER riva_ash WITH PASSWORD 'secure_password';

# Grant privileges
GRANT ALL PRIVILEGES ON DATABASE riva_ash TO riva_ash;

# Grant all privileges on schema public
GRANT ALL ON SCHEMA public TO riva_ash;

# Grant usage on schema
GRANT USAGE ON SCHEMA public TO riva_ash;

# Grant all privileges on all tables in the future
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON TABLES TO riva_ash;

# Grant all privileges on all sequences in the future
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON SEQUENCES TO riva_ash;

# Grant all privileges on all types in the future
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON TYPES TO riva_ash;

# Exit psql
\q
```

### Method 2: Using createdb

```bash
# Create database
createdb riva_ash

# Create user and set password
psql -c "CREATE USER riva_ash WITH PASSWORD 'secure_password';"

# Grant privileges
psql -c "GRANT ALL PRIVILEGES ON DATABASE riva_ash TO riva_ash;"
psql -c "GRANT ALL ON SCHEMA public TO riva_ash;"
psql -c "ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON TABLES TO riva_ash;"
psql -c "ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON SEQUENCES TO riva_ash;"
psql -c "ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON TYPES TO riva_ash;"
```

### Method 3: Using Docker

```bash
# Create database and user in running container
docker exec -it riva-ash-db psql -U postgres -c "
CREATE DATABASE riva_ash;
CREATE USER riva_ash WITH PASSWORD 'secure_password';
GRANT ALL PRIVILEGES ON DATABASE riva_ash TO riva_ash;
GRANT ALL ON SCHEMA public TO riva_ash;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON TABLES TO riva_ash;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON SEQUENCES TO riva_ash;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON TYPES TO riva_ash;
"
```

## Database Configuration

### PostgreSQL Configuration File

Edit the PostgreSQL configuration file:

```bash
# Ubuntu/Debian
sudo nano /etc/postgresql/14/main/postgresql.conf

# CentOS/RHEL
sudo nano /var/lib/pgsql/14/data/postgresql.conf

# macOS
nano /usr/local/var/postgres/postgresql.conf
```

### Configuration Settings

```ini
# Connection Settings
listen_addresses = 'localhost'          # Listen on all interfaces
port = 5432                            # Port to listen on
max_connections = 200                  # Maximum connections
superuser_reserved_connections = 3     # Connections reserved for superusers

# Memory Settings
shared_buffers = 256MB                 # Shared memory for caching
effective_cache_size = 1GB             # Effective cache size
maintenance_work_mem = 64MB            # Memory for maintenance operations
checkpoint_completion_target = 0.9     # Checkpoint completion target
wal_buffers = 16MB                     # Write-ahead log buffers
default_statistics_target = 100        # Default statistics target

# Write-Ahead Log (WAL) Settings
wal_level = replica                    # Write-ahead log level
synchronous_commit = off               # Synchronous commit
wal_compression = on                   # Compress WAL files
wal_buffers = 16MB                     # WAL buffers
wal_writer_delay = 200ms               # WAL writer delay

# Checkpoint Settings
checkpoint_timeout = 5min              # Time between checkpoints
checkpoint_completion_target = 0.9     # Checkpoint completion target
checkpoint_flush_after = 256kB         # Flush after checkpoint
checkpoint_warning = 30s               # Warning for long checkpoints

# Query Settings
random_page_cost = 1.1                 # Cost of a random disk page
effective_io_concurrency = 200         # Number of concurrent I/O operations
work_mem = 4MB                         # Memory for sorting and hashing

# Logging Settings
log_destination = 'stderr'             # Log destination
logging_collector = on                 # Enable logging collector
log_directory = 'pg_log'               # Log directory
log_filename = 'postgresql-%Y-%m-%d_%H%M%S.log' # Log filename
log_statement = 'all'                  # Log all statements
log_duration = on                      # Log statement duration
log_line_prefix = '%t [%p]: [%l-1] user=%u,db=%d,app=%a,client=%h ' # Log line prefix
log_timezone = 'UTC'                   # Log timezone

# Autovacuum Settings
autovacuum = on                        # Enable autovacuum
log_autovacuum_min_duration = 0        # Log autovacuum min duration
autovacuum_max_workers = 4             # Maximum autovacuum workers
autovacuum_naptime = 1min              # Time between autovacuum runs
autovacuum_vacuum_scale_factor = 0.2   # Vacuum scale factor
autovacuum_analyze_scale_factor = 0.1  # Analyze scale factor
autovacuum_vacuum_cost_limit = 2000    # Vacuum cost limit
autovacuum_vacuum_cost_delay = 20ms    # Vacuum cost delay

# SSL Settings
ssl = on                               # Enable SSL
ssl_cert_file = 'server.crt'           # SSL certificate file
ssl_key_file = 'server.key'            # SSL private key file
ssl_ca_file = 'root.crt'               # SSL CA certificate file
ssl_ciphers = 'HIGH:!aNULL:!MD5'       # SSL ciphers
ssl_prefer_server_ciphers = on         # Prefer server ciphers
ssl_ecdh_curve = 'prime256v1'          # ECDH curve
ssl_dh_params_file = ''                # DH parameters file

# Replication Settings (if needed)
max_wal_senders = 3                    # Maximum WAL senders
max_replication_slots = 3              # Maximum replication slots
wal_keep_size = 1GB                    # WAL keep size
```

### pg_hba.conf Configuration

Edit the PostgreSQL host-based authentication file:

```bash
# Ubuntu/Debian
sudo nano /etc/postgresql/14/main/pg_hba.conf

# CentOS/RHEL
sudo nano /var/lib/pgsql/14/data/pg_hba.conf

# macOS
nano /usr/local/var/postgres/pg_hba.conf
```

Add the following lines:

```ini
# TYPE  DATABASE        USER            ADDRESS                 METHOD

# Local connections
local   all             all                                     trust

# IPv4 local connections
host    all             all             127.0.0.1/32            md5

# IPv6 local connections
host    all             all             ::1/128                 md5

# Allow application server connections
host    riva_ash        riva_ash         192.168.1.0/24          md5

# Allow replication connections
host    replication     all             192.168.1.0/24          md5
```

### Restart PostgreSQL

```bash
# Ubuntu/Debian
sudo systemctl restart postgresql

# CentOS/RHEL
sudo systemctl restart postgresql

# macOS
brew services restart postgresql

# Docker
docker restart riva-ash-db
```

## Database Migration

### Method 1: Using Ecto Migrations

```bash
# Create migration
mix ecto.gen.migration CreateInitialSchema

# Run migrations
mix ecto.migrate

# Rollback migrations (if needed)
mix ecto.migrate.down

# Reset database (drop and recreate)
mix ecto.reset
```

### Method 2: Manual Migration

```bash
# Connect to database
psql -h localhost -U riva_ash -d riva_ash

-- Create extensions
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pgcrypto";
CREATE EXTENSION IF NOT EXISTS "btree_gin";
CREATE EXTENSION IF NOT EXISTS "btree_gist";

-- Create tables
CREATE TABLE users (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    email VARCHAR(255) UNIQUE NOT NULL,
    username VARCHAR(255) UNIQUE NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    first_name VARCHAR(100),
    last_name VARCHAR(100),
    role VARCHAR(50) NOT NULL DEFAULT 'user',
    status VARCHAR(20) NOT NULL DEFAULT 'active',
    last_login_at TIMESTAMP,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE documents (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    category_id UUID NOT NULL REFERENCES categories(id) ON DELETE CASCADE,
    title VARCHAR(255) NOT NULL,
    description TEXT,
    file_path VARCHAR(500) NOT NULL,
    file_size BIGINT NOT NULL,
    file_type VARCHAR(50) NOT NULL,
    mime_type VARCHAR(100) NOT NULL,
    status VARCHAR(20) NOT NULL DEFAULT 'active',
    metadata JSONB,
    tags TEXT[],
    retention_period INTEGER,
    archival_date TIMESTAMP,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Create indexes
CREATE INDEX idx_users_email ON users(email);
CREATE INDEX idx_users_username ON users(username);
CREATE INDEX idx_users_status ON users(status);
CREATE INDEX idx_documents_user_id ON documents(user_id);
CREATE INDEX idx_documents_category_id ON documents(category_id);
CREATE INDEX idx_documents_status ON documents(status);
CREATE INDEX idx_documents_created_at ON documents(created_at);
CREATE INDEX idx_documents_title ON documents USING gin(to_tsvector('english', title));

-- Create audit table
CREATE TABLE audit_entries (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    entity_type VARCHAR(50) NOT NULL,
    entity_id UUID NOT NULL,
    action VARCHAR(50) NOT NULL,
    user_id UUID REFERENCES users(id),
    changes JSONB,
    ip_address INET,
    user_agent TEXT,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Create indexes for audit table
CREATE INDEX idx_audit_entries_entity_id ON audit_entries(entity_id);
CREATE INDEX idx_audit_entries_entity_type ON audit_entries(entity_type);
CREATE INDEX idx_audit_entries_action ON audit_entries(action);
CREATE INDEX idx_audit_entries_user_id ON audit_entries(user_id);
CREATE INDEX idx_audit_entries_created_at ON audit_entries(created_at);

-- Create function to update updated_at timestamp
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ language 'plpgsql';

-- Create triggers for updated_at
CREATE TRIGGER update_users_updated_at BEFORE UPDATE ON users
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_documents_updated_at BEFORE UPDATE ON documents
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-- Create function for audit logging
CREATE OR REPLACE FUNCTION audit_trigger_function()
RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        INSERT INTO audit_entries (entity_type, entity_id, action, user_id, changes, ip_address, user_agent)
        VALUES (TG_TABLE_NAME, NEW.id, 'create', NEW.user_id, row_to_json(NEW), NULL, NULL);
        RETURN NEW;
    ELSIF TG_OP = 'UPDATE' THEN
        INSERT INTO audit_entries (entity_type, entity_id, action, user_id, changes, ip_address, user_agent)
        VALUES (TG_TABLE_NAME, NEW.id, 'update', NEW.user_id, row_to_json(NEW), NULL, NULL);
        RETURN NEW;
    ELSIF TG_OP = 'DELETE' THEN
        INSERT INTO audit_entries (entity_type, entity_id, action, user_id, changes, ip_address, user_agent)
        VALUES (TG_TABLE_NAME, OLD.id, 'delete', OLD.user_id, row_to_json(OLD), NULL, NULL);
        RETURN OLD;
    END IF;
    RETURN NULL;
END;
$$ language 'plpgsql';

-- Create audit triggers
CREATE TRIGGER audit_users_trigger
    AFTER INSERT OR UPDATE OR DELETE ON users
    FOR EACH ROW EXECUTE FUNCTION audit_trigger_function();

CREATE TRIGGER audit_documents_trigger
    AFTER INSERT OR UPDATE OR DELETE ON documents
    FOR EACH ROW EXECUTE FUNCTION audit_trigger_function();

-- Exit psql
\q
```

### Method 3: Using Docker

```bash
# Run migrations in Docker container
docker exec -it riva-ash-db psql -U riva_ash -d riva_ash -f /path/to/migration.sql
```

## Database Backup

### Method 1: Using pg_dump

```bash
# Create full database backup
pg_dump -h localhost -U riva_ash -d riva_ash > backup_$(date +%Y%m%d_%H%M%S).sql

# Create compressed backup
pg_dump -h localhost -U riva_ash -d riva_ash | gzip > backup_$(date +%Y%m%d_%H%M%S).sql.gz

# Create custom format backup
pg_dump -h localhost -U riva_ash -d riva_ash -Fc > backup_$(date +%Y%m%d_%H%M%S).dump

# Create directory backup
pg_dump -h localhost -U riva_ash -d riva_ash -F d -f backup_$(date +%Y%m%d_%H%M%S)
```

### Method 2: Using pg_dumpall

```bash
# Create cluster backup
pg_dumpall -h localhost -U postgres > cluster_backup_$(date +%Y%m%d_%H%M%S).sql
```

### Method 3: Automated Backup Script

```bash
#!/bin/bash
# scripts/backup_database.sh

BACKUP_DIR="/var/backups/riva_ash"
DATE=$(date +%Y%m%d_%H%M%S)
DB_NAME="riva_ash"
DB_USER="riva_ash"
DB_HOST="localhost"
RETENTION_DAYS=30

# Create backup directory
mkdir -p "$BACKUP_DIR"

# Create database backup
echo "Creating database backup..."
pg_dump -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" | gzip > "$BACKUP_DIR/db_backup_$DATE.sql.gz"

# Keep only last $RETENTION_DAYS of backups
echo "Cleaning up old backups..."
find "$BACKUP_DIR" -name "db_backup_*.sql.gz" -mtime +$RETENTION_DAYS -delete

# Verify backup
echo "Verifying backup..."
if gzip -t "$BACKUP_DIR/db_backup_$DATE.sql.gz"; then
    echo "Backup verification successful"
else
    echo "Backup verification failed"
    exit 1
fi

# Upload to S3 (optional)
if command -v aws &> /dev/null; then
    echo "Uploading backup to S3..."
    aws s3 cp "$BACKUP_DIR/db_backup_$DATE.sql.gz" "s3://riva-ash-backups/db/$DATE.sql.gz"
fi

echo "Backup completed successfully"
```

### Method 4: Using Docker

```bash
# Create backup from Docker container
docker exec riva-ash-db pg_dump -U riva_ash -d riva_ash | gzip > backup_$(date +%Y%m%d_%H%M%S).sql.gz
```

## Database Maintenance

### Regular Maintenance Tasks

```bash
#!/bin/bash
# scripts/maintenance.sh

DB_NAME="riva_ash"
DB_USER="riva_ash"
DB_HOST="localhost"

# Vacuum analyze
echo "Running vacuum analyze..."
psql -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" -c "VACUUM ANALYZE;"

# Reindex tables
echo "Running reindex..."
psql -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" -c "REINDEX DATABASE $DB_NAME;"

# Update statistics
echo "Updating statistics..."
psql -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" -c "ANALYZE;"

# Clean up old audit entries
echo "Cleaning up old audit entries..."
psql -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" -c "
DELETE FROM audit_entries 
WHERE created_at < NOW() - INTERVAL '1 year';
"

# Clean up old document versions
echo "Cleaning up old document versions..."
psql -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" -c "
DELETE FROM document_versions 
WHERE created_at < NOW() - INTERVAL '6 months';
"

echo "Maintenance completed successfully"
```

### Autovacuum Configuration

```sql
-- Check current autovacuum settings
SELECT * FROM pg_settings WHERE name LIKE 'autovacuum%';

-- Adjust autovacuum settings for high write load
ALTER SYSTEM SET autovacuum_vacuum_scale_factor = 0.1;
ALTER SYSTEM SET autovacuum_analyze_scale_factor = 0.05;
ALTER SYSTEM SET autovacuum_vacuum_cost_limit = 2000;
ALTER SYSTEM SET autovacuum_vacuum_cost_delay = 20ms;

-- Reload configuration
SELECT pg_reload_conf();

-- Check autovacuum activity
SELECT schemaname,relname,last_autovacuum,autovacuum_count 
FROM pg_stat_user_tables 
ORDER BY last_autovacuum DESC;
```

## Performance Optimization

### Index Optimization

```sql
-- Create composite indexes for common queries
CREATE INDEX idx_documents_user_category_status ON documents(user_id, category_id, status);
CREATE INDEX idx_documents_created_status ON documents(created_at, status);
CREATE INDEX idx_audit_entries_user_action ON audit_entries(user_id, action);

-- Create partial indexes for better performance
CREATE INDEX idx_documents_active ON documents(id) WHERE status = 'active';
CREATE INDEX idx_audit_recent ON audit_entries(id) 
WHERE created_at > NOW() - INTERVAL '30 days';

-- Create expression indexes for full-text search
CREATE INDEX idx_documents_search ON documents USING gin(to_tsvector('english', title || ' ' || COALESCE(description, '')));
```

### Query Optimization

```sql
-- Enable query logging
SET log_min_duration_statement = 1000;  -- Log queries running longer than 1 second
SET log_statement = 'all';

-- Create query performance view
CREATE OR REPLACE VIEW slow_queries AS
SELECT query, mean_time, calls, rows, 100.0 * shared_blks_hit / nullif(shared_blks_hit + shared_blks_read, 0) AS hit_percent
FROM pg_stat_statements
ORDER BY mean_time DESC
LIMIT 10;

-- Analyze query performance
EXPLAIN ANALYZE SELECT * FROM documents WHERE user_id = 'uuid' AND status = 'active' ORDER BY created_at DESC LIMIT 10;
```

### Memory Configuration

```sql
-- Check current memory settings
SHOW shared_buffers;
SHOW effective_cache_size;
SHOW work_mem;
SHOW maintenance_work_mem;

-- Adjust memory settings for better performance
ALTER SYSTEM SET shared_buffers = '512MB';
ALTER SYSTEM SET effective_cache_size = '2GB';
ALTER SYSTEM SET work_mem = '8MB';
ALTER SYSTEM SET maintenance_work_mem = '256MB';

-- Reload configuration
SELECT pg_reload_conf();
```

## Security Configuration

### SSL Configuration

```sql
-- Check SSL status
SHOW ssl;

-- Create SSL certificates (if not already created)
openssl req -new -x509 -days 365 -nodes -text -out server.crt \
  -keyout server.key -subj "/CN=localhost"

-- Set SSL permissions
chmod 600 server.key
chmod 644 server.crt

-- Configure SSL in PostgreSQL
ALTER SYSTEM SET ssl = on;
ALTER SYSTEM SET ssl_cert_file = 'server.crt';
ALTER SYSTEM SET ssl_key_file = 'server.key';

-- Reload configuration
SELECT pg_reload_conf();
```

### User and Role Management

```sql
-- Create read-only role
CREATE ROLE riva_ash_readonly;
GRANT CONNECT ON DATABASE riva_ash TO riva_ash_readonly;
GRANT USAGE ON SCHEMA public TO riva_ash_readonly;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO riva_ash_readonly;
GRANT SELECT ON ALL SEQUENCES IN SCHEMA public TO riva_ash_readonly;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO riva_ash_readonly;

-- Create read-write role
CREATE ROLE riva_ash_readwrite;
GRANT CONNECT ON DATABASE riva_ash TO riva_ash_readwrite;
GRANT USAGE ON SCHEMA public TO riva_ash_readwrite;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO riva_ash_readwrite;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA public TO riva_ash_readwrite;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO riva_ash_readwrite;

-- Create admin role
CREATE ROLE riva_ash_admin WITH LOGIN SUPERUSER;
GRANT ALL PRIVILEGES ON DATABASE riva_ash TO riva_ash_admin;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO riva_ash_admin;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO riva_ash_admin;
```

### Row Level Security

```sql
-- Enable row level security
ALTER TABLE documents ENABLE ROW LEVEL SECURITY;

-- Create policies
CREATE POLICY user_documents_policy ON documents
    FOR ALL TO riva_ash_readonly
    USING (user_id = current_user_id());

CREATE POLICY admin_documents_policy ON documents
    FOR ALL TO riva_ash_admin
    USING (true);

-- Create function to get current user ID
CREATE OR REPLACE FUNCTION current_user_id()
RETURNS UUID AS $$
BEGIN
    RETURN (SELECT id FROM users WHERE email = current_user());
END;
$$ LANGUAGE plpgsql SECURITY DEFINER;
```

## Monitoring and Health Checks

### Database Monitoring

```sql
-- Check database connections
SELECT count(*) FROM pg_stat_activity WHERE state = 'active';

-- Check long-running queries
SELECT query, now() - query_start AS duration, state
FROM pg_stat_activity
WHERE state != 'idle'
ORDER BY duration DESC;

-- Check table sizes
SELECT 
    schemaname,
    tablename,
    pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename)) AS size,
    pg_size_pretty(pg_relation_size(schemaname||'.'||tablename)) AS table_size,
    pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename) - pg_relation_size(schemaname||'.'||tablename)) AS index_size
FROM pg_tables
WHERE schemaname = 'public'
ORDER BY pg_total_relation_size(schemaname||'.'||tablename) DESC;

-- Check index usage
SELECT 
    schemaname,
    tablename,
    indexname,
    idx_scan,
    idx_tup_read,
    idx_tup_fetch
FROM pg_stat_user_indexes
ORDER BY idx_scan DESC;
```

### Health Check Script

```bash
#!/bin/bash
# scripts/health_check.sh

DB_NAME="riva_ash"
DB_USER="riva_ash"
DB_HOST="localhost"

# Check database connection
if ! psql -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" -c "SELECT 1;" > /dev/null 2>&1; then
    echo "Database connection failed"
    exit 1
fi

# Check active connections
ACTIVE_CONNECTIONS=$(psql -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" -t -c "SELECT count(*) FROM pg_stat_activity WHERE state = 'active';")
if [ "$ACTIVE_CONNECTIONS" -gt 100 ]; then
    echo "Warning: High number of active connections: $ACTIVE_CONNECTIONS"
fi

# Check disk space
DISK_USAGE=$(df /var/lib/postgresql/14/ | tail -1 | awk '{print $5}' | sed 's/%//')
if [ "$DISK_USAGE" -gt 90 ]; then
    echo "Critical: Disk space usage is ${DISK_USAGE}%"
    exit 1
fi

# Check replication status (if applicable)
if [ -n "$REPLICATION_MODE" ]; then
    REPLICATION_STATUS=$(psql -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" -t -c "SELECT count(*) FROM pg_stat_replication WHERE application_name = 'riva_ash';")
    if [ "$REPLICATION_STATUS" -eq 0 ]; then
        echo "Warning: No active replication connections"
    fi
fi

echo "Database health check passed"
```

## Troubleshooting

### Common Issues

#### Connection Issues

```bash
# Check PostgreSQL status
sudo systemctl status postgresql

# Check PostgreSQL logs
sudo tail -f /var/log/postgresql/postgresql-14-main.log

# Check port availability
sudo netstat -tlnp | grep 5432

# Test connection
psql -h localhost -U riva_ash -d riva_ash
```

#### Permission Issues

```bash
# Check user permissions
psql -h localhost -U riva_ash -d riva_ash -c "\du"

# Check database permissions
psql -h localhost -U riva_ash -d riva_ash -c "\l"

# Grant missing permissions
psql -h localhost -U postgres -d riva_ash -c "GRANT ALL PRIVILEGES ON DATABASE riva_ash TO riva_ash;"
```

#### Performance Issues

```bash
# Check slow queries
SELECT query, mean_time, calls
FROM pg_stat_statements
ORDER BY mean_time DESC
LIMIT 10;

# Check lock contention
SELECT blocked_locks.pid AS blocked_pid,
       blocked_activity.usename AS blocked_user,
       blocking_locks.pid AS blocking_pid,
       blocking_activity.usename AS blocking_user,
       blocked_activity.query AS blocked_statement,
       blocking_activity.query AS current_statement_in_blocking_process,
       blocked_activity.application_name AS blocked_application,
       blocking_activity.application_name AS blocking_application
FROM pg_catalog.pg_locks blocked_locks
JOIN pg_catalog.pg_stat_activity blocked_activity ON blocked_activity.pid = blocked_locks.pid
JOIN pg_catalog.pg_locks blocking_locks
    ON blocking_locks.locktype = blocked_locks.locktype
    AND blocking_locks.DATABASE IS NOT DISTINCT FROM blocked_locks.DATABASE
    AND blocking_locks.relation IS NOT DISTINCT FROM blocked_locks.relation
    AND blocking_locks.page IS NOT DISTINCT FROM blocked_locks.page
    AND blocking_locks.tuple IS NOT DISTINCT FROM blocked_locks.tuple
    AND blocking_locks.virtualxid IS NOT DISTINCT FROM blocked_locks.virtualxid
    AND blocking_locks.transactionid IS NOT DISTINCT FROM blocked_locks.transactionid
    AND blocking_locks.classid IS NOT DISTINCT FROM blocked_locks.classid
    AND blocking_locks.objid IS NOT DISTINCT FROM blocked_locks.objid
    AND blocking_locks.objsubid IS NOT DISTINCT FROM blocked_locks.objsubid
    AND blocking_locks.pid != blocked_locks.pid
JOIN pg_catalog.pg_stat_activity blocking_activity ON blocking_activity.pid = blocking_locks.pid
WHERE NOT blocked_locks.GRANTED;
```

### Recovery Procedures

#### Point-in-Time Recovery

```bash
# Stop PostgreSQL
sudo systemctl stop postgresql

# Restore from backup
cp /var/backups/riva_ash/db_backup_20231201_120000.sql.gz /var/lib/postgresql/14/main/
gunzip /var/lib/postgresql/14/main/db_backup_20231201_120000.sql.gz
psql -U postgres -d riva_ash -f /var/lib/postgresql/14/main/db_backup_20231201_120000.sql

# Start PostgreSQL
sudo systemctl start postgresql
```

#### Emergency Recovery

```bash
# Start PostgreSQL in single-user mode
sudo systemctl stop postgresql
sudo -u postgres postgres --single -D /var/lib/postgresql/14/main

# In single-user mode
ALTER USER riva_ash WITH SUPERUSER;
\q

# Restart PostgreSQL
sudo systemctl start postgresql
```

## Best Practices

### Security Best Practices

1. **Use SSL for all connections**
   - Enable SSL in PostgreSQL
   - Use SSL certificates
   - Configure client connections to use SSL

2. **Implement proper access controls**
   - Use least privilege principle
   - Create specific roles for different access levels
   - Enable row-level security where needed

3. **Regular security audits**
   - Review user permissions regularly
   - Check for unused accounts
   - Monitor for suspicious activity

### Performance Best Practices

1. **Optimize indexes**
   - Create indexes for frequently queried columns
   - Remove unused indexes
   - Use composite indexes for common query patterns

2. **Monitor and tune queries**
   - Use EXPLAIN ANALYZE to analyze query performance
   - Identify and optimize slow queries
   - Use query hints when necessary

3. **Configure memory properly**
   - Set appropriate shared_buffers size
   - Configure work_mem for your workload
   - Use maintenance_work_mem for maintenance operations

### Backup Best Practices

1. **Regular backups**
   - Schedule regular automated backups
   - Test backup restoration regularly
   - Keep multiple backup copies

2. **Backup verification**
   - Verify backup integrity regularly
   - Test backup restoration procedures
   - Document recovery procedures

3. **Off-site backups**
   - Store backups off-site
   - Use cloud storage for backup redundancy
   - Implement backup encryption

### Maintenance Best Practices

1. **Regular maintenance**
   - Schedule regular VACUUM and ANALYZE operations
   - Monitor database performance
   - Update statistics regularly

2. **Monitoring and alerting**
   - Set up monitoring for database health
   - Configure alerts for critical issues
   - Monitor resource usage

3. **Documentation**
   - Maintain database documentation
   - Document configuration changes
   - Keep backup and recovery procedures up to date

This database setup guide provides comprehensive instructions for configuring and maintaining the PostgreSQL database for the Riva Ash application. Follow these best practices to ensure optimal performance, security, and reliability of your database.