# Production Deployment Guide

This guide covers deploying the Reservation System to production with proper
security, monitoring, and scalability considerations.

## Required Environment Variables

### Database Configuration

```bash
DATABASE_URL="postgresql://username:password@hostname:5432/database_name"
# OR individual components:
DB_USERNAME="your_db_user"
DB_PASSWORD="your_secure_db_password"
DB_HOSTNAME="your_db_host"
DB_NAME="reservation_system_prod"
DB_PORT="5432"
DB_POOL_SIZE="10"
```

### Application Secrets

```bash
# REQUIRED: Generate with `mix phx.gen.secret`
SECRET_KEY_BASE="your_64_character_secret_key_base_here"

# REQUIRED: Generate with `mix phx.gen.secret`
AUTH_TOKEN_SECRET="your_64_character_auth_token_secret_here"

# REQUIRED: Generate with `mix phx.gen.secret`
LIVE_VIEW_SIGNING_SALT="your_32_character_signing_salt_here"
```

### Application Configuration

```bash
# Application environment
MIX_ENV="prod"
PHX_HOST="your-domain.com"
PORT="4000"

# SSL Configuration (recommended)
FORCE_SSL="true"
SSL_PORT="443"

# Optional: Custom configuration
POOL_SIZE="10"
LOG_LEVEL="info"
```

## Docker Deployment

### Using Docker Compose

1. **Create production docker-compose.yml:**

```yaml
version: "3.8"

services:
    app:
        build: .
        ports:
            - "4000:4000"
        environment:
            - MIX_ENV=prod
            - DATABASE_URL=${DATABASE_URL}
            - SECRET_KEY_BASE=${SECRET_KEY_BASE}
            - AUTH_TOKEN_SECRET=${AUTH_TOKEN_SECRET}
            - LIVE_VIEW_SIGNING_SALT=${LIVE_VIEW_SIGNING_SALT}
            - PHX_HOST=${PHX_HOST}
            - PORT=4000
        depends_on:
            - db
        restart: unless-stopped
        healthcheck:
            test: ["CMD", "curl", "-f", "http://localhost:4000/health"]
            interval: 30s
            timeout: 10s
            retries: 3

    db:
        image: postgres:15
        environment:
            - POSTGRES_DB=reservation_system_prod
            - POSTGRES_USER=${DB_USERNAME}
            - POSTGRES_PASSWORD=${DB_PASSWORD}
        volumes:
            - postgres_data:/var/lib/postgresql/data
        restart: unless-stopped

volumes:
    postgres_data:
```

2. **Create .env file (DO NOT COMMIT):**

```bash
# Database
DATABASE_URL=postgresql://reservation_user:secure_password@db:5432/reservation_system_prod
DB_USERNAME=reservation_user
DB_PASSWORD=secure_password

# Application secrets (generate these!)
SECRET_KEY_BASE=your_generated_secret_key_base_here
AUTH_TOKEN_SECRET=your_generated_auth_token_secret_here
LIVE_VIEW_SIGNING_SALT=your_generated_signing_salt_here

# Application
PHX_HOST=your-domain.com
```

3. **Deploy:**

```bash
docker-compose up -d
```

### Health Checks

The application provides multiple health check endpoints:

-   **Liveness**: `GET /health/liveness` - Basic application health
-   **Readiness**: `GET /health/readiness` - Ready to serve traffic (includes DB
    check)
-   **Detailed**: `GET /health` - Comprehensive system status

Configure your load balancer/orchestrator to use these endpoints:

-   **Liveness probe**: `/health/liveness` (for restart decisions)
-   **Readiness probe**: `/health/readiness` (for traffic routing)

## Security Considerations

### Environment Variables

-   **Never commit secrets** to version control
-   Use a secure secret management system in production
-   Rotate secrets regularly
-   Use different secrets for each environment

### Database Security

-   Use strong, unique passwords
-   Enable SSL/TLS for database connections
-   Restrict database access to application servers only
-   Regular security updates for PostgreSQL

### Application Security

-   Enable HTTPS/SSL in production (`FORCE_SSL=true`)
-   Use secure headers (configured in runtime.exs)
-   Regular security updates for dependencies
-   Monitor for security vulnerabilities

### Network Security

-   Use firewalls to restrict access
-   Consider using a VPN for database access
-   Implement rate limiting
-   Use a reverse proxy (nginx, Cloudflare, etc.)

## Monitoring and Logging

### Application Logs

```bash
# Set appropriate log level
LOG_LEVEL=info  # or warn, error for production

# Structured logging is enabled by default
# Logs include request IDs for tracing
```

### Health Monitoring

Monitor these endpoints:

-   `/health/liveness` - Application is running
-   `/health/readiness` - Application can serve traffic
-   `/health` - Detailed system metrics

### Database Monitoring

-   Connection pool usage
-   Query performance
-   Database size and growth
-   Backup status

### Performance Metrics

-   Response times
-   Memory usage
-   CPU usage
-   Error rates

## Scaling Considerations

### Horizontal Scaling

-   The application is stateless and can be scaled horizontally
-   Use a load balancer to distribute traffic
-   Database connection pooling is configured per instance

### Database Scaling

-   Consider read replicas for read-heavy workloads
-   Monitor connection pool usage
-   Optimize queries and add indexes as needed

### Caching

-   Phoenix LiveView includes built-in optimizations
-   Consider adding Redis for session storage if needed
-   Use CDN for static assets

## Backup and Recovery

### Database Backups

```bash
# Automated backup script
pg_dump $DATABASE_URL > backup_$(date +%Y%m%d_%H%M%S).sql

# Restore from backup
psql $DATABASE_URL < backup_file.sql
```

### Application State

-   The application is stateless
-   All persistent data is in the database
-   Configuration is via environment variables

## Troubleshooting

### Common Issues

1. **Application won't start**

    - Check all required environment variables are set
    - Verify database connectivity
    - Check logs for specific error messages

2. **Database connection errors**

    - Verify DATABASE_URL format
    - Check database server is running
    - Verify network connectivity
    - Check connection pool settings

3. **Authentication issues**
    - Verify AUTH_TOKEN_SECRET is set
    - Check SECRET_KEY_BASE is properly configured
    - Ensure LIVE_VIEW_SIGNING_SALT is set

### Log Analysis

```bash
# View application logs
docker-compose logs app

# Follow logs in real-time
docker-compose logs -f app

# Filter for errors
docker-compose logs app | grep ERROR
```

### Health Check Debugging

```bash
# Test health endpoints
curl http://your-domain.com/health/liveness
curl http://your-domain.com/health/readiness
curl http://your-domain.com/health
```

## Maintenance

### Updates

1. Test updates in staging environment first
2. Create database backup before updates
3. Use rolling deployments to minimize downtime
4. Monitor health checks during deployment

### Database Migrations

```bash
# Run migrations in production
docker-compose exec app mix ecto.migrate
```

### Security Updates

-   Monitor security advisories for Elixir/Phoenix
-   Update dependencies regularly
-   Test updates in staging first
-   Have rollback plan ready
