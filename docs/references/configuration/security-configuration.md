# Security Configuration Guide

This document provides comprehensive guidance on configuring security settings for the Riva Ash document management system. It covers authentication, authorization, encryption, network security, and monitoring to ensure the application meets security best practices and compliance requirements.

## Table of Contents

1. [Overview](#overview)
2. [Authentication Configuration](#authentication-configuration)
3. [Authorization Configuration](#authorization-configuration)
4. [Encryption Configuration](#encryption-configuration)
5. [Network Security](#network-security)
6. [File Security](#file-security)
7. [Session Management](#session-management)
8. [Rate Limiting](#rate-limiting)
9. [CORS Configuration](#cors-configuration)
10. [Security Headers](#security-headers)
11. [Monitoring and Auditing](#monitoring-and-auditing)
12. [Compliance Requirements](#compliance-requirements)
13. [Security Best Practices](#security-best-practices)
14. [Incident Response](#incident-response)

## Overview

Security is a critical aspect of the Riva Ash document management system. This guide provides detailed configuration options to ensure the application is secure against common threats and vulnerabilities. The security configuration covers multiple layers including application, database, network, and infrastructure security.

### Security Principles

- **Defense in Depth**: Implement multiple layers of security controls
- **Least Privilege**: Grant only necessary permissions to users and systems
- **Zero Trust**: Verify all users and devices regardless of network location
- **Security by Design**: Integrate security into the development lifecycle
- **Continuous Monitoring**: Monitor security posture continuously

### Security Scope

The security configuration covers:
- **Application Security**: Authentication, authorization, and input validation
- **Data Security**: Encryption at rest and in transit
- **Network Security**: Firewalls, VPNs, and secure communication
- **Infrastructure Security**: Server hardening and access controls
- **Compliance**: GDPR, HIPAA, and other regulatory requirements

## Authentication Configuration

### Authentication Methods

#### JWT Authentication

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Auth,
  token_expiry: 3600, # 1 hour
  refresh_token_expiry: 86400, # 24 hours
  max_refresh_tokens: 5,
  jwt_algorithm: "HS256",
  jwt_issuer: "riva_ash",
  jwt_audience: "riva_ash_users",
  password_hashing_algorithm: :argon2,
  argon2_opts: [
    memory_cost: 32768,
    parallelism: 4,
    iterations: 3
  ]
```

#### OAuth 2.0 Configuration

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
      redirect_uri: "https://your-domain.com/auth/google/callback",
      scopes: ["email", "profile"]
    },
    %{
      name: :github,
      strategy: Ueberauth.Strategy.GitHub.OAuth,
      uid_field: "id",
      client_id: System.get_env("GITHUB_CLIENT_ID"),
      client_secret: System.get_env("GITHUB_CLIENT_SECRET"),
      redirect_uri: "https://your-domain.com/auth/github/callback",
      scopes: ["user:email"]
    }
  ]
```

#### Multi-Factor Authentication

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Auth.MFA,
  enabled: true,
  totp_issuer: "Riva Ash",
  totp_period: 30,
  totp_window: 1,
  backup_codes_count: 10,
  recovery_codes_expiry: 365, # days
  sms_provider: System.get_env("SMS_PROVIDER"),
  email_provider: System.get_env("EMAIL_PROVIDER")
```

### Password Policy

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Auth.PasswordPolicy,
  min_length: 12,
  max_length: 128,
  require_uppercase: true,
  require_lowercase: true,
  require_numbers: true,
  require_special_chars: true,
  prevent_reuse: 5,
  expiry_days: 90,
  warning_days: 7,
  lockout_attempts: 5,
  lockout_duration: 900 # 15 minutes
```

### Authentication Pipeline

```elixir
# config/config.exs
config :riva_ash, RivaAshWeb.AuthPipeline,
  module: RivaAshWeb.Auth,
  error_handler: RivaAshWeb.AuthErrorHandler,
  session_store: :cookie,
  secure_cookie: true,
  same_site: :strict,
  max_age: 1800, # 30 minutes
  http_only: true,
  extra: ["SameSite=Lax"],
  require_existing_session: true,
  login_timeout: 300 # 5 minutes
```

## Authorization Configuration

### Role-Based Access Control

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Auth.Roles,
  roles: %{
    "admin" => [
      "users:*",
      "documents:*",
      "categories:*",
      "audit:*",
      "system:*",
      "settings:*"
    ],
    "manager" => [
      "users:read",
      "users:create",
      "users:update",
      "documents:*",
      "categories:*",
      "audit:read",
      "reports:*"
    ],
    "user" => [
      "documents:read",
      "documents:create",
      "documents:update",
      "documents:delete",
      "categories:read",
      "audit:read"
    ],
    "viewer" => [
      "documents:read",
      "categories:read",
      "audit:read"
    ]
  }
```

### Permission System

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Auth.Permissions,
  permissions: %{
    "users:read" => "Read user information",
    "users:create" => "Create new users",
    "users:update" => "Update existing users",
    "users:delete" => "Delete users",
    "documents:read" => "Read documents",
    "documents:create" => "Create documents",
    "documents:update" => "Update documents",
    "documents:delete" => "Delete documents",
    "documents:download" => "Download documents",
    "documents:share" => "Share documents",
    "categories:read" => "Read categories",
    "categories:create" => "Create categories",
    "categories:update" => "Update categories",
    "categories:delete" => "Delete categories",
    "audit:read" => "Read audit logs",
    "audit:export" => "Export audit logs",
    "reports:generate" => "Generate reports",
    "system:backup" => "Perform system backups",
    "system:restore" => "Restore system from backup",
    "settings:read" => "Read system settings",
    "settings:update" => "Update system settings"
  }
```

### Access Control Lists

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Auth.ACL,
  document_access: %{
    "public" => ["viewer", "user", "manager", "admin"],
    "internal" => ["user", "manager", "admin"],
    "confidential" => ["admin"],
    "restricted" => ["manager", "admin"]
  },
  category_access: %{
    "general" => ["viewer", "user", "manager", "admin"],
    "financial" => ["user", "manager", "admin"],
    "hr" => ["manager", "admin"],
    "legal" => ["admin"]
  }
```

## Encryption Configuration

### Data Encryption

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Encryption,
  key: System.get_env("ENCRYPTION_KEY"),
  algorithm: :aes_256_gcm,
  iterations: 10000,
  length: 32,
  digest: :sha256,
  salt: "your-salt-value"
```

### Database Encryption

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Repo.Encryption,
  enabled: true,
  key: System.get_env("DB_ENCRYPTION_KEY"),
  algorithm: :aes_256_gcm,
  fields: [
    "users.password_hash",
    "documents.metadata",
    "audit_entries.changes"
  ]
```

### File Encryption

```elixir
# config/config.exs
config :riva_ash, RivaAsh.FileEncryption,
  enabled: true,
  algorithm: :aes_256_gcm,
  key: System.get_env("FILE_ENCRYPTION_KEY"),
  chunk_size: 64 * 1024, # 64KB chunks
  iv_length: 16,
  tag_length: 16
```

### Transport Layer Security

```elixir
# config/config.exs
config :riva_ash, RivaAshWeb.Endpoint,
  https: [
    port: 443,
    cipher_suite: :strong,
    keyfile: System.get_env("SSL_KEY_PATH"),
    certfile: System.get_env("SSL_CERT_PATH"),
    transport_options: [
      verify: :verify_none,
      verify_fun: {&RivaAshWeb.SSLVerification.verify/3, :valid}
    ]
  ],
  force_ssl: [rewrite_on: [:x_forwarded_proto]],
  check_origin: false,
  server: true
```

## Network Security

### Firewall Configuration

```bash
#!/bin/bash
# scripts/setup_firewall.sh

# Allow SSH
ufw allow OpenSSH

# Allow HTTP and HTTPS
ufw allow 'Nginx Full'

# Allow PostgreSQL (if needed)
ufw allow 5432/tcp

# Allow Redis (if needed)
ufw allow 6379/tcp

# Allow application server
ufw allow 4000/tcp

# Deny all other incoming connections
ufw default deny incoming

# Enable firewall
ufw --force enable

# Show status
ufw status
```

### Network Segmentation

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Network,
  trusted_networks: [
    "192.168.1.0/24",  # Internal network
    "10.0.0.0/8",      # VPN network
    "172.16.0.0/12"    # Additional trusted network
  ],
  allowed_ips: [
    "127.0.0.1/32",    # Localhost
    "::1/128"          # IPv6 localhost
  ],
  block_unknown_ips: true,
  rate_limit_by_ip: true,
  max_requests_per_minute: 100
```

### VPN Configuration

```elixir
# config/config.exs
config :riva_ash, RivaAsh.VPN,
  enabled: true,
  provider: "openvpn",
  config_path: "/etc/openvpn/riva_ash.conf",
  user_cert_path: "/etc/openvpn/users",
  group_cert_path: "/etc/openvpn/groups",
  cert_expiry_days: 365,
  renew_days_before_expiry: 30
```

## File Security

### File Upload Security

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Upload,
  max_file_size: 100_000_000, # 100MB
  allowed_extensions: ~w(.pdf .doc .docx .txt .jpg .png .xlsx .pptx),
  allowed_mime_types: [
    "application/pdf",
    "application/msword",
    "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    "text/plain",
    "image/jpeg",
    "image/png",
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    "application/vnd.openxmlformats-officedocument.presentationml.presentation"
  ],
  virus_scanning: true,
  ocr_processing: false,
  file_validation: true,
  sanitize_filenames: true,
  max_filename_length: 255
```

### File Storage Security

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Storage,
  backend: :s3,
  encryption: true,
  access_control: true,
  versioning: true,
  lifecycle_rules: [
    %{
      id: "delete_old_versions",
      status: "Enabled",
      noncurrent_version_expiration_in_days: 30
    },
    %{
      id: "transition_to_ia",
      status: "Enabled",
      transition_to_ia_in_days: 30
    },
    %{
      id: "transition_to_glacier",
      status: "Enabled",
      transition_to_glacier_in_days: 90
    }
  ]
```

### File Access Control

```elixir
# config/config.exs
config :riva_ash, RivaAsh.FileAccess,
  download_requires_auth: true,
  download_requires_permission: "documents:download",
  share_requires_auth: true,
  share_requires_permission: "documents:share",
  share_expiry_days: 7,
  max_shares_per_user: 10,
  enable_watermarking: true,
  watermark_text: "Confidential - Riva Ash"
```

## Session Management

### Session Configuration

```elixir
# config/config.exs
config :riva_ash, RivaAshWeb.AuthPipeline,
  session_store: :cookie,
  secure_cookie: true,
  same_site: :strict,
  max_age: 1800, # 30 minutes
  http_only: true,
  extra: ["SameSite=Lax"],
  domain: ".your-domain.com",
  path: "/",
  key_base: System.get_env("SESSION_KEY_BASE"),
  signing_salt: System.get_env("SESSION_SIGNING_SALT")
```

### Session Security

```elixir
# config/config.exs
config :riva_ash, RivaAshWeb.Session,
  regenerate_on_login: true,
  regenerate_on_password_change: true,
  timeout: 1800, # 30 minutes
  absolute_timeout: 86400, # 24 hours
  concurrent_sessions: 3,
  track_ip_address: true,
  track_user_agent: true,
  track_device_fingerprint: true
```

### Session Cleanup

```elixir
# config/config.exs
config :riva_ash, RivaAshWeb.SessionCleanup,
  enabled: true,
  interval: 300, # 5 minutes
  max_age: 86400, # 24 hours
  batch_size: 100,
  timeout: 5000 # 5 seconds
```

## Rate Limiting

### Rate Limit Configuration

```elixir
# config/config.exs
config :riva_ash, RivaAshWeb.RateLimiter,
  enabled: true,
  storage: :ets,
  max_requests: 100,
  time_window: 60, # seconds
  exclude: ["/health", "/metrics", "/api/docs"],
  by_ip: true,
  by_user: true,
  by_endpoint: true,
  custom_limits: %{
    "/api/auth/login" => %{max_requests: 5, time_window: 300}, # 5 per 5 minutes
    "/api/documents/upload" => %{max_requests: 10, time_window: 60}, # 10 per minute
    "/api/documents/search" => %{max_requests: 50, time_window: 60} # 50 per minute
  }
```

### Rate Limit Storage

```elixir
# config/config.exs
config :riva_ash, RivaAshWeb.RateLimiter.Storage,
  backend: :ets,
  table_name: :riva_ash_rate_limit,
  cleanup_interval: 300, # 5 minutes
  max_ttl: 3600 # 1 hour
```

### IP Whitelisting

```elixir
# config/config.exs
config :riva_ash, RivaAshWeb.RateLimiter,
  ip_whitelist: [
    "127.0.0.1",
    "::1",
    "192.168.1.0/24",
    "10.0.0.0/8"
  ],
  ip_blacklist: [
    "1.2.3.4",
    "5.6.7.8/32"
  ]
```

## CORS Configuration

### CORS Settings

```elixir
# config/config.exs
config :cors_plug, RivaAshWeb.CORSPlug,
  origin: [
    "https://your-domain.com",
    "https://app.your-domain.com",
    "https://admin.your-domain.com"
  ],
  max_age: 86400,
  methods: ["GET", "POST", "PUT", "DELETE", "OPTIONS"],
  headers: [
    "Authorization",
    "Content-Type",
    "Accept",
    "Origin",
    "User-Agent",
    "X-Requested-With",
    "X-Auth-Token"
  ],
  expose: [
    "X-Request-ID",
    "X-RateLimit-Limit",
    "X-RateLimit-Remaining",
    "X-RateLimit-Reset",
    "X-Response-Time"
  ],
  credentials: true,
  send_preflight_response: true
```

### CORS Environment Configuration

```elixir
# config/config.exs
config :riva_ash, RivaAshWeb.CORS,
  development: %{
    origin: ["http://localhost:3000", "http://localhost:8080"],
    max_age: 3600,
    credentials: true
  },
  test: %{
    origin: ["http://localhost:4000"],
    max_age: 3600,
    credentials: true
  },
  production: %{
    origin: ["https://your-domain.com"],
    max_age: 86400,
    credentials: true
  }
```

## Security Headers

### Security Header Configuration

```elixir
# config/config.exs
config :riva_ash, RivaAshWeb.SecurityHeaders,
  enabled: true,
  headers: [
    {"X-Frame-Options", "DENY"},
    {"X-Content-Type-Options", "nosniff"},
    {"X-XSS-Protection", "1; mode=block"},
    {"Strict-Transport-Security", "max-age=31536000; includeSubDomains; preload"},
    {"Content-Security-Policy", "default-src 'self'; script-src 'self' 'unsafe-inline' 'unsafe-eval'; style-src 'self' 'unsafe-inline'; img-src 'self' data:; font-src 'self' data:; connect-src 'self' https://api.your-domain.com"},
    {"Referrer-Policy", "strict-origin-when-cross-origin"},
    {"Permissions-Policy", "geolocation=(), microphone=(), camera=(), payment=(), usb=(), display-capture=()"},
    {"X-Content-Security-Policy", "default-src 'self'"},
    {"X-WebKit-CSP", "default-src 'self'"},
    {"Expect-CT", "max-age=604800, enforce, report-uri=https://your-domain.com/report-ct"}
  ]
```

### Content Security Policy

```elixir
# config/config.exs
config :riva_ash, RivaAshWeb.CSP,
  default_src: ["'self'"],
  script_src: ["'self'", "'unsafe-inline'", "'unsafe-eval'"],
  style_src: ["'self'", "'unsafe-inline'"],
  img_src: ["'self'", "data:", "https:"],
  font_src: ["'self'", "data:"],
  connect_src: ["'self'", "https://api.your-domain.com"],
  media_src: ["'self'", "https:"],
  object_src: ["'none'"],
  child_src: ["'self'"],
  frame_src: ["'self'"],
  frame_ancestors: ["'none'"],
  form_action: ["'self'"],
  manifest_src: ["'self'"],
  worker_src: ["'self'"],
  sandbox: ["allow-scripts", "allow-same-origin"],
  report_uri: ["/csp-report"]
```

## Monitoring and Auditing

### Security Monitoring

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Security.Monitoring,
  enabled: true,
  log_failed_logins: true,
  log_failed_access_attempts: true,
  log_permission_denials: true,
  log_configuration_changes: true,
  monitor_suspicious_activity: true,
  alert_threshold: 5, # events per minute
  alert_cooldown: 300, # 5 minutes
  retention_days: 90
```

### Audit Logging

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Audit,
  enabled: true,
  log_level: :info,
  log_format: :json,
  log_to_file: true,
  log_to_database: true,
  log_to_external: false,
  retention_days: 365,
  sensitive_fields: [
    "password",
    "password_hash",
    "credit_card",
    "ssn",
    "api_key"
  ]
```

### Security Alerts

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Security.Alerts,
  enabled: true,
  channels: [
    %{
      type: :email,
      recipients: ["security@your-domain.com"],
      template: "security_alert.html"
    },
    %{
      type: :slack,
      webhook_url: System.get_env("SLACK_SECURITY_WEBHOOK"),
      channel: "#security-alerts"
    }
  ],
  alert_types: [
    :failed_login,
    :brute_force,
    :permission_denial,
    :data_access,
    :configuration_change,
    :suspicious_activity
  ]
```

## Compliance Requirements

### GDPR Compliance

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Compliance.GDPR,
  enabled: true,
  data_subject_rights: [
    :right_to_access,
    :right_to_rectification,
    :right_to_erasure,
    :right_to_restrict_processing,
    :right_to_data_portability,
    :right_to_object,
    :right_to_not_be_subject_to_automated_decision
  ],
  retention_policies: %{
    "user_data" => 365, # days
    "document_metadata" => 2555, # 7 years
    "audit_logs" => 2555, # 7 years
    "system_logs" => 90 # 3 months
  },
  data_processing_agreements: true,
  data_protection_officer: "dpo@your-domain.com"
```

### HIPAA Compliance

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Compliance.HIPAA,
  enabled: false, # Only enable if handling PHI
  encryption_required: true,
  access_controls: true,
  audit_logging: true,
  incident_notification: true,
  business_associate_agreements: true,
  training_requirements: true,
    risk_assessment: true
```

### SOC 2 Compliance

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Compliance.SOC2,
  enabled: true,
  security_criteria: [
    :access_controls,
    :awareness_and_training,
    :change_management,
    :configuration_management,
    :data_security,
    :environmental_security,
    :incident_management,
    :maintenance,
    :media_protection,
    :network_security,
    :operations_security,
    :physical_security,
    :security_operations_center,
    :system_operations,
    :system_security_management,
    :vulnerability_management,
    :wireless_security
  ],
  availability_criteria: [
    :infrastructure,
    :protection_of_information_processing_systems,
    :environmental_facilities,
    :storage,
    :online_services,
    :operations,
    :software,
    :system_capacity,
    :system_performance,
    :system_scheduling
  ]
```

## Security Best Practices

### Application Security

1. **Input Validation**
   - Validate all user inputs
   - Use parameterized queries
   - Sanitize output
   - Implement content security policy

2. **Authentication**
   - Use strong password policies
   - Implement multi-factor authentication
   - Use secure session management
   - Implement account lockout policies

3. **Authorization**
   - Implement role-based access control
   - Use principle of least privilege
   - Implement attribute-based access control
   - Regular access reviews

### Data Security

1. **Encryption**
   - Encrypt sensitive data at rest
   - Use TLS for data in transit
   - Implement proper key management
   - Use strong encryption algorithms

2. **Data Protection**
   - Implement data loss prevention
   - Use data masking
   - Implement data retention policies
   - Regular data backups

3. **Access Control**
   - Implement proper access controls
   - Use attribute-based access control
   - Implement data access auditing
   - Regular access reviews

### Network Security

1. **Network Protection**
   - Use firewalls
   - Implement intrusion detection
   - Use VPN for remote access
   - Implement network segmentation

2. **Secure Communication**
   - Use TLS for all communications
   - Implement certificate management
   - Use secure protocols
   - Implement proper cipher suites

3. **Monitoring**
   - Implement network monitoring
   - Use intrusion detection
   - Implement log management
   - Regular security assessments

### Incident Response

1. **Incident Detection**
   - Implement security monitoring
   - Use intrusion detection
   - Implement log analysis
   - Regular security assessments

2. **Incident Response**
   - Develop incident response plan
   - Establish incident response team
   - Implement communication procedures
   - Regular incident response testing

3. **Recovery**
   - Implement backup procedures
   - Develop recovery procedures
   - Test recovery procedures
   - Document recovery procedures

## Incident Response

### Incident Response Plan

```elixir
# config/config.exs
config :riva_ash, RivaAsh.IncidentResponse,
  contact_info: %{
    security_team: "security@your-domain.com",
    incident_response_team: "irt@your-domain.com",
    legal_team: "legal@your-domain.com",
    public_relations: "pr@your-domain.com"
  },
  escalation_procedures: %{
    low_severity: "security_team",
    medium_severity: "incident_response_team",
    high_severity: "incident_response_team",
    critical_severity: "incident_response_team"
  },
  communication_channels: %{
    internal: ["email", "slack", "phone"],
    external: ["email", "press_release", "social_media"],
    regulatory: ["email", "phone", "portal"]
  }
```

### Security Incident Procedures

1. **Detection and Analysis**
   - Monitor security alerts
   - Analyze security events
   - Determine incident scope
   - Assess impact

2. **Containment**
   - Isolate affected systems
   - Preserve evidence
   - Implement temporary fixes
   - Notify stakeholders

3. **Eradication**
   - Remove root cause
   - Implement permanent fixes
   - Verify fixes
   - Monitor for recurrence

4. **Recovery**
   - Restore systems
   - Monitor systems
   - Verify functionality
   - Document lessons learned

5. **Post-Incident Activity**
   - Conduct post-mortem
   - Update security policies
   - Implement improvements
   - Train staff

### Security Training

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Security.Training,
  enabled: true,
  training_programs: [
    %{
      name: "Security Awareness",
      frequency: "quarterly",
      duration: 30, # minutes
      mandatory: true,
      audience: "all"
    },
    %{
      name: "Incident Response",
      frequency: "annually",
      duration: 120, # minutes
      mandatory: true,
      audience: "security_team"
    },
    %{
      name: "GDPR Compliance",
      frequency: "annually",
      duration: 90, # minutes
      mandatory: true,
      audience: "all"
    }
  ],
  phishing_simulation: true,
  phishing_frequency: "monthly",
  phishing_target_rate: 0.1 # 10% of users
```

This security configuration guide provides comprehensive guidance for securing the Riva Ash document management system. Follow these best practices to ensure the application meets security requirements and protects sensitive data.