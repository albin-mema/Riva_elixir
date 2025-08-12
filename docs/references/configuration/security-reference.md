# Security & GDPR

This document provides comprehensive security configuration and GDPR compliance information for Riva Ash.

**For a condensed GDPR overview and implementation roadmap, see [GDPR Compliance](GDPR_COMPLIANCE.md).**

## Security Configuration

### Authentication
- **AshAuthentication**: Token-based with multiple strategies
- **Sessions**: Secure, signed cookies with rotation
- **Passwords**: bcrypt hashing with configurable rounds
- **Tokens**: JWT with expiration and refresh capability

#### Authentication Strategies
```elixir
# Password strategy with security settings
authentication do
  strategies do
    password :password do
      identity_field :email
      hashed_password_field :hashed_password
      hash_provider AshAuthentication.BcryptProvider
      confirmation_required? true
      password_confirmation_required? true

      resettable do
        sender MyApp.AuthEmail
        token_lifetime 24
      end
    end

    # API key strategy for programmatic access
    api_key :api_key do
      identity_field :api_key
      hashed_api_key_field :hashed_api_key
      api_key_generator fn ->
        "ak_" <> Base.url_encode64(:crypto.strong_rand_bytes(32), padding: false)
      end
    end

    # Magic link for passwordless auth
    magic_link :magic_link do
      identity_field :email
      sender MyApp.AuthEmail
      token_lifetime 10
    end
  end
end
```

### Authorization
- **Policies**: Declarative, resource-level access control
- **Actor Context**: User vs Employee with different permissions
- **Tenant Isolation**: Organization-level data boundaries
- **Role-Based**: Admin, manager, employee, viewer roles

### Data Protection
- **Encryption in Transit**: HTTPS/TLS 1.3 mandatory
- **Encryption at Rest**: Database-level encryption (PostgreSQL)
- **Session Security**: Secure, HttpOnly, SameSite cookies
- **API Security**: Bearer token authentication, rate limiting

### Audit & Monitoring
- **Complete Audit Trail**: AshPaperTrail tracks all changes
- **Failed Auth Logging**: Brute force detection
- **Access Monitoring**: Unusual activity alerts
- **System Metrics**: Performance and security monitoring

## GDPR Compliance

### Current Status
- ✅ **Audit trails** (AshPaperTrail)
- ✅ **Soft deletes** (AshArchival) 
- ✅ **Data minimization** (conditional collection)
- ✅ **Security** (encryption, access controls)
- ⚠️ **Consent management** (partial)
- 🔴 **Data subject rights** (needs automation)

### Data Processing Activities

#### User Data (Organization Owners)
- **Data**: Name, email, password hash, organization info
- **Purpose**: Account management, business operations
- **Legal Basis**: Contract performance (Article 6(1)(b))
- **Retention**: Account lifetime + 7 years

#### Employee Data (Staff)
- **Data**: Name, email, phone, role, organization association
- **Purpose**: Employment management, reservation processing
- **Legal Basis**: Legitimate interest (Article 6(1)(f))
- **Retention**: Employment period + 3 years

#### Client Data
- **Data**: Name, optional email/phone, reservation history
- **Purpose**: Reservation management, service delivery
- **Legal Basis**: Contract performance (Article 6(1)(b))
- **Retention**: Last interaction + 2 years

#### System Data
- **Data**: Audit logs, session tokens, metrics
- **Purpose**: Security, compliance, operations
- **Legal Basis**: Legitimate interest (Article 6(1)(f))
- **Retention**: Logs 1 year, sessions 30 days

### Data Controller Relationships
```
Platform → Data Processor for Organization Owners
Organization Owner → Data Controller for their organization
Employee → Data Processor + Data Subject
Client → Data Subject
```

### Technical Safeguards

#### Access Controls
```elixir
# Organization-level isolation
policy action_type([:read, :create, :update, :destroy]) do
  authorize_if(expr(organization.owner_id == ^actor(:id)))
end

# Role-based restrictions
policy action_type(:read) do
  authorize_if(actor_attribute_equals(:role, :employee))
end
```

#### Data Encryption
- **In Transit**: HTTPS/TLS 1.3
- **At Rest**: PostgreSQL encryption
- **Passwords**: bcrypt with salt
- **Sessions**: Signed and encrypted cookies

#### Audit Logging
- All modifications tracked with full diff
- User actions with timestamps and IP
- Failed authentication attempts
- System access monitoring

### Implementation Gaps & Actions

#### 1. Consent Management (Priority: High)
```elixir
# Add ConsentRecord resource
defmodule RivaAsh.Resources.ConsentRecord do
  attributes do
    uuid_primary_key(:id)
    attribute(:user_id, :uuid)
    attribute(:purpose, :string)  # "marketing", "analytics"
    attribute(:consent_given, :boolean)
    attribute(:consent_date, :utc_datetime)
    attribute(:consent_version, :string)
    attribute(:ip_address, :string)
    attribute(:user_agent, :string)
  end
end
```

#### 2. Data Subject Rights (Priority: High)
```elixir
# Add to User resource
actions do
  read :export_personal_data do
    description("Export all personal data in portable format")
  end

  update :request_data_deletion do
    description("Initiate GDPR deletion process")
  end

  update :rectify_personal_data do
    description("Allow data subjects to correct their data")
  end
end
```

#### 3. Retention Automation (Priority: Medium)
```elixir
# Background job for retention enforcement
defmodule RivaAsh.Jobs.GDPRRetentionJob do
  def perform do
    delete_expired_audit_logs()
    anonymize_inactive_clients()
    cleanup_expired_sessions()
  end
end
```

### Operational Cadence
- **Monthly**: Update data processing records, audit access logs, test backups, security patches
- **Quarterly**: Privacy impact assessments for new features, retention policy review, breach response drills, staff training
- **Annual**: Full GDPR compliance audit, privacy policy updates, third-party security assessments

### Security Best Practices

#### Development
- Never commit secrets to version control
- Use environment variables for configuration
- Validate all inputs at resource level
- Implement proper error handling (no data leaks)

#### Production
- Enable HTTPS everywhere
- Use secure session configuration
- Implement rate limiting
- Monitor for security events
- Regular security updates

#### Database
- Use connection pooling
- Enable query logging (without sensitive data)
- Regular backups with encryption
- Access control at database level

### Incident Response

#### Data Breach Procedure
1. **Detect**: Automated monitoring and alerts
2. **Assess**: Determine scope and impact
3. **Contain**: Stop ongoing breach
4. **Notify**: Authorities within 72 hours, subjects if high risk
5. **Investigate**: Root cause analysis
6. **Remediate**: Fix vulnerabilities
7. **Document**: Complete incident report

#### Contact Information
- **DPO**: [To be designated]
- **Security Team**: [To be designated]
- **Legal**: [To be designated]
- **Privacy Email**: privacy@rivaash.com

### Compliance Monitoring

#### Monthly
- Review access logs for anomalies
- Update data processing records
- Test backup and recovery
- Security patch updates

#### Quarterly
- Privacy impact assessments
- Policy reviews and updates
- Breach response drills
- Staff training updates

#### Annual
- Full GDPR compliance audit
- Privacy policy updates
- Third-party assessments
- Security architecture review

### Configuration Examples

#### Secure Session Config
```elixir
config :riva_ash, RivaAshWeb.Endpoint,
  secret_key_base: System.get_env("SECRET_KEY_BASE"),
  live_view: [signing_salt: System.get_env("SIGNING_SALT")],
  session: [
    store: :cookie,
    key: "_riva_ash_key",
    signing_salt: System.get_env("SIGNING_SALT"),
    same_site: "Lax",
    secure: true,
    http_only: true,
    max_age: 86400  # 24 hours
  ]
```

#### HTTPS Enforcement
```elixir
config :riva_ash, RivaAshWeb.Endpoint,
  force_ssl: [rewrite_on: [:x_forwarded_proto]],
  url: [scheme: "https", port: 443]
```
