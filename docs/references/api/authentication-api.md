# Authentication API Guide

This guide provides comprehensive instructions for using the authentication API in Riva Ash. It covers authentication flows, token management, OAuth integration, multi-factor authentication, and security best practices.

## Table of Contents

1. [Overview](#overview)
2. [Authentication Methods](#authentication-methods)
3. [JWT Authentication](#jwt-authentication)
4. [OAuth 2.0 Integration](#oauth-20-integration)
5. [Multi-Factor Authentication](#multi-factor-authentication)
6. [Token Management](#token-management)
7. [Security Best Practices](#security-best-practices)
8. [API Endpoints](#api-endpoints)
9. [Examples](#examples)
10. [Troubleshooting](#troubleshooting)

## Overview

The Riva Ash authentication system provides secure and flexible authentication methods for users and API clients. The system supports JWT-based authentication, OAuth 2.0 providers, multi-factor authentication, and comprehensive security features.

### Key Features

- **JWT-based authentication**: Stateless authentication with JSON Web Tokens
- **OAuth 2.0 support**: Integration with popular identity providers
- **Multi-factor authentication**: SMS, TOTP, and backup codes
- **Role-based access control**: Granular permission management
- **Session management**: Secure session handling and refresh tokens
- **Security monitoring**: Comprehensive audit logging and security alerts

### Authentication Flow

```mermaid
sequenceDiagram
    participant U as User
    participant C as Client
    participant AS as Auth Service
    participant DB as Database
    participant S as Session Store

    U->>C: Login Request
    C->>AS: Authenticate Credentials
    AS->>DB: Verify User
    DB-->>AS: User Data
    AS->>AS: Validate Credentials
    AS-->>C: JWT Tokens
    C->>S: Store Session
    C-->>U: Access Token
    U->>C: API Request with Token
    C->>AS: Validate Token
    AS-->>C: Token Valid
    C->>C: Process Request
```

## Authentication Methods

### 1. Username/Password Authentication

Traditional username and password authentication with secure password hashing.

### 2. JWT Authentication

Stateless authentication using JSON Web Tokens with configurable expiration.

### 3. OAuth 2.0

Third-party authentication using providers like Google, GitHub, Microsoft, etc.

### 4. API Key Authentication

Machine-to-machine authentication using API keys.

### 5. Multi-Factor Authentication

Additional security layer with SMS, TOTP, and backup codes.

## JWT Authentication

### JWT Structure

```json
{
  "header": {
    "alg": "HS256",
    "typ": "JWT"
  },
  "payload": {
    "sub": "550e8400-e29b-41d4-a716-446655440000",
    "email": "user@example.com",
    "name": "John Doe",
    "role": "user",
    "permissions": ["documents:read", "documents:create"],
    "iat": 1672531200,
    "exp": 1672534800,
    "jti": "550e8400-e29b-41d4-a716-446655440001"
  },
  "signature": "signature-here"
}
```

### JWT Configuration

```elixir
# config/config.exs
config :riva_ash, RivaAsh.Auth.Jwt,
  issuer: "riva_ash",
  audience: "riva_ash_users",
  secret_key_base: System.get_env("SECRET_KEY_BASE"),
  access_token_expiry: 3600, # 1 hour
  refresh_token_expiry: 86400, # 24 hours
  algorithm: "HS256",
  leeway: 30
```

### JWT Claims

| Claim | Description | Type |
|-------|-------------|------|
| `sub` | Subject (user ID) | String |
| `email` | User email | String |
| `name` | User name | String |
| `role` | User role | String |
| `permissions` | User permissions | Array |
| `iat` | Issued at timestamp | Integer |
| `exp` | Expiration timestamp | Integer |
| `jti` | JWT ID | String |
| `aud` | Audience | String |
| `iss` | Issuer | String |

### JWT Authentication Flow

#### 1. Login

```bash
curl -X POST \
  https://api.riva-ash.com/auth/login \
  -H "Content-Type: application/json" \
  -d '{
    "email": "user@example.com",
    "password": "password123",
    "remember_me": false
  }'
```

**Response**:
```json
{
  "data": {
    "access_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
    "refresh_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
    "expires_in": 3600,
    "token_type": "Bearer",
    "user": {
      "id": "550e8400-e29b-41d4-a716-446655440000",
      "email": "user@example.com",
      "name": "John Doe",
      "role": "user",
      "permissions": ["documents:read", "documents:create"]
    }
  }
}
```

#### 2. Access Protected Resource

```bash
curl -X GET \
  https://api.riva-ash.com/documents \
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
```

#### 3. Refresh Token

```bash
curl -X POST \
  https://api.riva-ash.com/auth/refresh \
  -H "Content-Type: application/json" \
  -d '{
    "refresh_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
  }'
```

**Response**:
```json
{
  "data": {
    "access_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
    "expires_in": 3600,
    "token_type": "Bearer"
  }
}
```

#### 4. Logout

```bash
curl -X POST \
  https://api.riva-ash.com/auth/logout \
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..." \
  -H "Content-Type: application/json" \
  -d '{
    "access_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
  }'
```

**Response**:
```json
{
  "data": {
    "message": "Successfully logged out"
  }
}
```

## OAuth 2.0 Integration

### Supported Providers

| Provider | Authorization Endpoint | Token Endpoint | User Info Endpoint |
|----------|----------------------|----------------|-------------------|
| Google | `https://accounts.google.com/o/oauth2/v2/auth` | `https://oauth2.googleapis.com/token` | `https://www.googleapis.com/oauth2/v2/userinfo` |
| GitHub | `https://github.com/login/oauth/authorize` | `https://github.com/login/oauth/access_token` | `https://api.github.com/user` |
| Microsoft | `https://login.microsoftonline.com/common/oauth2/v2.0/authorize` | `https://login.microsoftonline.com/common/oauth2/v2.0/token` | `https://graph.microsoft.com/v1.0/me` |
| Facebook | `https://www.facebook.com/v18.0/dialog/oauth` | `https://graph.facebook.com/v18.0/oauth/access_token` | `https://graph.facebook.com/v18.0/me` |

### OAuth Configuration

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
      scopes: ["email", "profile"],
      response_type: "code"
    },
    %{
      name: :github,
      strategy: Ueberauth.Strategy.GitHub.OAuth,
      uid_field: "id",
      client_id: System.get_env("GITHUB_CLIENT_ID"),
      client_secret: System.get_env("GITHUB_CLIENT_SECRET"),
      redirect_uri: "https://your-domain.com/auth/github/callback",
      scopes: ["user:email"],
      response_type: "code"
    },
    %{
      name: :microsoft,
      strategy: Ueberauth.Strategy.Microsoft.OAuth,
      uid_field: "id",
      client_id: System.get_env("MICROSOFT_CLIENT_ID"),
      client_secret: System.get_env("MICROSOFT_CLIENT_SECRET"),
      redirect_uri: "https://your-domain.com/auth/microsoft/callback",
      scopes: ["User.Read"],
      response_type: "code"
    }
  ]
```

### OAuth Authentication Flow

#### 1. Redirect to Provider

```javascript
// Redirect user to Google OAuth
window.location.href = `https://accounts.google.com/o/oauth2/v2/auth?client_id=YOUR_CLIENT_ID&redirect_uri=${encodeURIComponent(REDIRECT_URI)}&response_type=code&scope=email&state=${STATE}`;
```

#### 2. Handle Callback

```javascript
// Handle OAuth callback
async function handleOAuthCallback(code, state) {
  try {
    const response = await fetch('/auth/google/callback', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({ code, state })
    });

    const result = await response.json();

    if (result.access_token) {
      // Store tokens and redirect to app
      localStorage.setItem('accessToken', result.access_token);
      localStorage.setItem('refreshToken', result.refresh_token);
      window.location.href = '/dashboard';
    } else {
      // Handle error
      console.error('OAuth error:', result.error);
    }
  } catch (error) {
    console.error('OAuth callback error:', error);
  }
}
```

#### 3. Link OAuth Account

```bash
curl -X POST \
  https://api.riva-ash.com/auth/oauth/link \
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..." \
  -H "Content-Type: application/json" \
  -d '{
    "provider": "google",
    "access_token": "google-access-token"
  }'
```

**Response**:
```json
{
  "data": {
    "message": "OAuth account linked successfully"
  }
}
```

#### 4. Unlink OAuth Account

```bash
curl -X POST \
  https://api.riva-ash.com/auth/oauth/unlink \
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..." \
  -H "Content-Type: application/json" \
  -d '{
    "provider": "google"
  }'
```

**Response**:
```json
{
  "data": {
    "message": "OAuth account unlinked successfully"
  }
}
```

## Multi-Factor Authentication

### MFA Configuration

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

### MFA Flow

#### 1. Enable MFA

```bash
curl -X POST \
  https://api.riva-ash.com/auth/mfa/enable \
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..." \
  -H "Content-Type: application/json" \
  -d '{
    "method": "totp"
  }'
```

**Response**:
```json
{
  "data": {
    "secret": "JBSWY3DPEHPK3PXP",
    "qr_code": "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg==",
    "backup_codes": [
      "123456",
      "234567",
      "345678",
      "456789",
      "567890",
      "678901",
      "789012",
      "890123",
      "901234",
      "012345"
    ]
  }
}
```

#### 2. Verify MFA Setup

```bash
curl -X POST \
  https://api.riva-ash.com/auth/mfa/verify \
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..." \
  -H "Content-Type: application/json" \
  -d '{
    "code": "123456"
  }'
```

**Response**:
```json
{
  "data": {
    "message": "MFA enabled successfully"
  }
}
```

#### 3. Login with MFA

```bash
curl -X POST \
  https://api.riva-ash.com/auth/login \
  -H "Content-Type: application/json" \
  -d '{
    "email": "user@example.com",
    "password": "password123",
    "mfa_code": "123456"
  }'
```

**Response**:
```json
{
  "data": {
    "access_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
    "refresh_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
    "expires_in": 3600,
    "token_type": "Bearer",
    "user": {
      "id": "550e8400-e29b-41d4-a716-446655440000",
      "email": "user@example.com",
      "name": "John Doe",
      "role": "user",
      "mfa_enabled": true
    }
  }
}
```

#### 4. Generate Backup Codes

```bash
curl -X POST \
  https://api.riva-ash.com/auth/mfa/backup-codes \
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..." \
  -H "Content-Type: application/json"
```

**Response**:
```json
{
  "data": {
    "backup_codes": [
      "654321",
      "765432",
      "876543",
      "987654",
      "098765",
      "198765",
      "298765",
      "398765",
      "498765",
      "598765"
    ]
  }
}
```

#### 5. Disable MFA

```bash
curl -X POST \
  https://api.riva-ash.com/auth/mfa/disable \
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..." \
  -H "Content-Type: application/json" \
  -d '{
    "password": "current_password",
    "mfa_code": "123456"
  }'
```

**Response**:
```json
{
  "data": {
    "message": "MFA disabled successfully"
  }
}
```

## Token Management

### Token Storage

#### Client-Side Storage

```javascript
// Store tokens securely
const storeTokens = (accessToken, refreshToken) => {
  // Use httpOnly cookies for production
  document.cookie = `accessToken=${accessToken}; path=/; secure; HttpOnly; SameSite=Strict`;
  document.cookie = `refreshToken=${refreshToken}; path=/; secure; HttpOnly; SameSite=Strict`;
  
  // Or use localStorage for development
  if (process.env.NODE_ENV === 'development') {
    localStorage.setItem('accessToken', accessToken);
    localStorage.setItem('refreshToken', refreshToken);
  }
};

// Retrieve tokens
const getTokens = () => {
  const accessToken = getCookie('accessToken');
  const refreshToken = getCookie('refreshToken');
  
  return { accessToken, refreshToken };
};

// Helper function to get cookie
const getCookie = (name) => {
  const value = `; ${document.cookie}`;
  const parts = value.split(`; ${name}=`);
  if (parts.length === 2) return parts.pop().split(';').shift();
};
```

### Token Refresh Strategy

```javascript
class TokenManager {
  constructor() {
    this.refreshPromise = null;
  }

  async getValidToken() {
    const { accessToken, refreshToken } = getTokens();
    
    // Check if access token is expired
    if (this.isTokenExpired(accessToken)) {
      // Try to refresh the token
      return await this.refreshAccessToken(refreshToken);
    }
    
    return accessToken;
  }

  async refreshAccessToken(refreshToken) {
    // Prevent multiple concurrent refresh requests
    if (this.refreshPromise) {
      return await this.refreshPromise;
    }

    this.refreshPromise = fetch('/auth/refresh', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({ refresh_token: refreshToken })
    })
    .then(response => response.json())
    .then(result => {
      if (result.data) {
        // Update stored tokens
        storeTokens(result.data.access_token, result.data.refresh_token);
        return result.data.access_token;
      } else {
        // Refresh token is invalid, redirect to login
        throw new Error('Session expired');
      }
    })
    .catch(error => {
      console.error('Token refresh error:', error);
      // Clear tokens and redirect to login
      clearTokens();
      window.location.href = '/login';
      throw error;
    })
    .finally(() => {
      this.refreshPromise = null;
    });

    return await this.refreshPromise;
  }

  isTokenExpired(token) {
    if (!token) return true;
    
    try {
      const payload = JSON.parse(atob(token.split('.')[1]));
      return payload.exp < Date.now() / 1000;
    } catch (error) {
      return true;
    }
  }

  logout() {
    clearTokens();
    fetch('/auth/logout', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${getTokens().accessToken}`
      }
    });
    window.location.href = '/login';
  }
}

// Usage
const tokenManager = new TokenManager();

// Add to request interceptor
axios.interceptors.request.use(async (config) => {
  const token = await tokenManager.getValidToken();
  config.headers.Authorization = `Bearer ${token}`;
  return config;
});

// Add to response interceptor
axios.interceptors.response.use(
  response => response,
  async (error) => {
    const originalRequest = error.config;
    
    if (error.response?.status === 401 && !originalRequest._retry) {
      originalRequest._retry = true;
      try {
        const token = await tokenManager.refreshAccessToken();
        originalRequest.headers.Authorization = `Bearer ${token}`;
        return axios(originalRequest);
      } catch (refreshError) {
        return Promise.reject(refreshError);
      }
    }
    
    return Promise.reject(error);
  }
);
```

### Token Validation

```javascript
// Validate JWT token
const validateToken = (token) => {
  try {
    const payload = JSON.parse(atob(token.split('.')[1]));
    
    // Check expiration
    if (payload.exp < Date.now() / 1000) {
      return { valid: false, error: 'Token expired' };
    }
    
    // Check issuer
    if (payload.iss !== 'riva_ash') {
      return { valid: false, error: 'Invalid issuer' };
    }
    
    // Check audience
    if (payload.aud !== 'riva_ash_users') {
      return { valid: false, error: 'Invalid audience' };
    }
    
    return { valid: true, payload };
  } catch (error) {
    return { valid: false, error: 'Invalid token format' };
  }
};

// Check user permissions
const hasPermission = (token, permission) => {
  const validation = validateToken(token);
  if (!validation.valid) return false;
  
  const { permissions } = validation.payload;
  return permissions.includes(permission) || permissions.includes('*');
};
```

## Security Best Practices

### 1. Token Security

- Use HTTPS for all API communications
- Store tokens in httpOnly cookies in production
- Set appropriate token expiration times
- Implement token refresh mechanisms
- Use short-lived access tokens and long-lived refresh tokens
- Implement token revocation mechanisms

### 2. Password Security

- Use strong password policies
- Implement password hashing with Argon2 or bcrypt
- Require multi-factor authentication for sensitive operations
- Implement account lockout after failed attempts
- Use password strength meters
- Implement password expiration policies

### 3. OAuth Security

- Use PKCE for public clients
- Validate redirect URIs
- Implement state parameter for CSRF protection
- Use appropriate scopes
- Monitor for suspicious authentication patterns
- Implement rate limiting for OAuth endpoints

### 4. MFA Security

- Support multiple MFA methods
- Implement backup codes
- Use time-based one-time passwords (TOTP)
- Implement rate limiting for MFA verification
- Provide MFA recovery mechanisms
- Monitor MFA bypass attempts

### 5. Session Management

- Implement session timeout
- Use secure session cookies
- Implement session invalidation on logout
- Provide session management UI
- Monitor active sessions
- Implement session hijacking protection

## API Endpoints

### Authentication Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| `POST` | `/auth/login` | User login |
| `POST` | `/auth/logout` | User logout |
| `POST` | `/auth/refresh` | Refresh access token |
| `POST` | `/auth/register` | User registration |
| `POST` | `/auth/forgot-password` | Request password reset |
| `POST` | `/auth/reset-password` | Reset password |
| `POST` | `/auth/verify-email` | Verify email address |
| `GET` | `/auth/me` | Get current user |

### OAuth Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| `GET` | `/auth/:provider` | Redirect to OAuth provider |
| `GET` | `/auth/:provider/callback` | OAuth callback |
| `POST` | `/auth/oauth/link` | Link OAuth account |
| `POST` | `/auth/oauth/unlink` | Unlink OAuth account |
| `GET` | `/auth/oauth/providers` | Get available providers |

### MFA Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| `POST` | `/auth/mfa/enable` | Enable MFA |
| `POST` | `/auth/mfa/verify` | Verify MFA setup |
| `POST` | `/auth/mfa/disable` | Disable MFA |
| `POST` | `/auth/mfa/backup-codes` | Generate backup codes |
| `POST` | `/auth/mfa/verify-code` | Verify MFA code |
| `GET` | `/auth/mfa/status` | Get MFA status |

### User Management Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| `GET` | `/users` | List users |
| `POST` | `/users` | Create user |
| `GET` | `/users/:id` | Get user |
| `PUT` | `/users/:id` | Update user |
| `DELETE` | `/users/:id` | Delete user |
| `POST` | `/users/:id/roles` | Update user roles |

## Examples

### Complete Authentication Example

```javascript
class AuthService {
  constructor() {
    this.tokenManager = new TokenManager();
    this.api = axios.create({
      baseURL: 'https://api.riva-ash.com',
      timeout: 10000
    });
    
    this.setupInterceptors();
  }

  setupInterceptors() {
    // Request interceptor
    this.api.interceptors.request.use(
      async (config) => {
        const token = await this.tokenManager.getValidToken();
        config.headers.Authorization = `Bearer ${token}`;
        return config;
      },
      (error) => {
        return Promise.reject(error);
      }
    );

    // Response interceptor
    this.api.interceptors.response.use(
      (response) => response,
      async (error) => {
        const originalRequest = error.config;
        
        if (error.response?.status === 401 && !originalRequest._retry) {
          originalRequest._retry = true;
          try {
            const token = await this.tokenManager.refreshAccessToken();
            originalRequest.headers.Authorization = `Bearer ${token}`;
            return this.api(originalRequest);
          } catch (refreshError) {
            this.tokenManager.logout();
            return Promise.reject(refreshError);
          }
        }
        
        return Promise.reject(error);
      }
    );
  }

  async login(email, password, mfaCode = null) {
    try {
      const response = await this.api.post('/auth/login', {
        email,
        password,
        mfa_code: mfaCode
      });
      
      const { access_token, refresh_token, user } = response.data.data;
      
      // Store tokens
      this.tokenManager.storeTokens(access_token, refresh_token);
      
      return { success: true, user };
    } catch (error) {
      if (error.response?.status === 401) {
        if (error.response.data.error?.code === 'MFA_REQUIRED') {
          return { 
            success: false, 
            requiresMFA: true,
            mfaRequired: true 
          };
        }
        return { success: false, error: 'Invalid credentials' };
      }
      return { success: false, error: 'Login failed' };
    }
  }

  async loginWithMFA(mfaCode) {
    const { email, password } = this.getCredentials();
    return await this.login(email, password, mfaCode);
  }

  async logout() {
    try {
      await this.api.post('/auth/logout');
    } catch (error) {
      console.error('Logout error:', error);
    } finally {
      this.tokenManager.logout();
    }
  }

  async getCurrentUser() {
    try {
      const response = await this.api.get('/auth/me');
      return response.data.data;
    } catch (error) {
      if (error.response?.status === 401) {
        this.tokenManager.logout();
      }
      throw error;
    }
  }

  async enableMFA(method = 'totp') {
    try {
      const response = await this.api.post('/auth/mfa/enable', { method });
      return response.data.data;
    } catch (error) {
      throw error;
    }
  }

  async verifyMFA(code) {
    try {
      const response = await this.api.post('/auth/mfa/verify', { code });
      return response.data.data;
    } catch (error) {
      throw error;
    }
  }

  async disableMFA(password, mfaCode) {
    try {
      const response = await this.api.post('/auth/mfa/disable', {
        password,
        mfa_code: mfaCode
      });
      return response.data.data;
    } catch (error) {
      throw error;
    }
  }

  async linkOAuthAccount(provider, accessToken) {
    try {
      const response = await this.api.post('/auth/oauth/link', {
        provider,
        access_token: accessToken
      });
      return response.data.data;
    } catch (error) {
      throw error;
    }
  }

  async unlinkOAuthAccount(provider) {
    try {
      const response = await this.api.post('/auth/oauth/unlink', {
        provider
      });
      return response.data.data;
    } catch (error) {
      throw error;
    }
  }

  getCredentials() {
    // Get stored credentials (for demo purposes)
    // In production, use secure storage
    return {
      email: localStorage.getItem('email'),
      password: localStorage.getItem('password')
    };
  }
}

// Usage
const authService = new AuthService();

// Login flow
async function handleLogin(email, password) {
  try {
    const result = await authService.login(email, password);
    
    if (result.requiresMFA) {
      // Show MFA input
      showMFAInput();
    } else if (result.success) {
      // Redirect to dashboard
      window.location.href = '/dashboard';
    } else {
      // Show error
      showError(result.error);
    }
  } catch (error) {
    console.error('Login error:', error);
    showError('Login failed');
  }
}

// MFA flow
async function handleMFA(mfaCode) {
  try {
    const result = await authService.loginWithMFA(mfaCode);
    
    if (result.success) {
      window.location.href = '/dashboard';
    } else {
      showError('Invalid MFA code');
    }
  } catch (error) {
    console.error('MFA error:', error);
    showError('MFA verification failed');
  }
}

// Enable MFA flow
async function handleEnableMFA() {
  try {
    const mfaData = await authService.enableMFA('totp');
    
    // Show QR code and backup codes
    showMFASetup(mfaData.qr_code, mfaData.backup_codes);
  } catch (error) {
    console.error('MFA enable error:', error);
    showError('Failed to enable MFA');
  }
}
```

### React Hook Example

```jsx
import { useState, useEffect, useCallback } from 'react';
import { useNavigate } from 'react-router-dom';

export const useAuth = () => {
  const [user, setUser] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const navigate = useNavigate();

  const authService = new AuthService();

  const login = useCallback(async (email, password, mfaCode = null) => {
    try {
      setLoading(true);
      setError(null);
      
      const result = await authService.login(email, password, mfaCode);
      
      if (result.requiresMFA) {
        return { requiresMFA: true };
      }
      
      if (result.success) {
        setUser(result.user);
        navigate('/dashboard');
        return { success: true };
      }
      
      return { success: false, error: result.error };
    } catch (err) {
      setError(err.message);
      return { success: false, error: err.message };
    } finally {
      setLoading(false);
    }
  }, [navigate]);

  const logout = useCallback(async () => {
    try {
      await authService.logout();
      setUser(null);
      navigate('/login');
    } catch (err) {
      console.error('Logout error:', err);
    }
  }, [navigate]);

  const getCurrentUser = useCallback(async () => {
    try {
      setLoading(true);
      const userData = await authService.getCurrentUser();
      setUser(userData);
    } catch (err) {
      setError(err.message);
      navigate('/login');
    } finally {
      setLoading(false);
    }
  }, [navigate]);

  const enableMFA = useCallback(async (method = 'totp') => {
    try {
      setLoading(true);
      setError(null);
      
      const mfaData = await authService.enableMFA(method);
      return mfaData;
    } catch (err) {
      setError(err.message);
      throw err;
    } finally {
      setLoading(false);
    }
  }, []);

  const verifyMFA = useCallback(async (code) => {
    try {
      setLoading(true);
      setError(null);
      
      const result = await authService.verifyMFA(code);
      return result;
    } catch (err) {
      setError(err.message);
      throw err;
    } finally {
      setLoading(false);
    }
  }, []);

  const disableMFA = useCallback(async (password, mfaCode) => {
    try {
      setLoading(true);
      setError(null);
      
      const result = await authService.disableMFA(password, mfaCode);
      return result;
    } catch (err) {
      setError(err.message);
      throw err;
    } finally {
      setLoading(false);
    }
  }, []);

  // Check if user is authenticated on mount
  useEffect(() => {
    getCurrentUser();
  }, [getCurrentUser]);

  return {
    user,
    loading,
    error,
    login,
    logout,
    getCurrentUser,
    enableMFA,
    verifyMFA,
    disableMFA,
    isAuthenticated: !!user,
    hasPermission: (permission) => {
      if (!user) return false;
      return user.permissions?.includes(permission) || user.permissions?.includes('*');
    }
  };
};

// Usage in component
const LoginPage = () => {
  const { login, loading, error } = useAuth();
  const [email, setEmail] = useState('');
  const [password, setPassword] = useState('');
  const [mfaCode, setMfaCode] = useState('');
  const [requiresMFA, setRequiresMFA] = useState(false);

  const handleSubmit = async (e) => {
    e.preventDefault();
    
    if (requiresMFA) {
      const result = await login(email, password, mfaCode);
      if (!result.requiresMFA && !result.success) {
        alert(result.error);
      }
    } else {
      const result = await login(email, password);
      if (result.requiresMFA) {
        setRequiresMFA(true);
      } else if (!result.success) {
        alert(result.error);
      }
    }
  };

  return (
    <div className="min-h-screen flex items-center justify-center bg-gray-50">
      <div className="max-w-md w-full space-y-8">
        <div>
          <h2 className="mt-6 text-center text-3xl font-extrabold text-gray-900">
            {requiresMFA ? 'Two-Factor Authentication' : 'Sign in to your account'}
          </h2>
        </div>
        
        <form className="mt-8 space-y-6" onSubmit={handleSubmit}>
          {error && (
            <div className="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded">
              {error}
            </div>
          )}
          
          <div className="rounded-md shadow-sm -space-y-px">
            <div>
              <label htmlFor="email" className="sr-only">
                Email address
              </label>
              <input
                id="email"
                name="email"
                type="email"
                required
                className="appearance-none rounded-none relative block w-full px-3 py-2 border border-gray-300 placeholder-gray-500 text-gray-900 rounded-t-md focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 focus:z-10 sm:text-sm"
                placeholder="Email address"
                value={email}
                onChange={(e) => setEmail(e.target.value)}
              />
            </div>
            
            <div>
              <label htmlFor="password" className="sr-only">
                Password
              </label>
              <input
                id="password"
                name="password"
                type="password"
                required
                className="appearance-none rounded-none relative block w-full px-3 py-2 border border-gray-300 placeholder-gray-500 text-gray-900 rounded-b-md focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 focus:z-10 sm:text-sm"
                placeholder="Password"
                value={password}
                onChange={(e) => setPassword(e.target.value)}
              />
            </div>
            
            {requiresMFA && (
              <div>
                <label htmlFor="mfaCode" className="sr-only">
                  Two-Factor Code
                </label>
                <input
                  id="mfaCode"
                  name="mfaCode"
                  type="text"
                  required
                  className="appearance-none rounded-none relative block w-full px-3 py-2 border border-gray-300 placeholder-gray-500 text-gray-900 rounded-b-md focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 focus:z-10 sm:text-sm"
                  placeholder="Enter 6-digit code"
                  value={mfaCode}
                  onChange={(e) => setMfaCode(e.target.value)}
                  maxLength={6}
                />
              </div>
            )}
          </div>

          <div>
            <button
              type="submit"
              disabled={loading}
              className="group relative w-full flex justify-center py-2 px-4 border border-transparent text-sm font-medium rounded-md text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 disabled:opacity-50"
            >
              {loading ? 'Signing in...' : 'Sign in'}
            </button>
          </div>
        </form>
      </div>
    </div>
  );
};
```

## Troubleshooting

### Common Authentication Issues

#### 1. Token Expiration

**Symptom**: 401 Unauthorized errors

**Solution**: Implement automatic token refresh

```javascript
// Check token expiration before making requests
const checkTokenExpiry = () => {
  const token = getAccessToken();
  if (!token) return false;
  
  const decoded = jwtDecode(token);
  return decoded.exp * 1000 > Date.now() + 5 * 60 * 1000; // 5 minutes buffer
};

// Use in axios interceptor
axios.interceptors.request.use((config) => {
  if (!checkTokenExpiry()) {
    refreshToken();
  }
  return config;
});
```

#### 2. MFA Issues

**Symptom**: User cannot login despite correct credentials

**Solution**: Verify MFA setup and provide backup options

```javascript
// Handle MFA errors
const handleLoginError = (error) => {
  if (error.response?.status === 401) {
    if (error.response.data.error?.code === 'MFA_REQUIRED') {
      showMFAFlow();
    } else if (error.response.data.error?.code === 'INVALID_MFA_CODE') {
      showMFAError('Invalid code. Please try again.');
    } else if (error.response.data.error?.code === 'MFA_LOCKED') {
      showMFAError('Too many failed attempts. Please try again later.');
    }
  } else {
    showGenericError('Login failed. Please check your credentials.');
  }
};
```

#### 3. OAuth Callback Issues

**Symptom**: OAuth callback fails with invalid state or code

**Solution**: Validate state parameter and handle errors gracefully

```javascript
// Handle OAuth callback
const handleOAuthCallback = async () => {
  const urlParams = new URLSearchParams(window.location.search);
  const code = urlParams.get('code');
  const state = urlParams.get('state');
  const error = urlParams.get('error');

  if (error) {
    console.error('OAuth error:', error);
    showError('Authentication failed');
    return;
  }

  if (!code || !state) {
    console.error('Missing OAuth parameters');
    showError('Invalid authentication callback');
    return;
  }

  // Validate state parameter
  const storedState = localStorage.getItem('oauth_state');
  if (state !== storedState) {
    console.error('Invalid state parameter');
    showError('Security validation failed');
    return;
  }

  try {
    const result = await exchangeCodeForToken(code);
    // Handle successful authentication
  } catch (error) {
    console.error('Token exchange error:', error);
    showError('Authentication failed');
  }
};
```

#### 4. Session Issues

**Symptom**: User appears logged out unexpectedly

**Solution**: Implement proper session management

```javascript
// Check session status
const checkSessionStatus = async () => {
  try {
    const response = await fetch('/auth/me', {
      credentials: 'include'
    });
    
    if (response.ok) {
      const userData = await response.json();
      setUser(userData);
    } else {
      // Session is invalid
      clearSession();
    }
  } catch (error) {
    console.error('Session check error:', error);
    clearSession();
  }
};

// Clear session
const clearSession = () => {
  setUser(null);
  localStorage.removeItem('accessToken');
  localStorage.removeItem('refreshToken');
  document.cookie = 'accessToken=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/;';
  document.cookie = 'refreshToken=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/;';
};
```

#### 5. Permission Issues

**Symptom**: User gets 403 Forbidden errors

**Solution**: Check user permissions and provide appropriate feedback

```javascript
// Check permissions
const checkPermission = (permission) => {
  const user = getCurrentUser();
  if (!user) return false;
  
  return user.permissions?.includes(permission) || user.permissions?.includes('*');
};

// Protected component
const ProtectedComponent = ({ permission, children }) => {
  const { user } = useAuth();
  
  if (!user) {
    return <Navigate to="/login" />;
  }
  
  if (!checkPermission(permission)) {
    return <div>Access denied</div>;
  }
  
  return children;
};

// Usage
<ProtectedComponent permission="documents:delete">
  <DeleteButton />
</ProtectedComponent>
```

This authentication API guide provides comprehensive instructions for implementing secure authentication in Riva Ash. Follow these best practices to ensure robust security and a smooth user experience.