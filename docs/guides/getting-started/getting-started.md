# Getting Started Guide

This guide will walk you through setting up and using Riva Ash for the first time. Follow these steps to get the application running and understand the basic workflows.

**For detailed setup instructions and troubleshooting, see the [Setup Guide](SETUP_GUIDE.md).**
**For comprehensive development information, see the [Complete Guide](GUIDE.md).**

## Prerequisites

Before you begin, ensure you have the following installed:

- **Elixir** 1.18 or later
- **Phoenix** 1.7 or later
- **PostgreSQL** 14 or later
- **Node.js** 18 or later
- **Git** for version control

## Installation Steps

### 1. Clone the Repository

```bash
git clone https://github.com/your-org/riva_ash.git
cd Riva_Ash
```

### 2. Install Dependencies

Install Elixir dependencies:

```bash
mix deps.get
```

Install Node.js dependencies:

```bash
cd assets && npm install
cd ..
```

### 3. Database Setup

Configure your database connection in `config/dev.exs`:

```elixir
# config/dev.exs
config :riva_ash, RivaAsh.Repo,
  username: "your_username",
  password: "your_password",
  database: "riva_ash_dev",
  hostname: "localhost",
  show_sensitive_data_on_connection_error: true,
  pool_size: 10
```

Create and migrate the database:

```bash
mix ecto.create
mix ecto.migrate
```

### 4. Run the Application

Start the Phoenix server:

```bash
mix phx.server
```

The application will be available at `http://localhost:4000`.

## First-Time Setup

### 1. Create an Admin User

Open the application in your browser and navigate to the registration page. Create your first admin user account.

### 2. Configure Basic Settings

1. Log in with your admin account
2. Navigate to **Settings** > **System Configuration**
3. Configure the following essential settings:
   - Organization name
   - Default document retention period
   - Email notifications
   - File upload limits

### 3. Set Up Document Categories

1. Go to **Documents** > **Categories**
2. Create document categories relevant to your organization:
   - Financial Documents
   - HR Records
   - Legal Documents
   - Project Files
   - etc.

### 4. Configure User Roles

1. Navigate to **Users** > **Roles**
2. Set up role-based permissions:
   - Admin: Full system access
   - Manager: Document management and user oversight
   - User: Document creation and basic management
   - Viewer: Read-only access

## Basic Workflows

### Uploading Your First Document

1. Click **Upload Document** in the Documents section
2. Fill in the document details:
   - Title: Clear, descriptive name
   - Category: Select appropriate category
   - Description: Provide context and purpose
   - Tags: Add relevant keywords for search
3. Upload the file (PDF, DOC, DOCX, etc.)
4. Set retention and security settings
5. Click **Save**

### Document Version Control

1. Navigate to an existing document
2. Click **Upload New Version**
3. Upload the updated file
4. Add version notes explaining changes
5. Save the new version

### Setting Up Document Archival

1. Go to **Settings** > **Retention Policies**
2. Create archival rules based on:
   - Document type
   - Age of document
   - Business requirements
3. Configure automated archival schedules

### Working with the Audit Trail

1. Access the audit trail from **Reports** > **Audit Trail**
2. Filter by:
   - Date range
   - User actions
   - Document types
   - Specific users
3. Export audit reports as needed

## Troubleshooting Common Issues

### Database Connection Issues

If you encounter database connection errors:

```bash
# Check PostgreSQL is running
sudo systemctl status postgresql

# Reset database if needed
mix ecto.reset
```

### Asset Compilation Issues

If assets fail to compile:

```bash
# Clean and recompile assets
cd assets && npm run clean && npm run build
cd ..
mix phx.server
```

### Permission Issues

If users can't access certain features:

1. Verify user roles and permissions
2. Check category and document access settings
3. Review system configuration

## Next Steps

Once you have the basic setup complete, explore these resources:

- [User Guide](./USER_GUIDE.md) - Detailed feature documentation
- [Architecture Overview](./ARCHITECTURE_OVERVIEW.md) - System architecture
- [Complete API Reference](./API_REFERENCE.md) - Integration documentation
- [API Quick Reference](./API_REFERENCE_COMPACT.md) - Essential API information
- [Configuration Guide](./CONFIGURATION.md) - Advanced configuration options

## Support

If you encounter issues during setup:
1. Check the [troubleshooting guide](./USER_GUIDE.md#troubleshooting)
2. Review the [FAQ](./USER_GUIDE.md#faq)
3. Create an issue on [GitHub](https://github.com/your-org/riva_ash/issues)
4. Contact support@your-org.com

## Getting Help

Join our community:
- **Documentation**: Browse the full documentation set
- **GitHub Issues**: Report bugs and request features
- **Community Forum**: Connect with other users
- **Email Support**: For enterprise customers