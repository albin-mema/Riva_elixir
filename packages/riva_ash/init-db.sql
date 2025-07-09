-- Initialize database with required extensions for Ash Framework

-- Create extensions required by AshPostgres
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "citext";

-- Note: ash-functions extension will be installed by AshPostgres migrations
-- This is a custom extension that comes with AshPostgres and is installed during migration
