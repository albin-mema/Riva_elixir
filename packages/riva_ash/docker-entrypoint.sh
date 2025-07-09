#!/bin/bash
set -e

# Wait for PostgreSQL to be ready
echo "Waiting for PostgreSQL to be ready..."
until PGPASSWORD=$DB_PASSWORD psql -h "$DB_HOSTNAME" -U "$DB_USERNAME" -d "$DB_NAME" -c '\q' >/dev/null 2>&1; do
  echo "PostgreSQL is unavailable - sleeping"
  sleep 2
done

echo "PostgreSQL is up - continuing"

# Initialize the database
if [ "$1" = 'start' ]; then
  # Run migrations
  echo "Running migrations..."
  /app/bin/riva_ash eval "RivaAsh.Release.migrate"

  # Start the server
  echo "Starting Phoenix server..."
  exec /app/bin/riva_ash start
else
  # Run the given command
  exec "$@"
fi
