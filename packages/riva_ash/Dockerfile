FROM elixir:1.18-alpine AS builder

# Install build dependencies
RUN apk add --no-cache \
    build-base \
    git \
    postgresql-client \
    nodejs \
    npm \
    inotify-tools \
    bash \
    curl \
    openssl

# Set working directory
WORKDIR /app

# Install hex and rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# Set environment variables for builds
ENV MIX_ENV=prod \
    MIX_HOME=/app/mix \
    HEX_HOME=/app/hex \
    HOME=/app

# Copy mix files
COPY mix.exs mix.lock ./

# Install dependencies
RUN mix deps.get --only $MIX_ENV && \
    mix deps.compile

# Copy application code
COPY . .

# Compile the application
RUN mix compile

# Deploy assets if the task exists, otherwise skip
RUN mix assets.deploy || echo "No assets to deploy"

# Create release
RUN mix release

# Create a new build stage for the runtime image
FROM alpine:3.16 AS app

# Install runtime dependencies
RUN apk add --no-cache \
    bash \
    openssl \
    libgcc \
    libstdc++ \
    ncurses-libs \
    postgresql-client \
    inotify-tools

# Set working directory
WORKDIR /app

# Copy the release from the builder stage
COPY --from=builder /app/_build/prod/rel/riva_ash .

# Copy entrypoint script
COPY docker-entrypoint.sh /app/
RUN chmod +x /app/docker-entrypoint.sh

# Expose port
EXPOSE 4000

# Set environment
ENV MIX_ENV=prod \
    PORT=4000 \
    SHELL=/bin/bash

# Set entrypoint and command
ENTRYPOINT ["/app/docker-entrypoint.sh"]
CMD ["start"]
