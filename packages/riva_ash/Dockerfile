FROM elixir:1.19-alpine

# Install build dependencies
RUN apk add --no-cache \
    build-base \
    git \
    postgresql-client \
    nodejs \
    npm

# Set working directory
WORKDIR /app

# Install hex and rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# Copy mix files
COPY mix.exs mix.lock ./

# Install dependencies
RUN mix deps.get

# Copy application code
COPY . .

# Compile the application
RUN mix compile

# Expose port
EXPOSE 4000

# Default command
CMD ["mix", "phx.server"]
