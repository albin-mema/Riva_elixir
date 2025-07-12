#!/bin/bash

# run-property-tests.sh
# Helper script to run property-based tests for Riva Ash

set -e

# Terminal colors
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}====================================${NC}"
echo -e "${BLUE}   RIVA ASH PROPERTY TEST RUNNER   ${NC}"
echo -e "${BLUE}====================================${NC}"

# Check if Docker is running
echo -e "${YELLOW}Checking Docker status...${NC}"
if ! ./docker-dev.sh status > /dev/null; then
  echo -e "${YELLOW}Starting Docker containers...${NC}"
  ./docker-dev.sh start
  
  # Wait for PostgreSQL to be ready
  echo -e "${YELLOW}Waiting for PostgreSQL to be ready...${NC}"
  for i in {1..30}; do
    if ./docker-dev.sh status > /dev/null; then
      echo -e "${GREEN}PostgreSQL is ready!${NC}"
      break
    fi
    echo -n "."
    sleep 1
  done
else
  echo -e "${GREEN}Docker is already running!${NC}"
fi

# Reset the test database
echo -e "${YELLOW}Setting up test database...${NC}"
cd packages/riva_ash
mix ecto.drop --quiet
mix ecto.create --quiet
mix ecto.migrate --quiet
cd ../..

# Run property-based tests
echo -e "${YELLOW}Running property-based tests...${NC}"
echo -e "${BLUE}------------------------------------${NC}"

pnpm run test:property

TEST_RESULT=$?

echo -e "${BLUE}------------------------------------${NC}"

# Report the result
if [ $TEST_RESULT -eq 0 ]; then
  echo -e "${GREEN}✓ Property tests passed successfully!${NC}"
else
  echo -e "${RED}✗ Property tests failed!${NC}"
fi

echo -e "${BLUE}====================================${NC}"

exit $TEST_RESULT