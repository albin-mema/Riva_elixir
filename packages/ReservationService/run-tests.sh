#!/bin/bash

# Test runner script with configurable performance test sizes
# Usage: ./run-tests.sh [fast|medium|full] [additional dotnet test args...]

set -e

# Default to fast tests
PERF_SIZE=${1:-fast}
shift || true  # Remove first argument, ignore error if no args

case "$PERF_SIZE" in
    "fast"|"f")
        echo "🚀 Running FAST tests (minimal performance test iterations)"
        export PERF_TEST_SIZE=""
        export FS_MAX_TESTS=10
        export FS_QUIET=1
        ;;
    "medium"|"m")
        echo "⚡ Running MEDIUM tests (moderate performance test iterations)"
        export PERF_TEST_SIZE="medium"
        export FS_MAX_TESTS=50
        export FS_QUIET=1
        ;;
    "full"|"performance"|"p")
        echo "🔥 Running FULL performance tests (maximum iterations)"
        export PERF_TEST_SIZE="large"
        export FS_MAX_TESTS=100
        export FS_QUIET=0
        ;;
    *)
        echo "❌ Invalid test size: $PERF_SIZE"
        echo "Usage: $0 [fast|medium|full] [additional dotnet test args...]"
        echo ""
        echo "Test sizes:"
        echo "  fast     - Quick tests for development (default)"
        echo "  medium   - Moderate performance testing"
        echo "  full     - Full performance test suite"
        echo ""
        echo "Examples:"
        echo "  $0 fast"
        echo "  $0 medium --filter 'FullyQualifiedName~Restaurant'"
        echo "  $0 full --logger 'console;verbosity=detailed'"
        exit 1
        ;;
esac

echo "Environment variables:"
echo "  PERF_TEST_SIZE=$PERF_TEST_SIZE"
echo "  FS_MAX_TESTS=$FS_MAX_TESTS"
echo "  FS_QUIET=$FS_QUIET"
echo ""

# Run the tests with any additional arguments
echo "Running: dotnet test $@"
dotnet test "$@"

echo ""
echo "✅ Tests completed!"
