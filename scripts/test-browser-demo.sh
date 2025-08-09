#!/usr/bin/env bash
set -euo pipefail

# Demo browser tests with visible, slowed browser and optional tracing.
# Usage: scripts/test-browser-demo.sh [chromium|firefox|webkit]

BROWSER="${1:-chromium}"
export PLAYWRIGHT_BROWSER="$BROWSER"
export PLAYWRIGHT_HEADLESS=false
export PLAYWRIGHT_SLOW_MO_MS=${PLAYWRIGHT_SLOW_MO_MS:-350}
# Optionally enable tracing for demos by setting PLAYWRIGHT_TRACE=open or true
export PLAYWRIGHT_TRACE=${PLAYWRIGHT_TRACE:-open}

cd packages/riva_ash
# Run only the browser tests to avoid unrelated compilation errors
MIX_ENV=test mix test --include browser --trace \
  test/riva_ash_web/navigation_browser_smoke_test.exs \
  test/riva_ash_web/navigation_property_playwright_test.exs

