# Browser Testing Setup Guide

## Overview

This guide shows you how to set up **real browser testing** for your authentication flows using Wallaby with Selenium WebDriver. The browser will actually open, click buttons, fill forms, and navigate pages just like a real user!

## Current Status

✅ **In-Memory Testing**: Working (no browser opens)
- Uses `Phoenix.LiveViewTest` and `PhoenixTest`
- Fast, reliable, CI/CD friendly
- Tests in `authentication_flow_test.exs`

✅ **Real Browser Testing**: WORKING! (opens actual browser)
- Uses `Wallaby` with Selenium WebDriver
- Opens actual Chrome browser and performs real clicks/typing
- Tests in `authentication_browser_test.exs`
- Screenshots saved to `tmp/wallaby_screenshots/`

## When to Use Each Approach

### Use In-Memory Testing (Current) When:
- ✅ Testing authentication logic and flows
- ✅ Testing form submissions and redirects
- ✅ Testing session management
- ✅ Running in CI/CD pipelines
- ✅ Fast feedback during development

### Use Real Browser Testing When:
- 🌐 Testing JavaScript interactions
- 🎨 Visual regression testing
- 📱 Responsive design testing
- ♿ Accessibility testing
- 🔄 Cross-browser compatibility
- 🎯 End-to-end user acceptance testing

## ✅ Real Browser Testing is NOW WORKING!

### Quick Start

```bash
# Run browser tests (headless)
./run_browser_tests.sh

# Run with VISIBLE browser (watch it work!)
./run_browser_tests.sh --visible

# Run only headless tests
./run_browser_tests.sh --headless-only
```

### What You'll See

When you run `./run_browser_tests.sh --visible`, you'll see:

1. **Chrome browser opens** 🌐
2. **Navigates to registration page** 📝
3. **Types in form fields** ⌨️
4. **Clicks submit button** 🖱️
5. **Navigates to login page** 🔐
6. **Fills login form** 📋
7. **Clicks login button** ✅
8. **Accesses protected pages** 🛡️
9. **Takes screenshots** 📸

### Setup Details (Already Done!)

✅ **Dependencies Added**: `wallaby` in `mix.exs`
✅ **ChromeDriver Installed**: Local chromedriver binary
✅ **Configuration**: Wallaby configured in `config/test.exs`
✅ **Test Cases**: Real browser tests in `authentication_browser_test.exs`
✅ **FeatureCase Updated**: Supports both in-memory and browser tests
✅ **Test Script**: `run_browser_tests.sh` for easy execution

## Test Examples

### Basic Browser Test
```elixir
test "user registration with real browser", %{conn: conn} do
  # This opens a REAL browser window
  conn
  |> visit("/register")
  |> fill_in("Name", with: "Test User")
  |> fill_in("Email", with: "test@example.com")
  |> fill_in("Password", with: "password123")
  |> click_button("Create Account")
  |> assert_path("/sign-in")
end
```

### Cross-Browser Testing
```elixir
use RivaAshWeb.FeatureCase,
  async: true,
  parameterize: [
    %{playwright: [browser: :chromium]},
    %{playwright: [browser: :firefox]},
    %{playwright: [browser: :webkit]}
  ]

test "works across all browsers", %{conn: conn} do
  # This test runs on Chrome, Firefox, and Safari
  conn
  |> visit("/sign-in")
  |> assert_has("h2", text: "Sign in to your account")
end
```

### Visual Testing
```elixir
test "visual regression testing", %{conn: conn} do
  conn
  |> visit("/sign-in")
  |> take_screenshot("sign_in_page.png")
  |> assert_has("input[name='email']")
end
```

## CI/CD Integration

### GitHub Actions Example

```yaml
name: Browser Tests
on: [push, pull_request]

jobs:
  browser-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '27.0'
          elixir-version: '1.19.0'
      
      - name: Install dependencies
        run: mix deps.get
      
      - name: Install Playwright
        run: npx playwright install --with-deps
      
      - name: Run browser tests
        run: mix test --include browser
        env:
          PW_HEADLESS: true
```

## Performance Considerations

### In-Memory Tests (Current)
- ⚡ **Speed**: ~50ms per test
- 💾 **Memory**: Low usage
- 🔄 **Parallelization**: High (async: true)

### Browser Tests (With Playwright)
- 🐌 **Speed**: ~2-5 seconds per test
- 💾 **Memory**: High usage (browser processes)
- 🔄 **Parallelization**: Limited (browser instances)

## Best Practices

### 1. Use Both Approaches
```elixir
# Fast feedback during development
mix test test/riva_ash_web/authentication_flow_test.exs

# Comprehensive testing before deployment
mix test test/riva_ash_web/authentication_browser_test.exs
```

### 2. Tag Your Tests
```elixir
@moduletag :browser  # For browser tests
@moduletag :fast     # For in-memory tests

# Run only fast tests during development
mix test --exclude browser

# Run all tests before deployment
mix test --include browser
```

### 3. Use Page Objects for Complex Flows
```elixir
defmodule AuthenticationPage do
  import PhoenixTest
  
  def register_user(session, user_data) do
    session
    |> visit("/register")
    |> fill_in("Name", with: user_data.name)
    |> fill_in("Email", with: user_data.email)
    |> fill_in("Password", with: user_data.password)
    |> click_button("Create Account")
  end
end
```

## Troubleshooting

### Common Issues

1. **Browsers not installed**: Run `npx playwright install`
2. **Tests timing out**: Increase timeout in config
3. **Flaky tests**: Add explicit waits with `assert_has/2`
4. **CI failures**: Ensure `--with-deps` flag for Playwright install

### Debug Mode

```bash
# Run with visible browser and debug info
PW_HEADLESS=false PW_DEBUG=true mix test test/riva_ash_web/authentication_browser_test.exs
```

## Summary

- ✅ **Current setup**: In-memory testing works great for authentication logic
- 🔧 **Optional upgrade**: Real browser testing for comprehensive coverage
- 🎯 **Recommendation**: Start with in-memory, add browser tests for specific needs
- 📈 **Best practice**: Use both approaches for different purposes
