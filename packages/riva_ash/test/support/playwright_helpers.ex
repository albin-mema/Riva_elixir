defmodule RivaAshWeb.PlaywrightHelpers do
  @moduledoc """
  Helper functions for Playwright-based testing in Riva Ash.
  
  This module provides utilities for:
  - Enhanced session management
  - Advanced interaction patterns
  - Performance monitoring
  - Error handling and debugging
  - Test data generation
  """

  alias PhoenixTest.Session
  require Logger

  @screenshot_dir "tmp/playwright_screenshots"
  @default_timeout 30_000
  @slow_page_threshold 5_000

  # --- Enhanced Session Management ---

  @doc """
  Create a new session with enhanced logging and monitoring.
  """
  def create_enhanced_session(conn, user \\ nil) do
    session = conn |> PhoenixTest.build_session()
    
    if user do
      session = log_in_user(session, user)
      Logger.info("🔐 Created authenticated session for user: #{user.email}")
    else
      Logger.info("🌐 Created public session")
    end
    
    session
  end

  @doc """
  Log in a user with enhanced error handling and validation.
  """
  def log_in_user(session, user) do
    start_time = System.monotonic_time(:millisecond)
    
    try do
      session
      |> visit("/sign-in")
      |> wait_for_selector("input[name='email']", timeout: @default_timeout)
      |> fill_in("Email address", with: to_string(user.email))
      |> fill_in("Password", with: user.password || "password123")
      |> click_button("Sign In")
      |> wait_for_navigation(timeout: @default_timeout)
      
      login_time = System.monotonic_time(:millisecond) - start_time
      Logger.info("✅ User login successful in #{login_time}ms")
      
      # Verify login was successful
      session = verify_login_success(session)
      
      session
    rescue
      error ->
        login_time = System.monotonic_time(:millisecond) - start_time
        Logger.error("❌ User login failed after #{login_time}ms: #{Exception.message(error)}")
        take_error_screenshot(session, "login_failure")
        raise error
    end
  end

  defp verify_login_success(session) do
    # Check for indicators of successful login
    cond do
      # Check for dashboard redirect
      session.current_path == "/dashboard" ->
        session
      # Check for welcome message
      has_text?(session, "Welcome") or has_text?(session, "Dashboard") ->
        session
      # Check for user menu/profile indicator
      has_selector?(session, "[data-testid='user-menu']") or
      has_selector?(session, ".user-avatar") or
      has_selector?(session, "[data-testid='user-profile']") ->
        session
      true ->
        Logger.warning("⚠️  Login success verification unclear - current path: #{session.current_path}")
        session
    end
  end

  @doc """
  Safely navigate to a URL with enhanced error handling and monitoring.
  """
  def safe_navigate_to(session, url, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    wait_for_load = Keyword.get(opts, :wait_for_load, true)
    
    start_time = System.monotonic_time(:millisecond)
    
    try do
      Logger.info("🌐 Navigating to: #{url}")
      
      result = session
        |> Session.navigate_to(url)
        |> maybe_wait_for_load(wait_for_load, timeout)
      
      load_time = System.monotonic_time(:millisecond) - start_time
      
      Logger.info("✅ Navigation to #{url} completed in #{load_time}ms")
      
      # Check if page is slow
      if load_time > @slow_page_threshold do
        Logger.warning("⚠️  Slow page load: #{url} took #{load_time}ms")
        take_screenshot(session, "#{url}_slow_load")
      end
      
      result
    rescue
      error ->
        load_time = System.monotonic_time(:millisecond) - start_time
        Logger.error("❌ Navigation to #{url} failed after #{load_time}ms: #{Exception.message(error)}")
        take_error_screenshot(session, "navigation_failure_#{url}")
        raise error
    end
  end

  defp maybe_wait_for_load(session, true, timeout) do
    Session.wait_for_load_state(session, "networkidle", timeout: timeout)
  end

  defp maybe_wait_for_load(session, false, _timeout) do
    session
  end

  # --- Advanced Interaction Patterns ---

  @doc """
  Fill a form with test data, handling various input types.
  """
  def fill_form_with_test_data(session, form_data) do
    Enum.reduce(form_data, session, fn {field, value}, acc_session ->
      fill_form_field(acc_session, field, value)
    end)
  end

  defp fill_form_field(session, field, value) when is_binary(value) do
    # Handle text inputs
    case find_field_selector(session, field) do
      {:ok, selector} ->
        session
        |> wait_for_selector(selector, timeout: @default_timeout)
        |> fill_in(field, with: value)
      {:error, reason} ->
        Logger.warning("⚠️  Could not find field '#{field}': #{reason}")
        session
    end
  end

  defp fill_form_field(session, field, value) when is_boolean(value) do
    # Handle checkboxes
    case find_field_selector(session, field) do
      {:ok, selector} ->
        if value do
          # Check if not already checked
          unless has_checked_selector?(session, selector) do
            session |> click(selector)
          end
        else
          # Uncheck if checked
          if has_checked_selector?(session, selector) do
            session |> click(selector)
          end
        end
      {:error, reason} ->
        Logger.warning("⚠️  Could not find checkbox '#{field}': #{reason}")
        session
    end
  end

  defp fill_form_field(session, field, value) when is_list(value) do
    # Handle multi-select or multiple checkboxes
    case find_field_selector(session, field) do
      {:ok, selector} ->
        # Clear existing selections
        session
        |> Session.click("#{selector} [data-clear]")
        |> Enum.reduce(session, fn item, acc_session ->
          item_selector = "#{selector} [data-value='#{item}']"
          if has_selector?(acc_session, item_selector) do
            acc_session |> click(item_selector)
          else
            acc_session
          end
        end)
      {:error, reason} ->
        Logger.warning("⚠️  Could not find multi-field '#{field}': #{reason}")
        session
    end
  end

  defp find_field_selector(session, field) do
    # Try multiple selector strategies
    selectors = [
      "[name='#{field}']",
      "[data-testid='#{field}']",
      "[aria-label='#{field}']",
      "##{field}",
      "input[placeholder*='#{String.downcase(field)}']",
      "label:contains('#{field}') + input",
      "##{String.replace(field, " ", "-")}"
    ]
    
    Enum.find_value(selectors, fn selector ->
      if has_selector?(session, selector) do
        {:ok, selector}
      else
        nil
      end
    end) || {:error, "No selector found for field '#{field}'"}
  end

  @doc """
  Click an element with enhanced error handling and retry logic.
  """
  def safe_click(session, selector, opts \\ []) do
    max_retries = Keyword.get(opts, :max_retries, 3)
    retry_delay = Keyword.get(opts, :retry_delay, 1000)
    
    try do
      session
      |> wait_for_selector(selector, timeout: @default_timeout)
      |> ensure_element_visible(selector)
      |> Session.click(selector)
    rescue
      error ->
        if max_retries > 0 do
          Logger.warning("⚠️  Click failed, retrying (#{max_retries} attempts left): #{selector}")
          :timer.sleep(retry_delay)
          safe_click(session, selector, max_retries: max_retries - 1, retry_delay: retry_delay)
        else
          Logger.error("❌ Click failed after #{max_retries} retries: #{selector}")
          take_error_screenshot(session, "click_failure_#{selector}")
          raise error
        end
    end
  end

  defp ensure_element_visible(session, selector) do
    if has_selector?(session, selector) do
      # Scroll element into view
      Session.evaluate(session, "document.querySelector('#{selector}').scrollIntoView({behavior: 'smooth'})")
      session
    else
      raise "Element not found: #{selector}"
    end
  end

  # --- Performance Monitoring ---

  @doc """
  Monitor page performance metrics.
  """
  def monitor_performance(session, url) do
    start_time = System.monotonic_time(:millisecond)
    
    session = safe_navigate_to(session, url)
    
    # Collect performance metrics
    metrics = collect_performance_metrics(session)
    
    total_time = System.monotonic_time(:millisecond) - start_time
    
    performance_report = %{
      url: url,
      total_load_time: total_time,
      dom_content_loaded: metrics.dom_content_loaded,
      first_contentful_paint: metrics.first_contentful_paint,
      largest_contentful_paint: metrics.largest_contentful_paint,
      first_input_delay: metrics.first_input_delay,
      cumulative_layout_shift: metrics.cumulative_layout_shift
    }
    
    Logger.info("📊 Performance metrics for #{url}: #{inspect(performance_report)}")
    
    # Check performance thresholds
    check_performance_thresholds(performance_report)
    
    performance_report
  end

  defp collect_performance_metrics(session) do
    # Use Performance API to get metrics
    metrics_script = """
    {
      domContentLoaded: performance.getEntriesByType('navigation')[0]?.domContentLoadedEventEnd - performance.getEntriesByType('navigation')[0]?.domContentLoadedEventStart,
      fcp: performance.getEntriesByName('first-contentful-paint')[0]?.startTime,
      lcp: performance.getEntriesByName('largest-contentful-paint')[0]?.startTime,
      fid: performance.getEntriesByName('first-input')[0]?.processingStart - performance.getEntriesByName('first-input')[0]?.startTime,
      cls: new PerformanceObserver((entryList) => {
        for (const entry of entryList.getEntries()) {
          if (entry.name === 'cumulative-layout-shift') {
            return entry.value;
          }
        }
      }).takeRecords()[0]?.value || 0
    }
    """
    
    try do
      Session.evaluate(session, metrics_script)
    rescue
      _ ->
        # Fallback to basic metrics
        %{
          dom_content_loaded: 0,
          first_contentful_paint: 0,
          largest_contentful_paint: 0,
          first_input_delay: 0,
          cumulative_layout_shift: 0
        }
    end
  end

  defp check_performance_thresholds(metrics) do
    thresholds = %{
      dom_content_loaded: 3000,  # 3s
      first_contentful_paint: 1800,  # 1.8s
      largest_contentful_paint: 2500,  # 2.5s
      first_input_delay: 100,  # 100ms
      cumulative_layout_shift: 0.1  # 0.1
    }
    
    Enum.each(thresholds, fn {metric, threshold} ->
      value = Map.get(metrics, metric)
      if value && value > threshold do
        Logger.warning("⚠️  #{metric} exceeds threshold: #{value} > #{threshold}")
      end
    end)
  end

  # --- Error Handling and Debugging ---

  @doc """
  Take a screenshot with error context.
  """
  def take_error_screenshot(session, error_type) do
    timestamp = System.system_time(:second)
    screenshot_path = "#{@screenshot_dir}/#{error_type}_#{timestamp}.png"
    
    try do
      Session.screenshot(session, screenshot_path)
      Logger.info("📸 Error screenshot saved: #{screenshot_path}")
      
      # Also capture page source for debugging
      source_path = "#{@screenshot_dir}/#{error_type}_#{timestamp}.html"
      Session.page_source(session) |> File.write!(source_path)
      Logger.info("📄 Page source saved: #{source_path}")
      
      screenshot_path
    rescue
      error ->
        Logger.error("❌ Failed to take error screenshot: #{Exception.message(error)}")
        nil
    end
  end

  @doc """
  Take a screenshot with custom naming.
  """
  def take_screenshot(session, name) do
    timestamp = System.system_time(:second)
    screenshot_path = "#{@screenshot_dir}/#{name}_#{timestamp}.png"
    
    try do
      Session.screenshot(session, screenshot_path)
      Logger.info("📸 Screenshot saved: #{screenshot_path}")
      screenshot_path
    rescue
      error ->
        Logger.error("❌ Failed to take screenshot: #{Exception.message(error)}")
        nil
    end
  end

  @doc """
  Check for common error patterns on the page.
  """
  def check_for_errors(session) do
    error_patterns = [
      ~s(text="Internal Server Error"),
      ~s(text="Server Error"),
      ~s(text="Exception"),
      ~s(text="500"),
      ~s(text="404"),
      ~s(text="403"),
      ~s(text="502"),
      ~s(text="503"),
      ~s(text="504"),
      ~s(text="Something went wrong"),
      ~s(text="An error occurred")
    ]
    
    errors_found = Enum.filter(error_patterns, &has_text?(session, &1))
    
    if errors_found != [] do
      Logger.error("❌ Found #{length(errors_found)} error patterns on page: #{inspect(errors_found)}")
      take_error_screenshot(session, "page_errors")
    end
    
    errors_found
  end

  @doc """
  Wait for a specific condition with timeout and retry logic.
  """
  def wait_for_condition(session, condition_fun, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    interval = Keyword.get(opts, :interval, 500)
    max_attempts = Keyword.get(opts, :max_attempts, div(timeout, interval))
    
    wait_for_condition_with_attempts(session, condition_fun, max_attempts, interval, timeout)
  end

  defp wait_for_condition_with_attempts(session, condition_fun, attempts, _interval, _timeout) when attempts <= 0 do
    if condition_fun.(session) do
      session
    else
      raise "Condition not met after timeout"
    end
  end

  defp wait_for_condition_with_attempts(session, condition_fun, attempts, interval, timeout) do
    if condition_fun.(session) do
      session
    else
      :timer.sleep(interval)
      wait_for_condition_with_attempts(session, condition_fun, attempts - 1, interval, timeout)
    end
  end

  # --- Test Data Generation ---

  @doc """
  Generate realistic test data for forms.
  """
  def generate_test_data do
    %{
      user: %{
        name: Faker.Name.name(),
        email: Faker.Internet.email(),
        phone: Faker.Phone.EnUs.phone(),
        company: Faker.Company.name(),
        address: Faker.Address.full_address()
      },
      item: %{
        name: Faker.Product.product_name(),
        description: Faker.Lorem.paragraph(),
        price: :rand.uniform(1000) + 10,
        category: Enum.random(["electronics", "furniture", "tools", "supplies"])
      },
      search: %{
        query: Faker.Lorem.words(3) |> Enum.join(" "),
        category: Enum.random(["all", "electronics", "furniture", "tools"]),
        location: Faker.Address.city(),
        date_range: %{
          start: Date.add(Date.utc_today(), 1),
          end: Date.add(Date.utc_today(), 7)
        }
      }
    }
  end

  @doc """
  Generate random test scenarios.
  """
  def generate_test_scenario do
    scenarios = [
      :user_registration,
      :user_login,
      :item_search,
      :item_booking,
      :payment_processing,
      :profile_update,
      :dashboard_view,
      :inventory_management
    ]
    
    Enum.random(scenarios)
  end

  # --- Utility Functions ---

  defp has_checked_selector?(session, selector) do
    try do
      Session.evaluate(session, "document.querySelector('#{selector}')?.checked || false")
    rescue
      _ -> false
    end
  end

  # Forward PhoenixTest session functions for convenience
  defdelegate has_selector?(session, selector), to: Session
  defdelegate has_text?(session, text), to: Session
  defdelegate wait_for_selector(session, selector, opts), to: Session
  defdelegate wait_for_navigation(session, opts), to: Session
  defdelegate fill_in(session, field, opts), to: Session
  defdelegate click(session, selector), to: Session
  defdelegate click_button(session, text), to: Session
  defdelegate current_path(session), to: Session
  defdelegate page_source(session), to: Session
  defdelegate visit(session, path), to: Session
end