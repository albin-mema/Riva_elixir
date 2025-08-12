defmodule RivaAshWeb.PlaywrightConfig do
  @moduledoc """
  Configuration management for Playwright-based testing across different environments.
  
  This module provides:
  - Environment-specific configurations
  - Test scenario definitions
  - Performance thresholds
  - Browser and viewport settings
  - Retry and timeout configurations
  """

  require Logger

  @default_browser :chromium
  @default_viewport %{width: 1280, height: 720}
  @default_timeout 30_000
  @default_retries 3

  # --- Environment Configuration ---

  @doc """
  Get environment-specific configuration.
  """
  def config do
    env = Application.get_env(:riva_ash, :env, :test)
    
    base_config = %{
      environment: env,
      browser: Application.get_env(:riva_ash, :playwright_browser, @default_browser),
      viewport: Application.get_env(:riva_ash, :playwright_viewport, @default_viewport),
      timeout: Application.get_env(:riva_ash, :playwright_timeout, @default_timeout),
      retries: Application.get_env(:riva_ash, :playwright_retries, @default_retries),
      headless: Application.get_env(:riva_ash, :playwright_headless, true),
      screenshots_enabled: Application.get_env(:riva_ash, :playwright_screenshots, true),
          trace_enabled: Application.get_env(:riva_ash, :playwright_trace, false),
      video_enabled: Application.get_env(:riva_ash, :playwright_video, false)
    }

    # Merge environment-specific overrides
    env_config = env_config(env)
    Map.merge(base_config, env_config)
  end

  defp env_config(:test) do
    %{
      headless: true,
      screenshots_enabled: true,
      trace_enabled: false,
      video_enabled: false,
      parallel: false,
      slow_mo: 0
    }
  end

  defp env_config(:dev) do
    %{
      headless: false,
      screenshots_enabled: true,
      trace_enabled: true,
      video_enabled: false,
      parallel: false,
      slow_mo: 100
    }
  end

  defp env_config(:ci) do
    %{
      headless: true,
      screenshots_enabled: true,
      trace_enabled: true,
      video_enabled: true,
      parallel: true,
      slow_mo: 0
    }
  end

  defp env_config(_other) do
    # Default to test config for unknown environments
    env_config(:test)
  end

  # --- Browser Configuration --

  @doc """
  Get browser-specific configuration.
  """
  def browser_config(browser \\ config().browser) do
    base_config = %{
      name: browser,
      headless: config().headless,
      slow_mo: config().slow_mo
    }

    browser_specific = case browser do
      :chromium ->
        %{
          args: [
            "--disable-gpu",
            "--disable-dev-shm-usage",
            "--disable-setuid-sandbox",
            "--no-sandbox",
            "--disable-web-security",
            "--disable-features=VizDisplayCompositor"
          ]
        }

      :firefox ->
        %{
          args: [
            "--headless",
            "--disable-gpu",
            "--disable-dev-shm-usage"
          ]
        }

      :webkit ->
        %{
          args: []
        }

      _other ->
        %{}
    end

    Map.merge(base_config, browser_specific)
  end

  # --- Viewport Configuration --

  @doc """
  Get viewport configuration for different device types.
  """
  def viewport_config(device \\ :desktop) do
    case device do
      :desktop ->
        %{width: 1920, height: 1080}
      
      :tablet ->
        %{width: 768, height: 1024}
      
      :mobile ->
        %{width: 375, height: 667}
      
      :responsive ->
        Enum.random([
          %{width: 320, height: 568},   # iPhone SE
          %{width: 375, height: 667},   # iPhone 8
          %{width: 414, height: 736},   # iPhone 8 Plus
          %{width: 768, height: 1024},  # iPad
          %{width: 1024, height: 768},  # iPad landscape
          %{width: 1366, height: 768},  # Small laptop
          %{width: 1920, height: 1080}  # Desktop
        ])
      
      _other ->
        config().viewport
    end
  end

  # --- Performance Thresholds --

  @doc """
  Get performance thresholds for different metrics.
  """
  def performance_thresholds do
    %{
      # Navigation timing thresholds (in milliseconds)
      navigation: %{
        dom_content_loaded: 3000,
        first_contentful_paint: 1800,
        largest_contentful_paint: 2500,
        first_byte: 200,
        dom_interactive: 4000,
        complete: 8000
      },

      # Resource loading thresholds
      resources: %{
        font_load: 2000,
        image_load: 3000,
        script_load: 2000,
        stylesheet_load: 1500
      },

      # Interaction thresholds
      interactions: %{
        first_input_delay: 100,
        cumulative_layout_shift: 0.1,
        time_to_interactive: 5000
      },

      # Custom thresholds for specific pages
      page_specific: %{
        "/": %{first_contentful_paint: 1500, largest_contentful_paint: 2000},
        "/dashboard": %{first_contentful_paint: 2000, largest_contentful_paint: 3000},
        "/search": %{first_contentful_paint: 1500, largest_contentful_paint: 2500},
        "/app/inventory": %{first_contentful_paint: 2000, largest_contentful_paint: 3500}
      }
    }
  end

  @doc """
  Check if a performance metric exceeds its threshold.
  """
  def exceeds_threshold?(metric_type, metric_name, value) do
    thresholds = performance_thresholds()
    threshold = get_in(thresholds, [metric_type, metric_name]) || 0
    
    if value > threshold do
      Logger.warning("⚠️  #{metric_type}.#{metric_name} exceeds threshold: #{value} > #{threshold}")
      true
    else
      false
    end
  end

  # --- Test Scenario Configuration --

  @doc """
  Get test scenario configurations.
  """
  def test_scenarios do
    %{
      # Smoke test scenarios - quick validation of critical paths
      smoke: [
        %{name: :home_page, path: "/", timeout: 10000, critical: true},
        %{name: :login_page, path: "/sign-in", timeout: 10000, critical: true},
        %{name: :dashboard, path: "/dashboard", timeout: 15000, critical: true},
        %{name: :search, path: "/search", timeout: 10000, critical: true}
      ],

      # Regression test scenarios - comprehensive page validation
      regression: [
        %{name: :public_pages, category: :public, timeout: 15000},
        %{name: :authenticated_pages, category: :authenticated, timeout: 15000},
        %{name: :admin_pages, category: :admin, timeout: 15000},
        %{name: :parameterized_pages, category: :parameterized, timeout: 20000}
      ],

      # User journey scenarios - end-to-end flows
      journeys: [
        %{name: :registration_flow, steps: ["/", "/register", "/sign-in", "/dashboard"], timeout: 30000},
        %{name: :booking_flow, steps: ["/search", "/app/inventory", "/app/reservations"], timeout: 30000},
        %{name: :payment_flow, steps: ["/app/finance", "/app/settings"], timeout: 25000},
        %{name: :admin_flow, steps: ["/admin", "/admin/users", "/admin/reports"], timeout: 25000}
      ],

      # Performance test scenarios
      performance: [
        %{name: :page_load_times, pages: ["/", "/dashboard", "/search"], iterations: 5},
        %{name: :interaction_responsiveness, interactions: [:click, :type, :navigate], pages: ["/dashboard"]},
        %{name: :resource_loading, resource_types: [:fonts, :images, :scripts]}
      ],

      # Cross-browser testing scenarios
      cross_browser: [
        %{name: :chrome_tests, browser: :chromium},
        %{name: :firefox_tests, browser: :firefox},
        %{name: :webkit_tests, browser: :webkit}
      ],

      # Device testing scenarios
      device_testing: [
        %{name: :desktop_tests, device: :desktop},
        %{name: :tablet_tests, device: :tablet},
        %{name: :mobile_tests, device: :mobile},
        %{name: :responsive_tests, device: :responsive}
      ]
    }
  end

  @doc """
  Get a specific test scenario configuration.
  """
  def get_scenario(scenario_type, scenario_name) do
    scenarios = test_scenarios()
    scenario_group = Map.get(scenarios, scenario_type, %{})
    
    Enum.find(scenario_group, &(&1.name == scenario_name))
  end

  # --- Timeout and Retry Configuration --

  @doc """
  Get timeout configuration for different operations.
  """
  def timeouts do
    %{
      # Navigation timeouts
      navigation: config().timeout,
      page_load: config().timeout,
      network_idle: 30_000,

      # Interaction timeouts
      element_wait: 10_000,
      click: 5_000,
      type: 5_000,
      select: 5_000,

      # Assertion timeouts
      assert_text: 10_000,
      assert_element: 10_000,
      assert_navigation: 15_000,

      # Specialized timeouts
      file_upload: 30_000,
      api_call: 10_000,
      database_operation: 5_000
    }
  end

  @doc """
  Get retry configuration for different operations.
  """
  def retry_config do
    %{
      # Retry counts for different failure types
      counts: %{
        network_error: config().retries,
        element_not_found: config().retries,
        timeout: max(1, div(config().retries, 2)),
        server_error: 1  # Don't retry server errors
      },

      # Retry delays in milliseconds
      delays: %{
        network_error: 2_000,
        element_not_found: 1_000,
        timeout: 1_000,
        server_error: 0
      },

      # Conditions for retrying
      conditions: %{
        retry_on_network_error: true,
        retry_on_timeout: true,
        retry_on_stale_element: true,
        retry_on_server_error: false
      }
    }
  end

  # --- Test Data Configuration --

  @doc """
  Get test data generation configuration.
  """
  def test_data_config do
    %{
      # Faker seed for reproducible test data
      seed: Application.get_env(:riva_ash, :test_data_seed, System.system_time(:second)),

      # Data generation strategies
      strategies: %{
        user: %{
          count: 10,
          fields: [:name, :email, :phone, :company, :address]
        },
        item: %{
          count: 20,
          fields: [:name, :description, :price, :category, :status]
        },
        search: %{
          count: 15,
          fields: [:query, :category, :location, :date_range]
        }
      },

      # Data constraints
      constraints: %{
        email: ~r/^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}$/,
        phone: ~r/^\+?[\d\s\-\(\)]+$/,
        price: :positive,
        name_length: 2..100
      }
    }
  end

  # --- Reporting Configuration --

  @doc """
  Get test reporting configuration.
  """
  def reporting_config do
    %{
      # Report formats
      formats: [:html, :json, :junit],

      # Report directories
      directories: %{
        screenshots: "test/playwright/screenshots",
        traces: "test/playwright/traces",
        videos: "test/playwright/videos",
        reports: "test/playwright/reports"
      },

      # Report settings
      settings: %{
        include_screenshots: true,
        include_traces: true,
        include_videos: true,
        include_performance_metrics: true,
        include_network_logs: false
      }
    }
  end

  # --- Helper Functions --

  @doc """
  Validate configuration.
  """
  def validate_config do
    config = config()
    
    validation_errors = []
    |> validate_browser(config.browser)
    |> validate_viewport(config.viewport)
    |> validate_timeout(config.timeout)
    |> validate_retries(config.retries)

    case validation_errors do
      [] ->
        Logger.info("✅ Playwright configuration is valid")
        :ok
      
      errors ->
        Logger.error("❌ Playwright configuration validation failed:")
        Enum.each(errors, &Logger.error("   - #{&1}"))
        :error
    end
  end

  defp validate_browser(errors, browser) do
    valid_browsers = [:chromium, :firefox, :webkit]
    
    if browser not in valid_browsers do
      ["Invalid browser: #{browser}. Must be one of #{inspect(valid_browsers)}" | errors]
    else
      errors
    end
  end

  defp validate_viewport(errors, %{width: width, height: height}) do
    cond do
      width <= 0 or height <= 0 ->
        ["Viewport dimensions must be positive" | errors]
      width > 3840 or height > 2160 ->
        ["Viewport dimensions too large: #{width}x#{height}" | errors]
      true ->
        errors
    end
  end

  defp validate_timeout(errors, timeout) when timeout <= 0 do
    ["Timeout must be positive" | errors]
  end

  defp validate_timeout(errors, timeout) when timeout > 300_000 do
    ["Timeout too large: #{timeout}ms" | errors]
  end

  defp validate_timeout(errors, _timeout), do: errors

  defp validate_retries(errors, retries) when retries < 0 do
    ["Retry count cannot be negative" | errors]
  end

  defp validate_retries(errors, retries) when retries > 10 do
    ["Retry count too large: #{retries}" | errors]
  end

  defp validate_retries(errors, _retries), do: errors

  @doc """
  Get configuration as a map for easy inspection.
  """
  def inspect_config do
    config = config()
    config = Map.put(config, :performance_thresholds, performance_thresholds())
    config = Map.put(config, :test_scenarios, test_scenarios())
    config = Map.put(config, :timeouts, timeouts())
    config = Map.put(config, :retry_config, retry_config())
    config = Map.put(config, :test_data_config, test_data_config())
    config = Map.put(config, :reporting_config, reporting_config())
    
    config
  end
end