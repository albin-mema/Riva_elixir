defmodule RivaAsh.PropertyTestingConfig do
  @moduledoc """
  Configuration for property-based testing parameters.
  
  This module centralizes all property testing configuration to make it easy
  to scale testing volume for different environments (development, CI, production).
  """

  @doc """
  Get property testing configuration based on environment.
  """
  def config(env \\ Mix.env()) do
    case env do
      :test -> development_config()
      :ci -> ci_config()
      :stress -> stress_config()
      _ -> development_config()
    end
  end

  @doc """
  Development configuration - moderate testing for fast feedback.
  """
  def development_config do
    %{
      # User flow testing
      user_flows: %{
        max_runs: 20,
        min_steps: 3,
        max_steps: 8,
        timeout: 120_000
      },
      
      # Navigation testing
      navigation: %{
        max_runs: 15,
        min_steps: 3,
        max_steps: 6,
        timeout: 60_000
      },
      
      # CRUD operations
      crud: %{
        max_runs: 8,
        resource_types: [:business, :client, :item],
        timeout: 90_000
      },
      
      # Error recovery
      error_recovery: %{
        max_runs: 5,
        timeout: 45_000
      },
      
      # Browser settings
      browser: %{
        headless: false,
        slow_mo: 1000,
        screenshots: true
      }
    }
  end

  @doc """
  CI configuration - balanced testing for automated pipelines.
  """
  def ci_config do
    %{
      user_flows: %{
        max_runs: 50,
        min_steps: 3,
        max_steps: 12,
        timeout: 180_000
      },
      
      navigation: %{
        max_runs: 30,
        min_steps: 3,
        max_steps: 8,
        timeout: 120_000
      },
      
      crud: %{
        max_runs: 20,
        resource_types: [:business, :client, :item, :employee, :reservation],
        timeout: 150_000
      },
      
      error_recovery: %{
        max_runs: 15,
        timeout: 90_000
      },
      
      browser: %{
        headless: true,
        slow_mo: 0,
        screenshots: false
      }
    }
  end

  @doc """
  Stress testing configuration - extensive testing for thorough validation.
  """
  def stress_config do
    %{
      user_flows: %{
        max_runs: 200,
        min_steps: 3,
        max_steps: 20,
        timeout: 600_000
      },
      
      navigation: %{
        max_runs: 100,
        min_steps: 5,
        max_steps: 15,
        timeout: 300_000
      },
      
      crud: %{
        max_runs: 50,
        resource_types: [:business, :client, :item, :employee, :reservation, :plot, :section],
        timeout: 400_000
      },
      
      error_recovery: %{
        max_runs: 30,
        timeout: 180_000
      },
      
      browser: %{
        headless: true,
        slow_mo: 0,
        screenshots: true
      }
    }
  end

  @doc """
  Get configuration for a specific test type.
  """
  def get(test_type, env \\ Mix.env()) do
    config(env)[test_type]
  end

  @doc """
  Get browser configuration.
  """
  def browser_config(env \\ Mix.env()) do
    config(env)[:browser]
  end

  @doc """
  Get timeout for a specific test type.
  """
  def timeout(test_type, env \\ Mix.env()) do
    get(test_type, env)[:timeout]
  end

  @doc """
  Get max_runs for a specific test type.
  """
  def max_runs(test_type, env \\ Mix.env()) do
    get(test_type, env)[:max_runs]
  end

  @doc """
  Environment-based configuration examples:
  
  ## Development (fast feedback)
  ```bash
  MIX_ENV=test mix test test/riva_ash_web/property_based_browser_test.exs
  ```
  
  ## CI Pipeline (balanced)
  ```bash
  MIX_ENV=ci mix test test/riva_ash_web/property_based_browser_test.exs
  ```
  
  ## Stress Testing (extensive)
  ```bash
  MIX_ENV=stress mix test test/riva_ash_web/property_based_browser_test.exs
  ```
  """
  def usage_examples, do: :ok
end
