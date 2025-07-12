# Debug script for SQL Sandbox issues in Riva_Ash
# Run with: mix run debug_sandbox.exs

defmodule RivaAsh.SandboxDebug do
  @moduledoc """
  Utility module to debug SQL Sandbox configuration issues.
  This script checks various aspects of the SQL Sandbox setup
  and provides diagnostic information.
  """

  def run do
    IO.puts("===== SQL SANDBOX DIAGNOSTICS =====\n")

    # Check Elixir and Erlang versions
    IO.puts("System Information:")
    IO.puts("Elixir version: #{System.version()}")
    IO.puts("Erlang/OTP version: #{:erlang.system_info(:otp_release)}")
    IO.puts("Application environment: #{Mix.env()}")
    IO.puts("")

    # Check application status
    check_application_status()
    IO.puts("")

    # Check repository configuration
    check_repo_configuration()
    IO.puts("")

    # Check actual pool configuration
    check_pool_configuration()
    IO.puts("")

    # Check if sandbox works
    test_sandbox_operations()
    IO.puts("")

    # Check for Ash-specific issues
    check_ash_integration()
    IO.puts("")

    # Provide recommendations
    provide_recommendations()
  end

  defp check_application_status do
    IO.puts("Application Status:")
    
    apps = Application.started_applications()
    
    required_apps = [
      :ecto,
      :ecto_sql,
      :postgrex,
      :ash,
      :ash_postgres,
      :riva_ash
    ]
    
    for app <- required_apps do
      status = if Enum.any?(apps, fn {name, _, _} -> name == app end) do
        "STARTED"
      else
        "NOT STARTED"
      end
      
      IO.puts("  #{app}: #{status}")
    end
  end

  defp check_repo_configuration do
    IO.puts("Repository Configuration:")
    
    # Get configured options
    repo_config = Application.get_env(:riva_ash, RivaAsh.Repo)
    
    # Display relevant configuration
    IO.puts("  database: #{inspect(repo_config[:database])}")
    IO.puts("  pool: #{inspect(repo_config[:pool])}")
    IO.puts("  pool_size: #{inspect(repo_config[:pool_size])}")
    
    # Check if pool is correctly set to SQL Sandbox
    if repo_config[:pool] == Ecto.Adapters.SQL.Sandbox do
      IO.puts("  ✅ Pool is correctly set to Ecto.Adapters.SQL.Sandbox")
    else
      IO.puts("  ❌ Pool is NOT set to Ecto.Adapters.SQL.Sandbox")
      IO.puts("     Current pool: #{inspect(repo_config[:pool])}")
    end
    
    # Get repo supervisor children to check actual configuration
    IO.puts("\nRepo Module Information:")
    IO.puts("  Using: #{inspect(RivaAsh.Repo.__adapter__())}")
    
    # Check if AshPostgres.Repo might be overriding something
    IO.puts("  Parent modules: #{inspect(RivaAsh.Repo.__info__(:attributes)[:behaviour])}")
  end

  defp check_pool_configuration do
    IO.puts("Runtime Pool Configuration:")
    
    # Try to access repo configuration at runtime
    try do
      # This will show what pool is actually being used
      repo_config = RivaAsh.Repo.config()
      IO.puts("  Current pool: #{inspect(repo_config[:pool])}")
      
      # Check if there might be a mismatch
      app_config = Application.get_env(:riva_ash, RivaAsh.Repo)[:pool]
      if repo_config[:pool] != app_config do
        IO.puts("  ❌ Mismatch between Application config and runtime config")
        IO.puts("     Application config: #{inspect(app_config)}")
        IO.puts("     Runtime config: #{inspect(repo_config[:pool])}")
      else
        IO.puts("  ✅ Application and runtime configs match")
      end
    rescue
      e ->
        IO.puts("  ❌ Error accessing repo configuration: #{inspect(e)}")
    end
  end

  defp test_sandbox_operations do
    IO.puts("Testing Sandbox Operations:")
    
    try do
      # Try to put repo in sandbox mode
      result = Ecto.Adapters.SQL.Sandbox.mode(RivaAsh.Repo, :manual)
      IO.puts("  ✅ Set sandbox mode to :manual: #{inspect(result)}")
      
      # Try checkout operation
      pid = self()
      checkout_result = Ecto.Adapters.SQL.Sandbox.checkout(RivaAsh.Repo)
      IO.puts("  ✅ Checkout result: #{inspect(checkout_result)}")
      
      # Try a simple query to verify connection
      query_result = RivaAsh.Repo.query("SELECT 1")
      IO.puts("  ✅ Test query result: #{inspect(query_result)}")
      
      # Try checkin operation
      checkin_result = Ecto.Adapters.SQL.Sandbox.checkin(RivaAsh.Repo)
      IO.puts("  ✅ Checkin result: #{inspect(checkin_result)}")
    rescue
      e ->
        IO.puts("  ❌ Error in sandbox operations: #{inspect(e)}")
        IO.puts("     #{Exception.format_stacktrace()}")
    end
  end

  defp check_ash_integration do
    IO.puts("Ash Framework Integration:")
    
    # Check if Ash is potentially overriding anything
    try do
      # Check if there's any Ash-specific pool configuration
      ash_config = Application.get_env(:ash, :database)
      IO.puts("  Ash database config: #{inspect(ash_config)}")
      
      # Check if AshPostgres is potentially overriding anything
      ash_postgres_config = Application.get_env(:ash_postgres, :database)
      IO.puts("  AshPostgres database config: #{inspect(ash_postgres_config)}")
      
      # Check if there's a mismatch in the adapters
      IO.puts("  Repo adapter: #{inspect(RivaAsh.Repo.__adapter__())}")
      
      # Try to get the Ash resources
      resource_count = length(RivaAsh.Resource.__ash_resources__() || [])
      IO.puts("  Number of Ash resources: #{resource_count}")
    rescue
      e ->
        IO.puts("  ❌ Error checking Ash integration: #{inspect(e)}")
    end
  end

  defp provide_recommendations do
    IO.puts("Recommendations:")
    IO.puts("1. Ensure config/test.exs has `pool: Ecto.Adapters.SQL.Sandbox` for RivaAsh.Repo")
    IO.puts("2. Check if AshPostgres.Repo is overriding the pool configuration")
    IO.puts("3. Verify test_helper.exs correctly sets up the sandbox:")
    IO.puts("   - Application.put_env(:riva_ash, RivaAsh.Repo, pool: Ecto.Adapters.SQL.Sandbox)")
    IO.puts("   - Ecto.Adapters.SQL.Sandbox.mode(RivaAsh.Repo, :manual)")
    IO.puts("4. Make sure the database exists and is accessible")
    IO.puts("5. Check for any Ash-specific testing configuration that might be needed")
    IO.puts("6. Ensure the Repo is started before trying to use SQL Sandbox")
    IO.puts("7. Consider creating a custom sandbox setup function in test_helper.exs")
  end
end

# Run the diagnostics
RivaAsh.SandboxDebug.run()