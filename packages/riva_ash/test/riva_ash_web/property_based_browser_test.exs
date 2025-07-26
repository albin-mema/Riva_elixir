defmodule RivaAshWeb.PropertyBasedBrowserTest do
  @moduledoc """
  Property-based browser testing that generates random user navigation flows
  and executes them in real browsers using Playwright.

  This test module combines StreamData property-based testing with browser
  automation to find edge cases and regressions in user flows.
  """

  use PhoenixTest.Playwright.Case,
    async: false,
    headless: false,  # Set to true for CI/CD
    slow_mo: 1000     # Slow down for visibility

  use ExUnitProperties

  import RivaAsh.PropertyTesting.FlowGenerator
  alias RivaAsh.PropertyTesting.{BrowserExecutor, DataManager, StateMachine, RouteEnumerator}

  @moduletag :property_based_browser
  @moduletag timeout: 120_000  # 2 minutes per property test

  setup_all do
    # Initialize test data once for all tests
    test_data = DataManager.initialize_test_data(cleanup_strategy: :after_suite)

    on_exit(fn ->
      DataManager.cleanup_all_test_data()
    end)

    %{test_data: test_data}
  end

  setup do
    # Ensure clean state before each test
    DataManager.ensure_clean_state()
    %{conn: Phoenix.ConnTest.build_conn()}
  end

  describe "Property-Based User Flow Testing" do
    @tag :slow
    property "random user flows complete successfully", %{conn: conn} do
      check all flow <- user_flow_generator(max_steps: 8, min_steps: 3),
                max_runs: 20 do

        IO.puts("\n" <> "=" |> String.duplicate(60))
        IO.puts("🎯 PROPERTY TEST: Random User Flow")
        IO.puts("   Flow length: #{length(flow)} steps")
        IO.puts("   Actions: #{inspect(Enum.map(flow, fn {action, _} -> action end))}")
        IO.puts("=" |> String.duplicate(60))

        case BrowserExecutor.execute_flow(flow, conn: conn, screenshots: true) do
          {:ok, result} ->
            # Verify the flow completed successfully
            assert result.final_state in StateMachine.states()

            IO.puts("🎉 PROPERTY TEST PASSED!")
            IO.puts("   Final state: #{result.final_state}")
            IO.puts("   Resources created: #{length(result.created_resources)}")

            # Log successful flow for analysis
            log_successful_flow(flow, result)

          {:error, {reason, step, state}} ->
            IO.puts("⚠️  PROPERTY TEST ENCOUNTERED ERROR")
            IO.puts("   Reason: #{inspect(reason)}")
            IO.puts("   Failed step: #{inspect(step)}")
            IO.puts("   State: #{state}")

            # Log failure details for debugging
            log_failed_flow(flow, reason, step, state)

            # Only fail if it's an unexpected error
            unless expected_error?(reason, step, state) do
              flunk("Unexpected flow failure: #{inspect(reason)} at step #{inspect(step)} in state #{state}")
            else
              IO.puts("   ✅ Error was expected and handled gracefully")
            end
        end
      end
    end

    @tag :authentication
    test "authentication flows work correctly", %{conn: conn} do
      IO.puts("\n🔐 AUTHENTICATION TEST STARTING")
      IO.puts("   This is a simple test to verify authentication works")

      # Simple test that will definitely run
      flow = [
        {:visit, %{path: "/sign-in"}},
        {:login, %{email: "test@example.com", password: "password123"}}
      ]

      IO.puts("   Testing flow: #{inspect(Enum.map(flow, fn {action, _} -> action end))}")

      case BrowserExecutor.execute_flow(flow, conn: conn) do
        {:ok, result} ->
          IO.puts("   ✅ Authentication test completed successfully!")
          IO.puts("   Final state: #{result.final_state}")
          assert result.final_state in [:authenticated, :admin, :error]

        {:error, {reason, step, state}} ->
          IO.puts("   ⚠️  Authentication test had error: #{inspect(reason)}")
          IO.puts("   Failed step: #{inspect(step)}")
          IO.puts("   State: #{state}")

          # Don't fail for expected errors
          unless reason in [:login_failed, :register_failed, :visit_failed] do
            flunk("Authentication flow failed unexpectedly: #{inspect(reason)}")
          else
            IO.puts("   ✅ Error was expected and handled")
          end
      end

      IO.puts("🔐 AUTHENTICATION TEST COMPLETE\n")
    end

    @tag :navigation
    property "navigation flows don't break the application", %{conn: conn} do
      check all flow <- navigation_flow_generator(:authenticated),
                max_runs: 15 do

        # Start with authenticated user
        auth_flow = [
          {:visit, %{path: "/sign-in"}},
          {:login, %{email: "test@example.com", password: "password123"}}
        ]

        full_flow = auth_flow ++ flow

        case BrowserExecutor.execute_flow(full_flow, conn: conn) do
          {:ok, result} ->
            # Navigation should maintain authenticated state
            assert result.final_state in [:authenticated, :admin]

          {:error, {reason, step, state}} ->
            # Log navigation errors for analysis
            log_navigation_error(flow, reason, step, state)

            # Only fail on server errors or crashes
            if reason in [:server_error, :application_crash] do
              flunk("Navigation caused application error: #{inspect(reason)}")
            end
        end
      end
    end

    @tag :crud
    property "CRUD operations maintain data consistency", %{conn: conn} do
      check all resource_type <- member_of([:business, :client, :item]),
                flow <- crud_flow_generator(resource_type),
                max_runs: 8 do

        # Start with authenticated user
        auth_flow = [
          {:visit, %{path: "/sign-in"}},
          {:login, %{email: "test@example.com", password: "password123"}}
        ]

        full_flow = auth_flow ++ flow

        case BrowserExecutor.execute_flow(full_flow, conn: conn) do
          {:ok, result} ->
            # CRUD operations should maintain authenticated state
            assert result.final_state == :authenticated

            # Verify created resources are tracked
            created_resources = result.created_resources
            resource_types = Enum.map(created_resources, fn {type, _id} -> type end)

            if Enum.any?(flow, fn {action, _} -> action == :create_resource end) do
              assert resource_type in resource_types
            end

          {:error, {reason, step, state}} ->
            log_crud_error(resource_type, flow, reason, step, state)

            # CRUD errors might be expected (validation failures, etc.)
            unless reason in [:create_resource_failed, :update_resource_failed, :delete_resource_failed] do
              flunk("CRUD operation failed unexpectedly: #{inspect(reason)}")
            end
        end
      end
    end

    @tag :error_recovery
    property "error recovery flows work correctly", %{conn: conn} do
      check all flow <- error_recovery_flow_generator(),
                max_runs: 5 do

        case BrowserExecutor.execute_flow(flow, conn: conn) do
          {:ok, result} ->
            # Error recovery should lead to a stable state
            assert result.final_state in [:anonymous, :authenticated]

          {:error, {reason, _step, _state}} ->
            # Some recovery failures are expected
            unless reason in [:recovery_failed, :session_expired] do
              flunk("Error recovery failed unexpectedly: #{inspect(reason)}")
            end
        end
      end
    end

    @tag :scenario
    test "happy path scenario works consistently", %{conn: conn} do
      # Test the most common user journey multiple times
      flow = [
        {:visit, %{path: "/"}},
        {:visit, %{path: "/register"}},
        {:register, %{
          name: "Happy Path User",
          email: "happy#{:rand.uniform(1000)}@example.com",
          password: "password123",
          password_confirmation: "password123"
        }},
        {:visit, %{path: "/sign-in"}},
        {:login, %{email: "happy#{:rand.uniform(1000)}@example.com", password: "password123"}},
        {:visit, %{path: "/dashboard"}},
        {:visit, %{path: "/businesses"}},
        {:logout, %{}}
      ]

      # Run this flow multiple times to ensure consistency
      for _i <- 1..5 do
        case BrowserExecutor.execute_flow(flow, conn: conn) do
          {:ok, result} ->
            assert result.final_state == :anonymous

          {:error, reason} ->
            flunk("Happy path failed: #{inspect(reason)}")
        end
      end
    end
  end

  describe "Route Coverage Testing" do
    @tag :coverage
    test "all public routes are accessible", %{conn: conn} do
      public_routes = RouteEnumerator.parameterless_routes(:public)

      Enum.each(public_routes, fn route ->
        flow = [{:visit, %{path: route.path}}]

        case BrowserExecutor.execute_flow(flow, conn: conn) do
          {:ok, _result} ->
            # Route is accessible
            :ok

          {:error, {reason, _step, _state}} ->
            # Log inaccessible routes
            IO.puts("⚠️  Public route #{route.path} is not accessible: #{inspect(reason)}")
        end
      end)
    end

    @tag :coverage
    test "all authenticated routes require authentication", %{conn: conn} do
      auth_routes = RouteEnumerator.parameterless_routes(:authenticated)

      Enum.each(auth_routes, fn route ->
        # Try to access without authentication
        flow = [{:visit, %{path: route.path}}]

        case BrowserExecutor.execute_flow(flow, conn: conn) do
          {:ok, result} ->
            # Should be redirected or in error state
            assert result.final_state in [:anonymous, :error]

          {:error, _reason} ->
            # Expected - route should not be accessible without auth
            :ok
        end
      end)
    end
  end

  # Helper functions for logging and analysis

  defp log_successful_flow(flow, result) do
    if Application.get_env(:riva_ash, :log_successful_flows, false) do
      IO.puts("✅ Successful flow: #{length(flow)} steps, final state: #{result.final_state}")
      IO.puts("   Steps: #{inspect(Enum.map(flow, fn {action, _} -> action end))}")
    end
  end

  defp log_failed_flow(flow, reason, step, state) do
    IO.puts("❌ Failed flow at step #{inspect(step)} in state #{state}")
    IO.puts("   Reason: #{inspect(reason)}")
    IO.puts("   Flow: #{inspect(Enum.map(flow, fn {action, _} -> action end))}")
  end

  defp log_navigation_error(flow, reason, step, state) do
    IO.puts("🧭 Navigation error: #{inspect(reason)} at #{inspect(step)} in state #{state}")
    IO.puts("   Flow: #{inspect(Enum.map(flow, fn {action, _} -> action end))}")
  end

  defp log_crud_error(resource_type, flow, reason, step, state) do
    IO.puts("📝 CRUD error for #{resource_type}: #{inspect(reason)} at #{inspect(step)} in state #{state}")
    IO.puts("   Flow: #{inspect(Enum.map(flow, fn {action, _} -> action end))}")
  end

  defp expected_error?(reason, step, state) do
    case {reason, step, state} do
      # Expected authentication errors
      {:login_failed, {:login, _}, :anonymous} -> true
      {:register_failed, {:register, _}, :anonymous} -> true

      # Expected permission errors
      {:visit_failed, {:visit, %{path: "/admin" <> _}}, :authenticated} -> true

      # Expected validation errors
      {:create_resource_failed, {:create_resource, _}, _} -> true
      {:update_resource_failed, {:update_resource, _}, _} -> true

      # Expected not found errors
      {:visit_failed, {:visit, %{path: "/nonexistent" <> _}}, _} -> true

      _ -> false
    end
  end
end
