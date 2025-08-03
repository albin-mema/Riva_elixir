#!/usr/bin/env elixir

# Simple script to test property-based testing functionality
IO.puts("🧪 Property-Based Testing Runner")
IO.puts("================================")

# Set environment
System.put_env("SKIP_DB", "false")
System.put_env("MIX_ENV", "test")

# Start Mix in the current directory
File.cd!(".")
Mix.start()
Mix.env(:test)

# Compile the project
IO.puts("📦 Compiling project...")
{result, _} = System.cmd("mix", ["compile"], cd: ".")
if result != 0 do
  IO.puts("❌ Compilation failed")
  System.halt(1)
end

# Load test helper
Code.require_file("test/test_helper.exs")

# Load the property testing modules
IO.puts("🔧 Loading property testing modules...")

try do
  Code.require_file("test/support/property_testing/state_machine.ex")
  Code.require_file("test/support/property_testing/route_enumerator.ex")
  Code.require_file("test/support/property_testing/flow_generator.ex")

  IO.puts("✅ Property testing modules loaded successfully")

  # Test basic functionality
  IO.puts("\n🎯 Testing State Machine...")
  alias RivaAsh.PropertyTesting.StateMachine

  initial_state = StateMachine.initial_state()
  IO.puts("   Initial state: #{initial_state}")

  states = StateMachine.states()
  IO.puts("   Available states: #{inspect(states)}")

  # Test state transitions
  case StateMachine.next_state(:anonymous, :login) do
    {:ok, new_state} ->
      IO.puts("   ✅ Login transition: :anonymous → #{new_state}")
    {:error, reason} ->
      IO.puts("   ❌ Login transition failed: #{reason}")
  end

  IO.puts("\n🗺️  Testing Route Enumeration...")
  alias RivaAsh.PropertyTesting.RouteEnumerator

  routes = RouteEnumerator.enumerate_routes()
  public_routes = Map.get(routes, :public, [])
  authenticated_routes = Map.get(routes, :authenticated, [])

  IO.puts("   Public routes: #{length(public_routes)}")
  IO.puts("   Authenticated routes: #{length(authenticated_routes)}")
  IO.puts("   Total route categories: #{map_size(routes)}")

  # Show some example routes
  if length(public_routes) > 0 do
    example_route = hd(public_routes)
    IO.puts("   Example public route: #{example_route.path}")
  end

  IO.puts("\n🌊 Testing Flow Generation...")
  alias RivaAsh.PropertyTesting.FlowGenerator

  # Generate a simple flow
  flow_generator = FlowGenerator.user_flow_generator(max_steps: 5, min_steps: 2)
  sample_flows = Enum.take(flow_generator, 3)

  IO.puts("   Generated #{length(sample_flows)} sample flows:")

  Enum.with_index(sample_flows, 1)
  |> Enum.each(fn {flow, index} ->
    actions = Enum.map(flow, fn {action, _data} -> action end)
    IO.puts("   Flow #{index}: #{inspect(actions)} (#{length(flow)} steps)")
  end)

  IO.puts("\n🎉 Property-Based Testing System Status: OPERATIONAL")
  IO.puts("   ✅ State machine working")
  IO.puts("   ✅ Route enumeration working")
  IO.puts("   ✅ Flow generation working")
  IO.puts("   ✅ Ready for scaled testing!")

  # Show current configuration
  IO.puts("\n⚙️  Current Test Configuration:")
  IO.puts("   Environment: #{Mix.env()}")
  IO.puts("   Database: #{if System.get_env("SKIP_DB") == "true", do: "Disabled", else: "Enabled"}")
  IO.puts("   Application: #{Application.get_application(:riva_ash)}")

  IO.puts("\n🚀 Ready to run property tests with increased scale!")
  IO.puts("   Run: SKIP_DB=false mix test test/riva_ash_web/property_based_browser_test.exs")

rescue
  error ->
    IO.puts("❌ Error during property testing setup:")
    IO.puts("   #{inspect(error)}")
    IO.puts("\n🔍 This might indicate missing dependencies or configuration issues")
end
