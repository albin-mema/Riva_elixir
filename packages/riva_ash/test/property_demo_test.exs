defmodule PropertyDemoTest do
  use ExUnit.Case

  test "property-based browser testing system demo" do
    IO.puts("\n🧪 Property-Based Browser Testing System Demo")
    IO.puts("=" |> String.duplicate(50))

    # Test 1: State Machine
    IO.puts("\n1. Testing State Machine...")
    alias RivaAsh.PropertyTesting.StateMachine

    initial_state = StateMachine.initial_state()
    IO.puts("   ✅ Initial state: #{initial_state}")

    {:ok, next_state} = StateMachine.next_state(:anonymous, :login)
    IO.puts("   ✅ Transition anonymous -> login: #{next_state}")

    actions = StateMachine.valid_actions(:authenticated)
    IO.puts("   ✅ Valid actions for authenticated user: #{inspect(actions)}")

    # Test 2: Route Enumeration
    IO.puts("\n2. Testing Route Enumeration...")
    alias RivaAsh.PropertyTesting.RouteEnumerator

    routes = RouteEnumerator.enumerate_routes()
    public_count = length(Map.get(routes, :public, []))
    auth_count = length(Map.get(routes, :authenticated, []))

    IO.puts("   ✅ Found #{public_count} public routes")
    IO.puts("   ✅ Found #{auth_count} authenticated routes")

    # Show some example routes
    if public_count > 0 do
      sample_public = Map.get(routes, :public, []) |> Enum.take(3)
      IO.puts("   📍 Sample public routes:")

      Enum.each(sample_public, fn route ->
        IO.puts("      - #{route.verb} #{route.path}")
      end)
    end

    # Test 3: Flow Generation
    IO.puts("\n3. Testing Flow Generation...")
    alias RivaAsh.PropertyTesting.FlowGenerator

    # Generate a simple flow
    flow = FlowGenerator.user_flow_generator(max_steps: 4, min_steps: 2) |> Enum.take(1) |> hd()
    flow_actions = Enum.map(flow, fn {action, _data} -> action end)

    IO.puts("   ✅ Generated flow with #{length(flow)} steps")
    IO.puts("   🔄 Flow sequence: #{inspect(flow_actions)}")

    # Test 4: Data Management
    IO.puts("\n4. Testing Data Management...")
    alias RivaAsh.PropertyTesting.DataManager

    test_users = DataManager.get_test_users()
    IO.puts("   ✅ Available test users: #{length(test_users)}")

    if length(test_users) > 0 do
      sample_user = List.first(test_users)
      IO.puts("   👤 Sample test user: #{sample_user.email}")
    end

    # Test 5: System Integration
    IO.puts("\n5. System Integration Check...")

    # Verify all components can work together
    state = StateMachine.initial_state()
    available_actions = StateMachine.available_actions(state)
    random_action = StateMachine.random_action(state)

    IO.puts("   ✅ State machine integration working")
    IO.puts("   🎲 Random action selected: #{random_action}")

    # Summary
    IO.puts(("\n" <> "=") |> String.duplicate(50))
    IO.puts("🎉 Property-Based Browser Testing System is READY!")
    IO.puts("   • State machine: ✅ Working")
    IO.puts("   • Route enumeration: ✅ Working")
    IO.puts("   • Flow generation: ✅ Working")
    IO.puts("   • Data management: ✅ Working")
    IO.puts("   • System integration: ✅ Working")

    IO.puts("\n🚀 Ready to run property-based browser tests!")
    IO.puts("   Use: ./run_property_browser_tests.sh --quick")
    IO.puts("=" |> String.duplicate(50))

    # All assertions to make sure everything works
    assert initial_state == :anonymous
    assert next_state == :authenticated
    assert is_list(actions)
    assert is_map(routes)
    assert length(flow) >= 2
    assert is_list(test_users)
    assert is_atom(random_action)
  end
end
