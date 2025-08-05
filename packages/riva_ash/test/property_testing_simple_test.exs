defmodule PropertyTestingSimpleTest do
  @moduledoc """
  Simple property testing tests for the Riva Ash application.
  Tests basic property testing functionality and system components.
  """

  use ExUnit.Case

  @moduletag :property_simple

  @doc """
  Tests that the basic property testing system works correctly.
  """
  @spec test_basic_property_testing_system_works :: :ok
  test "basic property testing system works" do
    alias RivaAsh.PropertyTesting.StateMachine

    # Test that the state machine module loads
    assert StateMachine.initial_state() == :anonymous

    # Test basic state transitions
    assert {:ok, :authenticated} = StateMachine.next_state(:anonymous, :login)
    assert {:ok, :anonymous} = StateMachine.next_state(:authenticated, :logout)

    IO.puts("✅ Property testing system basic functionality works!")
  end

  @doc """
  Tests that route enumeration works correctly.
  """
  @spec test_route_enumeration_works :: :ok
  @tag :unit
  test "route enumeration works" do
    if Application.get_env(:riva_ash, :skip_database) == true do
      IO.puts("⏭️  Skipping route enumeration test in unit test mode")
    else
      alias RivaAsh.PropertyTesting.RouteEnumerator

      # Test that route enumeration works
      routes = RouteEnumerator.enumerate_routes()

      assert is_map(routes)
      assert Map.has_key?(routes, :public)

      public_routes = Map.get(routes, :public, [])
      assert is_list(public_routes)

      IO.puts("✅ Route enumeration works! Found #{length(public_routes)} public routes")
    end
  end

  @doc """
  Tests that the flow generator produces valid flows.
  """
  @spec test_flow_generator_produces_flows :: :ok
  @tag :unit
  test "flow generator produces flows" do
    if Application.get_env(:riva_ash, :skip_database) == true do
      IO.puts("⏭️  Skipping flow generator test in unit test mode")
    else
      alias RivaAsh.PropertyTesting.FlowGenerator

      # Test that flow generation works
      flow = FlowGenerator.user_flow_generator(max_steps: 3, min_steps: 2) |> Enum.take(1) |> hd()

      assert is_list(flow)
      assert length(flow) >= 2
      assert length(flow) <= 3

      # Each step should be a {action, data} tuple
      Enum.each(flow, fn step ->
        assert {action, data} = step
        assert is_atom(action)
        assert is_map(data)
      end)

      IO.puts(
        "✅ Flow generator works! Generated flow: #{inspect(Enum.map(flow, fn {action, _} -> action end))}"
      )
    end
  end
end
