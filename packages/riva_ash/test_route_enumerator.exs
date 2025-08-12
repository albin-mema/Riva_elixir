#!/usr/bin/env elixir

# Simple script to test RouteEnumerator functionality
# Note: This script should be run from the packages/riva_ash directory

# Load the application
Application.ensure_all_started(:riva_ash)

# Import the RouteEnumerator
alias RivaAsh.PropertyTesting.RouteEnumerator

# Test route enumeration
IO.puts("=== Testing RouteEnumerator ===")

# Get all routes
all_routes = RouteEnumerator.enumerate_routes()
IO.puts("Found routes: #{inspect(Map.keys(all_routes))}")

# Get parameterized routes
parameterized_routes = RouteEnumerator.parameterized_routes()
IO.puts("Parameterized routes count: #{length(parameterized_routes)}")

# Test parameter generation for a few routes
if length(parameterized_routes) > 0 do
  IO.puts("\n=== Testing Parameter Generation ===")
  
  # Take first 3 parameterized routes for testing
  test_routes = Enum.take(parameterized_routes, 3)
  
  Enum.with_index(test_routes, 1) |> Enum.each(fn {route, index} ->
    IO.puts("\nRoute #{index}: #{route.verb} #{route.path}")
    IO.puts("  Parameters: #{inspect(route.param_types)}")
    
    try do
      params = RouteEnumerator.generate_route_params(route)
      IO.puts("  Generated params: #{inspect(params)}")
    rescue
      error ->
        IO.puts("  Error generating params: #{inspect(error)}")
    end
  end)
else
  IO.puts("No parameterized routes found to test")
end

# Test getting existing IDs
IO.puts("\n=== Testing ID Generation ===")

ids_to_test = [:business, :client, :item, :user]

Enum.each(ids_to_test, fn resource_type ->
  try do
    id = RivaAsh.PropertyTesting.DataManager.get_random_id(resource_type)
    IO.puts("#{resource_type}: #{id}")
  rescue
    error ->
      IO.puts("#{resource_type}: Error - #{inspect(error)}")
  end
end)

IO.puts("\n=== Test Complete ===")