#!/usr/bin/env elixir

# Simple test to check RouteEnumerator compilation and basic functionality
Mix.install([])

# Change to the package directory
File.cd!("/home/user/Riva_Ash/packages/riva_ash")

# Try to compile and check if RouteEnumerator exists
try do
  # Check if the module compiles
  Code.compile_file("test/support/property_testing/route_enumerator.ex")
  IO.puts("✓ RouteEnumerator module compiles successfully")
  
  # Try to load the module
  if Code.ensure_loaded?(RivaAsh.PropertyTesting.RouteEnumerator) do
    IO.puts("✓ RouteEnumerator module is loaded")
    
    # Check for expected functions
    functions = RivaAsh.PropertyTesting.RouteEnumerator.module_info(:functions)
    function_names = for {name, arity} <- functions, do: "#{name}/#{arity}"
    
    expected_functions = [
      "enumerate_routes/0",
      "parameterized_routes/0", 
      "generate_route_params/1",
      "get_valid_id_for_param/2"
    ]
    
    IO.puts("\nAvailable functions:")
    Enum.each(function_names, &IO.puts("  - #{&1}"))
    
    IO.puts("\nChecking for expected functions:")
    Enum.each(expected_functions, fn expected ->
      if expected in function_names do
        IO.puts("  ✓ #{expected}")
      else
        IO.puts("  ✗ #{expected} - NOT FOUND")
      end
    end)
    
    # Try to call a simple function
    try do
      routes = RivaAsh.PropertyTesting.RouteEnumerator.enumerate_routes()
      IO.puts("\n✓ enumerate_routes/0 returned: #{map_size(routes)} route categories")
    rescue
      error ->
        IO.puts("\n✗ enumerate_routes/0 failed: #{inspect(error)}")
    end
    
  else
    IO.puts("✗ RouteEnumerator module failed to load")
  end
  
rescue
  error ->
    IO.puts("✗ RouteEnumerator compilation failed: #{inspect(error)}")
end

IO.puts("\n=== Test Complete ===")