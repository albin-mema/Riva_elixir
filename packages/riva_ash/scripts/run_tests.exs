#!/usr/bin/env elixir

# Test runner script that properly handles test output
# Usage: mix run scripts/run_tests.exs [test_files...]

IO.puts("🧪 RivaAsh Test Runner")
IO.puts("=" |> String.duplicate(50))

# Set up environment
System.put_env("MIX_ENV", "test")
skip_db = System.get_env("SKIP_DB") == "true"

IO.puts("Environment: #{System.get_env("MIX_ENV")}")
IO.puts("Skip Database: #{skip_db}")

# Choose appropriate test helper
test_helper = if skip_db do
  "test/unit_test_helper.exs"
else
  "test/test_helper.exs"
end

IO.puts("Using test helper: #{test_helper}")

# Load test helper
try do
  Code.require_file(test_helper)
  IO.puts("✅ Test helper loaded successfully")
rescue
  e ->
    IO.puts("❌ Error loading test helper: #{inspect(e)}")
    System.halt(1)
end

# Get test files
test_files = case System.argv() do
  [] -> 
    IO.puts("📁 Scanning for test files...")
    files = Path.wildcard("test/**/*_test.exs")
    IO.puts("Found #{length(files)} test files")
    files
  files -> 
    IO.puts("📁 Running specific test files: #{inspect(files)}")
    files
end

# Load test files
IO.puts("📚 Loading test files...")
loaded_count = 0
error_count = 0

Enum.each(test_files, fn file ->
  try do
    Code.require_file(file)
    loaded_count = loaded_count + 1
    IO.write(".")
  rescue
    e ->
      error_count = error_count + 1
      IO.puts("\n❌ Error loading #{file}: #{inspect(e)}")
  end
end)

IO.puts("\n✅ Loaded #{loaded_count} test files")
if error_count > 0 do
  IO.puts("⚠️  #{error_count} files failed to load")
end

# Run tests
IO.puts("\n🚀 Running tests...")
IO.puts("-" |> String.duplicate(50))

ExUnit.run()
