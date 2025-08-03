# Minimal test helper for debugging test output issues
IO.puts("Starting minimal test helper...")

ExUnit.start()

# Configure ExUnit with minimal settings
ExUnit.configure(
  formatters: [ExUnit.CLIFormatter],
  colors: [enabled: true],
  trace: false,
  capture_log: false
)

IO.puts("ExUnit configured and started")
IO.puts("Test configuration: #{inspect(ExUnit.configuration())}")
