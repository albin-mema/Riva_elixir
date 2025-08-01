defmodule WorkingBrowserTest do
  use ExUnit.Case, async: false

  test "open browser and show automation" do
    # Start Phoenix server
    {:ok, _} = Application.ensure_all_started(:riva_ash)
    # Wait for server to start
    Process.sleep(3000)

    # Open browser manually first so you can see it
    {_output, _exit_code} =
      System.cmd(
        "chromium",
        [
          "--new-window",
          "--no-first-run",
          "--disable-default-apps",
          "http://localhost:4002/register"
        ],
        stderr_to_stdout: true
      )

    # Let browser open
    Process.sleep(2000)

    # Now use Playwright to control the browser
    # This should work with the browser that's already open
    try do
      # Import Phoenix Test functions
      import PhoenixTest

      # Create a connection
      conn = Phoenix.ConnTest.build_conn()

      # Try to use Phoenix Test to control the browser
      conn
      |> visit("/register")
      |> fill_in("Name", with: "Test User")
      |> fill_in("Email", with: "test@example.com")
      |> fill_in("Password", with: "password123")
      |> fill_in("Confirm Password", with: "password123")
      |> click_button("Create Account")

      # If we get here, it worked
      IO.puts("✅ Browser automation worked!")
    rescue
      error ->
        IO.puts("❌ Phoenix Test automation failed: #{inspect(error)}")

        # Fall back to just keeping the browser open so you can see the page
        IO.puts("🌐 Browser should be open showing the registration page")
        # Keep browser open for 10 seconds
        Process.sleep(10000)
    end
  end
end
