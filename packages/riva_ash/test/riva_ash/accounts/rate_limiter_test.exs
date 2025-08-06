defmodule RivaAsh.Accounts.RateLimiterTest do
  use RivaAsh.DataCase, async: true

  alias RivaAsh.Accounts.RateLimiter

  setup do
    # Ensure GenServer is started for tests
    start_supervised!({RateLimiter, []})
    :ok
  end

  test "allows within limit" do
    ip = "192.0.2.#{System.unique_integer([:positive])}"

    # Default max is 5; first 5 checks should be allowed if we don't record attempts yet
    assert {:ok, :allowed} = RateLimiter.check_rate(ip)

    # Simulate attempts and ensure still allowed until hitting max
    for _ <- 1..4 do
      RateLimiter.record_attempt(ip)
      assert {:ok, :allowed} = RateLimiter.check_rate(ip)
    end
  end

  test "blocks on overflow after recording attempts" do
    ip = "198.51.100.#{System.unique_integer([:positive])}"

    # Hit the window with attempts equal to max
    for _ <- 1..5 do
      RateLimiter.record_attempt(ip)
      _ = RateLimiter.check_rate(ip)
    end

    # Now expect rate limited
    assert {:error, :rate_limited} = RateLimiter.check_rate(ip)
  end

  test "window resets with reset_rate/1 (simulates window reset)" do
    ip = "203.0.113.#{System.unique_integer([:positive])}"

    for _ <- 1..5 do
      RateLimiter.record_attempt(ip)
      _ = RateLimiter.check_rate(ip)
    end

    assert {:error, :rate_limited} = RateLimiter.check_rate(ip)

    # Reset the counter for current window
    RateLimiter.reset_rate(ip)

    # Should be allowed again
    assert {:ok, :allowed} = RateLimiter.check_rate(ip)
  end
end
