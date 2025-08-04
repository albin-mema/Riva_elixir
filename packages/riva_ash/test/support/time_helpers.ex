defmodule RivaAsh.Test.TimeHelpers do
  @moduledoc """
  Test time control helpers for deterministic tests.

  Usage examples:

      # Freeze time to a specific NaiveDateTime or DateTime
      import RivaAsh.Test.TimeHelpers
      clock_freeze(~N[2025-01-01 12:00:00])

      # Travel forward/backward in seconds from frozen or current time
      clock_travel(3600)   # +1 hour
      clock_travel(-120)   # -2 minutes

      # Run a block with frozen time and then automatically reset
      with_frozen_time(~U[2025-01-01 12:00:00Z], fn ->
        # perform assertions
      end)

      # Reset any frozen/travelled time back to real time
      clock_reset()

  Implementation notes:
  - If a Clock behaviour/module is already present in the application and supports
    freeze/travel/reset via behaviour functions, these functions delegate to it.
  - Otherwise, a simple application env-based adapter is used which is also compatible
    with Mox stubs in test.
  """

  @behaviour_clock_mod Application.compile_env(:riva_ash, :clock_module, nil)

  @doc """
  Freeze time to the provided NaiveDateTime or DateTime.
  """
  @spec clock_freeze(NaiveDateTime.t() | DateTime.t()) :: :ok
  def clock_freeze(%DateTime{} = dt) do
    do_clock_freeze(dt)
  end

  def clock_freeze(%NaiveDateTime{} = ndt) do
    {:ok, dt} = DateTime.from_naive(ndt, "Etc/UTC")
    do_clock_freeze(dt)
  end

  defp do_clock_freeze(dt) do
    cond do
      clock_behaviour_supported?() and function_exported?(@behaviour_clock_mod, :freeze, 1) ->
        apply(@behaviour_clock_mod, :freeze, [dt])

      true ->
        # env-backed adapter
        Application.put_env(:riva_ash, :test_clock, %{mode: :frozen, base: dt, offset_sec: 0})
        :ok
    end
  end

  @doc """
  Travel relative to the current frozen time (or now if not frozen) by the given seconds.
  """
  @spec clock_travel(integer()) :: :ok
  def clock_travel(seconds) when is_integer(seconds) do
    cond do
      clock_behaviour_supported?() and function_exported?(@behaviour_clock_mod, :travel, 1) ->
        apply(@behaviour_clock_mod, :travel, [seconds])

      true ->
        case Application.get_env(:riva_ash, :test_clock) do
          %{mode: :frozen, base: base, offset_sec: offset} ->
            Application.put_env(:riva_ash, :test_clock, %{
              mode: :frozen,
              base: base,
              offset_sec: offset + seconds
            })

          _ ->
            # Start from now and set frozen + offset
            now = DateTime.utc_now()
            Application.put_env(:riva_ash, :test_clock, %{mode: :frozen, base: now, offset_sec: seconds})
        end

        :ok
    end
  end

  @doc """
  Reset any time overrides to use real system time again.
  """
  @spec clock_reset() :: :ok
  def clock_reset do
    cond do
      clock_behaviour_supported?() and function_exported?(@behaviour_clock_mod, :reset, 0) ->
        apply(@behaviour_clock_mod, :reset, [])

      true ->
        Application.delete_env(:riva_ash, :test_clock)
        :ok
    end
  end

  @doc """
  Run the given `fun` with time frozen at `datetime`, then automatically reset.

  Accepts either a 0-arity function or a do/end block via anonymous function.

      with_frozen_time(~U[2025-01-01 12:00:00Z], fn ->
        # assertions with frozen time
      end)
  """
  @spec with_frozen_time(NaiveDateTime.t() | DateTime.t(), (() -> any())) :: any()
  def with_frozen_time(datetime, fun) when is_function(fun, 0) do
    prev = Application.get_env(:riva_ash, :test_clock)
    try do
      clock_freeze(datetime)
      fun.()
    after
      if prev do
        Application.put_env(:riva_ash, :test_clock, prev)
      else
        clock_reset()
      end
    end
  end

  # Internal: helper to obtain "now" honoring the test adapter.
  @spec now() :: DateTime.t()
  defp now do
    case Application.get_env(:riva_ash, :test_clock) do
      %{mode: :frozen, base: base, offset_sec: offset} ->
        DateTime.add(base, offset, :second)

      _ ->
        DateTime.utc_now()
    end
  end

  defp clock_behaviour_supported? do
    case @behaviour_clock_mod do
      nil -> false
      mod -> Code.ensure_loaded?(mod)
    end
  end
end