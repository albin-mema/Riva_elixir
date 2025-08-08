alias RivaAsh.Test, as: Test

defmodule RivaAsh.Test.MoxHelpers do
  @moduledoc """
  Mox setup helpers.

  Usage examples (in a test case setup):

      import Mox
      import RivaAsh.Test.MoxHelpers

      setup :verify_on_exit!

      setup ctx do
        allow_default_mocks(self())
        :ok
      end

  Notes:
  - This helper assumes you define Mox mocks elsewhere with `Mox.defmock/2`.
  - It does not enable global mode; mocks are per-test process via `allow/3`.
  - Extend `default_behaviours/0` and `default_stubs/1` to register project behaviours.
  """

  import Mox

  @doc """
  Allow common mocks for the given `test_pid` and register default no-op stubs.

  Options:
    - `:stubs` - keyword list of `{behaviour, fun, impl}` to override defaults
  """
  @spec allow_default_mocks(pid(), keyword()) :: :ok
  def allow_default_mocks(test_pid, opts \\ []) when is_pid(test_pid) do
    stubs = Keyword.get(opts, :stubs, [])

    for {behaviour, mock} <- default_behaviours() do
      # allow calling process (and spawned processes) to use the mock
      allow(mock, test_pid, self())

      # register default stubs (no-ops) if behaviour known
      default_stubs({behaviour, mock})
    end

    # Apply caller-provided stubs last to override defaults
    Enum.each(stubs, fn
      {behaviour, fun, impl} when is_atom(behaviour) and is_atom(fun) and is_function(impl) ->
        mock = behaviour_to_mock(behaviour)
        expect_or_stub(mock, fun, impl)

      other ->
        raise ArgumentError, "Invalid stub entry: #{inspect(other)}"
    end)

    :ok
  end

  # Map behaviours to their Mox mock modules here.
  # Extend this list as new behaviours are introduced.
  @spec default_behaviours() :: [{module(), module()}]
  defp default_behaviours do
    # Example mapping when/if a Clock behaviour exists:
    # {RivaAsh.Clock, RivaAsh.ClockMock}
    mappings =
      [
        maybe_pair(RivaAsh.Clock, RivaAsh.ClockMock)
      ]
      |> Enum.filter(& &1)

    mappings
  end

  defp maybe_pair(behaviour, mock) do
    if Code.ensure_loaded?(behaviour) and Code.ensure_loaded?(mock) do
      {behaviour, mock}
    else
      nil
    end
  end

  # Register default stubs for a known behaviour
  defp default_stubs({RivaAsh.Clock, mock}) do
    # Provide minimal, deterministic clock behaviour
    if function_exported?(mock, :stub, 2) do
      # now/0
      stub(mock, :now, fn -> DateTime.utc_now() end)
      # freeze/1, travel/1, reset/0 if behaviour supports them
      maybe_stub(mock, :freeze, 1, fn _dt -> :ok end)
      maybe_stub(mock, :travel, 1, fn _sec -> :ok end)
      maybe_stub(mock, :reset, 0, fn -> :ok end)
    end

    :ok
  end

  defp default_stubs({_other_behaviour, _mock}), do: :ok

  defp behaviour_to_mock(RivaAsh.Clock), do: RivaAsh.ClockMock

  defp behaviour_to_mock(other),
    do: raise(ArgumentError, "Unknown behaviour mapping for #{inspect(other)}")

  defp expect_or_stub(mock, fun, impl) do
    # Prefer stub unless a specific expectation pattern is desired by caller
    if function_exported?(mock, :stub, 2) do
      stub(mock, fun, impl)
    else
      expect(mock, fun, impl)
    end
  end

  defp maybe_stub(mock, fun, arity, impl) do
    if function_exported?(mock, :stub, 2) and
         Code.ensure_loaded?(mock) and
         function_exported?(mock, fun, arity) do
      stub(mock, fun, impl)
    else
      :ok
    end
  end
end
