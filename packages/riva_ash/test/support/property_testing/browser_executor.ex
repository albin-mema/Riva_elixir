# PhoenixTest not available; provide a stubbed executor for compilation.

alias RivaAsh.PropertyTesting, as: PropertyTesting

defmodule RivaAsh.PropertyTesting.BrowserExecutor do
  @moduledoc """
  Stub executor used when PhoenixTest is unavailable to allow tests to compile.
  """

  @type execution_result :: {:ok, map()} | {:error, term()}
  @type flow_step :: {atom(), map()}

  def execute_flow(_flow, _opts \\ []) do
    {:error, :phoenix_test_not_available}
  end
end
