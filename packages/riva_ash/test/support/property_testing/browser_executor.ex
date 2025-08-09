# PhoenixTest/Playwright executor is optional; provide a stub to keep compilation smooth.

defmodule RivaAsh.PropertyTesting.BrowserExecutor do
  @moduledoc """
  Stub executor used when real browser drivers are not wired. Replace with a
  PhoenixTest-backed executor when needed.
  """

  @type execution_result :: {:ok, map()} | {:error, term()}
  @type flow_step :: {atom(), map()}

  @spec execute_flow([flow_step()], keyword()) :: execution_result()
  def execute_flow(_flow, _opts \\ []) do
    {:error, :browser_executor_not_configured}
  end
end
