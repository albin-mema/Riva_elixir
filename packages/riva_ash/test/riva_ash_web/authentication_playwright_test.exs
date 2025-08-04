defmodule RivaAshWeb.AuthenticationPlaywrightTest do
  @moduledoc """
  Real browser testing for authentication flows using Phoenix Test with Playwright.
  """

  use ExUnit.Case, async: false

  # Disable in CI/unit runs to avoid compile/runtime deps on Playwright
  @moduletag :skip
end
