defmodule RivaAshWeb.ComponentCase do
  @moduledoc """
  Test case template for component testing.

  Provides common setup and imports for testing Phoenix components.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      import Phoenix.Component, only: [sigil_H: 2]
      import Phoenix.LiveViewTest

      @endpoint RivaAshWeb.Endpoint
    end
  end
end
