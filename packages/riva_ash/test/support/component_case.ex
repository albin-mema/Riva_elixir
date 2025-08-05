defmodule RivaAshWeb.ComponentCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      import Phoenix.Component, only: [sigil_H: 2]
      import Phoenix.LiveViewTest

      @endpoint RivaAshWeb.Endpoint
    end
  end
end