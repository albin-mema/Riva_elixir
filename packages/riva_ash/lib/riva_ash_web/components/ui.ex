defmodule RivaAshWeb.Components.UI do
  @moduledoc """
  Main entry point for the UI component library.

  This module provides access to all UI components in the design system.
  This is the primary component system - use these components instead of atomic components.
  """

  # Core form components
  defdelegate button(assigns), to: RivaAshWeb.Components.UI.Button
  defdelegate input(assigns), to: RivaAshWeb.Components.UI.Input
  defdelegate checkbox(assigns), to: RivaAshWeb.Components.UI.Checkbox
  defdelegate select(assigns), to: RivaAshWeb.Components.UI.Select
  defdelegate textarea(assigns), to: RivaAshWeb.Components.UI.Textarea

  # Display components
  defdelegate badge(assigns), to: RivaAshWeb.Components.UI.Badge
  defdelegate alert(assigns), to: RivaAshWeb.Components.UI.Alert

  # Card components
  defdelegate card(assigns), to: RivaAshWeb.Components.UI.Card
  defdelegate card_header(assigns), to: RivaAshWeb.Components.UI.CardHeader
  defdelegate card_content(assigns), to: RivaAshWeb.Components.UI.CardContent
  defdelegate card_footer(assigns), to: RivaAshWeb.Components.UI.CardFooter
  defdelegate card_title(assigns), to: RivaAshWeb.Components.UI.CardTitle
  defdelegate card_description(assigns), to: RivaAshWeb.Components.UI.CardDescription

  # Convenience macro for importing all UI components
  defmacro __using__(_opts) do
    quote do
      import RivaAshWeb.Components.UI
    end
  end
end
