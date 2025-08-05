defmodule RivaAshWeb.Components.UI.Button do
  @moduledoc """
  Implements a button component using the design system.

  A versatile button component that supports multiple variants, sizes, and states.
  Used for triggering actions in forms, dialogs, and general UI interactions.
  """
  use Phoenix.Component

  @doc """
  Renders a button component using the design system.

  Supports multiple variants, sizes, and states as defined in the design system.
  """
  attr :type, :string, default: "button", doc: "The button type (button, submit, reset)"
  attr :variant, :string, default: "default", values: ~w(default destructive outline secondary ghost link), doc: "The button style variant"
  attr :size, :string, default: "default", values: ~w(default sm lg), doc: "The button size"
  attr :loading, :boolean, default: false, doc: "Whether the button is in a loading state"
  attr :class, :string, default: "", doc: "Additional CSS classes to apply"
  attr :disabled, :boolean, default: false, doc: "Whether the button is disabled"
  attr :rest, :global, doc: "Any additional HTML attributes"

  slot :inner_block, required: true, doc: "The content to display inside the button"

  @spec button(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def button(assigns) do
    # Render button using functional composition
    assigns
    |> Map.put_new(:button_class, button_class(assigns))
    |> Map.put_new(:spinner_class, build_spinner_class(assigns.loading))
    |> Map.put_new(:content_class, build_content_class(assigns.loading))
    |> Map.put_new(:disabled_state, build_disabled_state(assigns.disabled, assigns.loading))
    |> render_button_component()
  end

  # Private helper for button rendering
  @spec render_button_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_button_component(assigns) do
    ~H"""
    <button
      type={@type}
      class={@button_class}
      disabled={@disabled_state}
      {@rest}
    >
      <%= if @loading do %>
        <!-- Loading spinner -->
        <svg class={@spinner_class} xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24">
          <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
          <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
        </svg>
      <% end %>
      <span class={@content_class}>
        <%= render_slot(@inner_block) %>
      </span>
    </button>
    """
  end

  # Helper function to build spinner classes
  @spec build_spinner_class(boolean()) :: String.t()
  defp build_spinner_class(loading) do
    if loading, do: "mr-2 h-4 w-4 animate-spin", else: "hidden"
  end

  # Helper function to build content classes
  @spec build_content_class(boolean()) :: String.t()
  defp build_content_class(loading) do
    if loading, do: "inline-block", else: ""
  end

  # Helper function to build disabled state
  @spec build_disabled_state(boolean(), boolean()) :: boolean()
  defp build_disabled_state(disabled, loading) do
    disabled or loading
  end

  defp button_class(assigns) do
    base = "inline-flex items-center justify-center rounded-md text-sm font-medium transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:opacity-50 disabled:pointer-events-none ring-offset-background"

    variant = variant_classes(assigns.variant)
    size = size_classes(assigns.size)
    loading = if assigns.loading, do: "opacity-50 pointer-events-none", else: ""

    Enum.join([base, variant, size, loading, assigns.class], " ")
  end

  defp variant_classes(variant) do
    case variant do
      "default" -> "bg-primary text-primary-foreground hover:bg-primary/90"
      "destructive" -> "bg-destructive text-destructive-foreground hover:bg-destructive/90"
      "outline" -> "border border-input hover:bg-accent hover:text-accent-foreground"
      "secondary" -> "bg-secondary text-secondary-foreground hover:bg-secondary/80"
      "ghost" -> "hover:bg-accent hover:text-accent-foreground"
      "link" -> "underline-offset-4 hover:underline text-primary"
      _ -> "bg-primary text-primary-foreground hover:bg-primary/90"
    end
  end

  defp size_classes(size) do
    case size do
      "default" -> "h-10 py-2 px-4"
      "sm" -> "h-9 px-3 rounded-md"
      "lg" -> "h-11 px-8 rounded-md"
      _ -> "h-10 py-2 px-4"
    end
  end
end
