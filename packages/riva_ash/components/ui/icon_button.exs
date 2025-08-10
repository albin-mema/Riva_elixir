defmodule RivaAsh.Components.UI.IconButton do
  @moduledoc """
  Accessible icon-only button component with standardized props and design tokens.
  Follows atomic design principles with proper focus states and ARIA attributes.
  """

  use Phoenix.Component
  import RivaAsh.Components.UI.Atoms

  def icon_button(assigns) do
    assigns =
      assigns
      |> standard_assigns(variant: :secondary, size: :m)
      |> assign_new(:tooltip_text, fn -> nil end)
      |> assign_new(:icon, fn -> "placeholder" end)

    ~H"""
    <%= if @tooltip_text do %>
      <.tooltip text={@tooltip_text} disabled={@disabled}>
        <button
          type="button"
          role="button"
          class={[
            "inline-flex items-center justify-center rounded-full transition-all",
            variant_classes(:icon_button, @variant),
            size_classes(:icon_button, @size),
            focus_styles(),
            @disabled && "opacity-50 cursor-not-allowed"
          ]}
          {@aria_attributes}
          {@disabled && "disabled"}
        >
          <.icon name={@icon} size={@size} />
        </button>
      </.tooltip>
    <% else %>
      <button
        type="button"
        role="button"
        class={[
          "inline-flex items-center justify-center rounded-full transition-all",
          variant_classes(:icon_button, @variant),
          size_classes(:icon_button, @size),
          focus_styles(),
          @disabled && "opacity-50 cursor-not-allowed"
        ]}
        {@aria_attributes}
        {@disabled && "disabled"}
      >
        <.icon name={@icon} size={@size} />
      </button>
    <% end %>
    """
  end

  defp variant_classes(:icon_button, :primary) do
    "bg-primary text-primary-foreground hover:bg-primary/90 active:bg-primary/80"
  end

  defp variant_classes(:icon_button, :secondary) do
    "bg-secondary text-secondary-foreground hover:bg-secondary/90 active:bg-secondary/80"
  end

  defp variant_classes(:icon_button, :tertiary) do
    "bg-accent text-accent-foreground hover:bg-accent/90 active:bg-accent/80"
  end

  defp size_classes(:icon_button, :xs) do
    "p-1"
  end

  defp size_classes(:icon_button, :s) do
    "p-2"
  end

  defp size_classes(:icon_button, :m) do
    "p-3"
  end
end
