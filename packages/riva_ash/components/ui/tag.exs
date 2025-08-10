defmodule RivaAsh.Components.UI.Tag do
  @moduledoc """
  Tag/Chip atom component with semantic variants and removable state.
  Composed per composition guidelines 188-189 using container + text + optional close IconButton.
  """

  use Phoenix.Component
  import RivaAsh.Components.UI.Atoms

  @variants [:default, :primary, :success, :warning, :danger]

  @doc """
  Renders a Tag/Chip component with the following properties:
  - variant: semantic variant (default/primary/success/warning/danger)
  - removable: whether the tag has a close button
  - label: text content of the tag
  - on_remove: optional event handler for remove action (required when removable: true)
  """
  def tag(assigns) do
    assigns =
      assigns
      |> assign_new(:variant, fn -> :default end)
      |> assign_new(:removable, fn -> false end)
      |> assign_new(:label, fn -> "Tag" end)
      |> validate_variant()

    container_classes = container_classes(assigns.variant, assigns.removable)
    text_classes = text_classes(assigns.variant)
    close_button_classes = focus_styles()

    assigns =
      assign(assigns,
        container_classes: container_classes,
        text_classes: text_classes,
        close_button_classes: close_button_classes
      )

    ~H"""
    <div class={@container_classes} {@aria_attributes}>
      <span class={@text_classes}><%= @label %></span>
      <%= if @removable do %>
        <.icon_button
          variant={:ghost}
          size={:xs}
          aria_label="Remove {@label}"
          class={[@close_button_classes, "p-0.5"]}
          phx_click={@on_remove}
        >
          <.icon name="x" class="w-3 h-3" />
        </.icon_button>
      <% end %>
    </div>
    """
  end

  defp validate_variant(%{variant: variant} = assigns) do
    validated =
      case variant do
        v when v in @variants -> v
        _ -> :default
      end

    assign(assigns, :variant, validated)
  end

  defp container_classes(:default, false),
    do: "bg-muted text-muted-foreground rounded-full px-2 py-1 inline-flex items-center gap-1.5"

  defp container_classes(:primary, false),
    do: "bg-primary text-primary-foreground rounded-full px-2 py-1 inline-flex items-center gap-1.5"

  defp container_classes(:success, false),
    do: "bg-success text-success-foreground rounded-full px-2 py-1 inline-flex items-center gap-1.5"

  defp container_classes(:warning, false),
    do: "bg-warning text-warning-foreground rounded-full px-2 py-1 inline-flex items-center gap-1.5"

  defp container_classes(:danger, false),
    do: "bg-destructive text-destructive-foreground rounded-full px-2 py-1 inline-flex items-center gap-1.5"

  defp container_classes(variant, true),
    do: container_classes(variant, false) <> " pr-1"

  defp text_classes(:default), do: "text-xs font-medium"
  defp text_classes(_), do: "text-xs font-medium"
end
