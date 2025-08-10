defmodule RivaAshWeb.Components.UI.Button do
  use Phoenix.Component
  import TwMerge

  def button(assigns) do
    assigns =
      assigns
      |> assign_new(:variant, fn -> "default" end)
      |> assign_new(:size, fn -> "default" end)
      |> assign_new(:disabled, fn -> false end)
      |> assign_new(:loading, fn -> false end)

    # Map our variant to SaladUI's expected values
    saladui_variant =
      case assigns.variant do
        "primary" -> "default"
        "secondary" -> "secondary"
        "destructive" -> "destructive"
        _ -> assigns.variant
      end

    # Map our size to SaladUI's expected values
    saladui_size =
      case assigns.size do
        "sm" -> "sm"
        "lg" -> "lg"
        _ -> "default"
      end

    extra_attributes = assigns_to_attributes(assigns, [:variant, :size, :disabled, :loading, :class])

    ~H"""
    <SaladUI.Button.button
      variant={saladui_variant}
      size={saladui_size}
      disabled={@disabled || @loading}
      class={tw_merge([
        "inline-flex items-center justify-center rounded-md text-sm font-medium ring-offset-background transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2",
        @class
      ])}
      {@extra_attributes}
    >
      <%= if @loading do %>
        <SaladUI.Spinner.spinner class="mr-2 w-4 h-4 animate-spin" />
      <% end %>
      <%= render_slot(@inner_block) %>
    </SaladUI.Button.button>
    """
  end
end
