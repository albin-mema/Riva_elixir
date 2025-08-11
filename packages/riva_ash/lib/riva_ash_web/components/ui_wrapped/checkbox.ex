defmodule RivaAshWeb.Components.UIWrapped.Checkbox do
  @moduledoc """
  App-level Checkbox wrapper around SaladUI.Checkbox.

  Minimal, stable API that enforces accessibility and design tokens while allowing
  advanced usage via global attributes (:rest). Prefer this wrapper over direct
  SaladUI usage in application code while we evaluate and migrate existing UI.
  """
  use Phoenix.Component

  @doc """
  Renders a SaladUI-backed checkbox with app-level props.
  """
  attr :variant, :string,
    default: "default",
    values: ~w(default secondary destructive outline ghost link),
    doc: "Visual variant"

  attr :size, :string,
    default: "default",
    values: ~w(default sm lg icon),
    doc: "Size variant"

  attr :disabled, :boolean, default: false
  attr :loading, :boolean, default: false
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true
  slot :description

  def checkbox(assigns) do
    assigns =
      assigns
      |> assign_new(:_salad_variant, fn -> map_variant(assigns.variant) end)
      |> assign_new(:_salad_size, fn -> size_to_salad(assigns.size) end)

    ~H"""
    <div class={[
      "flex items-start gap-2",
      @class
    ]}>
      <input
        type="checkbox"
        class={[
          "peer h-4 w-4 shrink-0 shadow focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring checked:bg-primary checked:focus:bg-primary checked:hover:bg-primary checked:text-primary-foreground rounded border border-input bg-background focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-ring disabled:cursor-not-allowed disabled:opacity-50",
          @class
        ]}
        disabled={@disabled or @loading}
        {@rest}
      />
      <%= if @loading do %>
        <span class="mr-2 w-4 h-4 inline-block align-middle border-2 border-current border-t-transparent rounded-full animate-spin" aria-hidden="true"></span>
      <% end %>
      <div class={[
        if @description do
          "flex flex-col"
        else
          "flex items-center"
        end
      ]}>
        <label class={[
          "text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70",
          "text-sm"
        ]}>
          <%= render_slot(@inner_block) %>
        </label>
        <%= if length(@description) > 0 do %>
          <p class="mt-1 text-muted-foreground text-sm">
            <%= render_slot(@description) %>
          </p>
        <% end %>
      </div>
    </div>
    """
  end

  # Map our stable API to SaladUI expected props
  defp map_variant("primary"), do: "default"
  defp map_variant(v) when v in ["default", "secondary", "destructive", "outline", "ghost", "link"], do: v
  defp map_variant(_), do: "default"

  defp size_to_salad("md"), do: "default"
  defp size_to_salad(s) when s in ["default", "sm", "lg", "icon"], do: s
  defp size_to_salad(_), do: "default"
end
