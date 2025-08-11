defmodule RivaAshWeb.Components.UIWrapped.Select do
  @moduledoc """
  App-level Select wrapper around SaladUI.Select.

  Minimal, stable API that enforces accessibility and design tokens while allowing
  advanced usage via global attributes (:rest). Prefer this wrapper over direct
  SaladUI usage in application code while we evaluate and migrate existing UI.
  """
  use Phoenix.Component

  @doc """
  Renders a SaladUI-backed select with app-level props.
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

  def select(assigns) do
    assigns =
      assigns
      |> assign_new(:_salad_variant, fn -> map_variant(assigns.variant) end)
      |> assign_new(:_salad_size, fn -> size_to_salad(assigns.size) end)

    ~H"""
    <SaladUI.Select.select
      variant={@_salad_variant}
      size={@_salad_size}
      disabled={@disabled or @loading}
      class={[
        "flex w-full rounded-md border border-input bg-background px-3 py-2 text-sm ring-offset-background focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50",
        @class
      ]}
      {@rest}
    >
      <%= if @loading do %>
        <span class="mr-2 w-4 h-4 inline-block align-middle border-2 border-current border-t-transparent rounded-full animate-spin" aria-hidden="true"></span>
      <% end %>
      <%= render_slot(@inner_block) %>
    </SaladUI.Select.select>
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
