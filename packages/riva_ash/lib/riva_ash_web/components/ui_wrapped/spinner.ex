defmodule RivaAshWeb.Components.UIWrapped.Spinner do
  @moduledoc """
  App-level Spinner wrapper around SaladUI Spinner.

  Minimal, stable API that enforces accessibility and design tokens while allowing
  advanced usage via global attributes (:rest). Prefer this wrapper over direct
  SaladUI usage in application code while we evaluate and migrate existing UI.
  """
  use Phoenix.Component

  @doc """
  Renders a SaladUI-backed spinner with app-level props.
  """
  attr :variant, :string,
    default: "default",
    values: ~w(default primary secondary),
    doc: "Visual variant"

  attr :size, :string,
    default: "default",
    values: ~w(default xs sm lg xl),
    doc: "Size variant"

  attr :loading, :boolean, default: false
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def spinner(assigns) do
    assigns =
      assigns
      |> assign_new(:_salad_variant, fn -> map_variant(assigns.variant) end)
      |> assign_new(:_salad_size, fn -> size_to_salad(assigns.size) end)

    ~H"""
    <div
      class={[
        "inline-flex items-center justify-center",
        @class
      ]}
      {@rest}
    >
      <%= if @loading do %>
        <span class="mr-2 w-4 h-4 inline-block align-middle border-2 border-current border-t-transparent rounded-full animate-spin" aria-hidden="true"></span>
      <% end %>
      <%= render_slot(@inner_block) %>
    </div>
    """
  end

  # Map our stable API to SaladUI expected props
  defp map_variant("default"), do: "default"
  defp map_variant(v) when v in ["primary", "secondary"], do: v
  defp map_variant(_), do: "default"

  defp size_to_salad("default"), do: "default"
  defp size_to_salad(s) when s in ["xs", "sm", "lg", "xl"], do: s
  defp size_to_salad(_), do: "default"
end
