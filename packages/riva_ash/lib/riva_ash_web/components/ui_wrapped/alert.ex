defmodule RivaAshWeb.Components.UIWrapped.Alert do
  @moduledoc """
  App-level Alert wrapper around SaladUI.Alert.

  Minimal, stable API that enforces accessibility and design tokens while allowing
  advanced usage via global attributes (:rest). Prefer this wrapper over direct
  SaladUI usage in application code while we evaluate and migrate existing UI.
  """
  use Phoenix.Component

  @doc """
  Renders a SaladUI-backed alert with app-level props.
  """
  attr :variant, :string,
    default: "default",
    values: ~w(default destructive success warning),
    doc: "Visual variant"

  attr :size, :string,
    default: "default",
    values: ~w(default sm lg),
    doc: "Size variant"

  attr :disabled, :boolean, default: false
  attr :loading, :boolean, default: false
  attr :class, :string, default: ""
  attr :rest, :global

  slot :title
  slot :inner_block, required: true

  def alert(assigns) do
    assigns =
      assigns
      |> assign_new(:_salad_variant, fn -> map_variant(assigns.variant) end)
      |> assign_new(:_salad_size, fn -> size_to_salad(assigns.size) end)

    ~H"""
    <SaladUI.Alert.alert
      variant={@_salad_variant}
      size={@_salad_size}
      disabled={@disabled or @loading}
      class={[
        "relative w-full rounded-lg border p-4 [&>svg]:absolute [&>svg]:text-foreground [&>svg]:left-4 [&>svg]:top-4 [&>svg+div]:translate-y-[-3px] [&:has(svg)]:pl-11",
        @class
      ]}
      {@rest}
    >
      <%= if @loading do %>
        <span class="mr-2 w-4 h-4 inline-block align-middle border-2 border-current border-t-transparent rounded-full animate-spin" aria-hidden="true"></span>
      <% end %>
      <%= if @title do %>
        <h5 class="mb-1 font-medium leading-none tracking-tight">
          <%= render_slot(@title) %>
        </h5>
      <% end %>
      <div class={if @title, do: "text-sm [&_p]:leading-relaxed", else: "text-sm [&_p]:leading-relaxed"}>
        <%= render_slot(@inner_block) %>
      </div>
    </SaladUI.Alert.alert>
    """
  end

  # Map our stable API to SaladUI expected props
  defp map_variant("primary"), do: "default"
  defp map_variant(v) when v in ["default", "destructive", "success", "warning"], do: v
  defp map_variant(_), do: "default"

  defp size_to_salad("md"), do: "default"
  defp size_to_salad(s) when s in ["default", "sm", "lg"], do: s
  defp size_to_salad(_), do: "default"
end
