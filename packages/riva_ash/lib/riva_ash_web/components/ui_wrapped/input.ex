defmodule RivaAshWeb.Components.UIWrapped.Input do
  @moduledoc """
  App-level Input wrapper around SaladUI.Input.

  Minimal, stable API that enforces accessibility and design tokens while allowing
  advanced usage via global attributes (:rest). Prefer this wrapper over direct
  SaladUI usage in application code while we evaluate and migrate existing UI.
  """
  use Phoenix.Component

  @doc """
  Renders a SaladUI-backed input with app-level props.
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

  slot :inner_block

  def input(assigns) do
    assigns =
      assigns
      |> assign_new(:_salad_variant, fn -> map_variant(assigns.variant) end)
      |> assign_new(:_salad_size, fn -> size_to_salad(assigns.size) end)

    ~H"""
    <SaladUI.Input.input
      variant={@_salad_variant}
      size={@_salad_size}
      disabled={@disabled or @loading}
      class={[
        "flex w-full rounded-md border border-input bg-background px-3 py-2 text-sm ring-offset-background file:border-0 file:bg-transparent file:text-sm file:font-medium placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50",
        @class
      ]}
      {@rest}
    />
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
