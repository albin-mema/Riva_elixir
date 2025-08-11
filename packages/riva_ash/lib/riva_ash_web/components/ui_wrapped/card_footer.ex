defmodule RivaAshWeb.Components.UIWrapped.CardFooter do
  @moduledoc """
  App-level CardFooter wrapper.

  Minimal, stable API that enforces accessibility and design tokens while allowing
  advanced usage via global attributes (:rest). Prefer this wrapper over direct
  HTML usage in application code while we evaluate and migrate existing UI.
  """
  use Phoenix.Component

  @doc """
  Renders a card footer with app-level props.
  """
  attr :class, :string, default: ""
  attr :disabled, :boolean, default: false
  attr :loading, :boolean, default: false
  attr :rest, :global

  slot :inner_block, required: true

  def card_footer(assigns) do
    assigns =
      assigns
      |> assign_new(:_salad_disabled, fn -> Map.get(assigns, :disabled, false) or Map.get(assigns, :loading, false) end)

    ~H"""
    <div
      class={[
        "flex items-center p-6 pt-0",
        @class
      ]}
      disabled={@_salad_disabled}
      {@rest}
    >
      <%= if @loading do %>
        <span class="mr-2 w-4 h-4 inline-block align-middle border-2 border-current border-t-transparent rounded-full animate-spin" aria-hidden="true"></span>
      <% end %>
      <%= render_slot(@inner_block) %>
    </div>
    """
  end
end
