defmodule RivaAshWeb.Components.Atoms.Button do
  use Phoenix.Component

  def button(assigns) do
    ~H"""
    <button
      type={@type || "button"}
      class={[
        "px-4 py-2 rounded-md font-semibold text-white",
        @class
      ]}
      phx-disable-with={@phx_disable_with}
      phx-click={@phx_click}
      phx-value={@phx_value}
      phx-value-id={@phx_value_id}
      disabled={@disabled}
    >
      <%= render_slot(@inner_block) %>
    </button>
    """
  end
end
