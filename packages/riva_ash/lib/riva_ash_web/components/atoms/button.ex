defmodule RivaAshWeb.Components.Atoms.Button do
  use Phoenix.Component

  @doc """
  Renders a button component.
  """
  attr :type, :string, default: "button"
  attr :class, :string, default: ""
  attr :disabled, :boolean, default: false
  attr :rest, :global, include: ~w(phx-click phx-disable-with phx-value phx-value-id)

  slot :inner_block, required: true

  def button(assigns) do
    ~H"""
    <button
      type={@type}
      class={[
        "px-4 py-2 rounded-md font-semibold text-white",
        @class
      ]}
      disabled={@disabled}
      {@rest}
    >
      <%= render_slot(@inner_block) %>
    </button>
    """
  end
end
