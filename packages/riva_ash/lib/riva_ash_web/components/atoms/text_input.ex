defmodule RivaAshWeb.Components.Atoms.TextInput do
  use Phoenix.Component

  def text_input(assigns) do
    ~H"""
    <input
      type={@type || "text"}
      class={[
        "border px-3 py-2 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500",
        @class
      ]}
      name={@name}
      id={@id || @name}
      value={@value}
      placeholder={@placeholder}
      phx-debounce={@phx_debounce}
      phx-change={@phx_change}
      phx-keydown={@phx_keydown}
      disabled={@disabled}
    />
    """
  end
end
