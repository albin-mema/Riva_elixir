defmodule RivaAshWeb.Components.Molecules.StatusIndicator do
  @moduledoc """
  Status indicator component with colors and icons.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Badge
  import RivaAshWeb.Components.Atoms.Icon

  @doc """
  Renders a status indicator with appropriate styling.
  """
  attr(:status, :string, required: true)
  attr(:label, :string, default: nil)
  attr(:show_icon, :boolean, default: true)
  attr(:show_pulse, :boolean, default: false)
  attr(:size, :string, default: "md", values: ~w(sm md lg))
  attr(:variant, :string, default: "auto", values: ~w(auto success warning error info))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def status_indicator(assigns) do
    ~H"""
    <!-- Status indicator implementation will go here -->
    <.badge {@rest}>
      <.icon :if={@show_icon} name={:check_circle} />
      <%= @label || @status %>
    </.badge>
    """
  end
end
