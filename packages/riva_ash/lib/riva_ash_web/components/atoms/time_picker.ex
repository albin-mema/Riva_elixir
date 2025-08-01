defmodule RivaAshWeb.Components.Atoms.TimePicker do
  @moduledoc """
  Time picker component for time selection.
  """
  use Phoenix.Component

  @doc """
  Renders a time picker input.
  """
  attr(:field, Phoenix.HTML.FormField, default: nil)
  attr(:value, :string, default: nil)
  attr(:min_time, :string, default: nil)
  attr(:max_time, :string, default: nil)
  attr(:step, :integer, default: 60)
  attr(:placeholder, :string, default: "Select time")
  attr(:disabled, :boolean, default: false)
  attr(:required, :boolean, default: false)
  attr(:format, :string, default: "24", values: ~w(12 24))
  attr(:size, :string, default: "md", values: ~w(sm md lg))
  attr(:variant, :string, default: "default", values: ~w(default error success))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def time_picker(assigns) do
    ~H"""
    <!-- Time picker implementation will go here -->
    <input type="time" {@rest} />
    """
  end
end
