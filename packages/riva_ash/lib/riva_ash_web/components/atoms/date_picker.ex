defmodule RivaAshWeb.Components.Atoms.DatePicker do
  @moduledoc """
  Date picker component with calendar popup.
  """
  use Phoenix.Component

  @doc """
  Renders a date picker input.
  """
  attr(:field, Phoenix.HTML.FormField, default: nil)
  attr(:value, :string, default: nil)
  attr(:min_date, :string, default: nil)
  attr(:max_date, :string, default: nil)
  attr(:placeholder, :string, default: "Select date")
  attr(:disabled, :boolean, default: false)
  attr(:required, :boolean, default: false)
  attr(:format, :string, default: "yyyy-mm-dd")
  attr(:size, :string, default: "md", values: ~w(sm md lg))
  attr(:variant, :string, default: "default", values: ~w(default error success))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def date_picker(assigns) do
    ~H"""
    <!-- Date picker implementation will go here -->
    <input type="date" {@rest} />
    """
  end
end
