defmodule RivaAshWeb.Components.Atoms.Spinner do
  @moduledoc """
  Loading spinner component with various styles.
  """
  use Phoenix.Component

  @doc """
  Renders a loading spinner.
  """
  attr :size, :string, default: "md", values: ~w(xs sm md lg xl)
  attr :variant, :string, default: "default", values: ~w(default primary secondary)
  attr :label, :string, default: "Loading..."
  attr :show_label, :boolean, default: false
  attr :class, :string, default: ""
  attr :rest, :global

  def spinner(assigns) do
    ~H"""
    <!-- Spinner implementation will go here -->
    <div {@rest}>
      <div><!-- Spinner animation --></div>
      <span :if={@show_label}><%= @label %></span>
    </div>
    """
  end
end
