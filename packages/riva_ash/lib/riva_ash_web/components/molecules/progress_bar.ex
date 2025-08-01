defmodule RivaAshWeb.Components.Molecules.ProgressBar do
  @moduledoc """
  Progress bar component for indicating completion status.
  """
  use Phoenix.Component

  @doc """
  Renders a progress bar.
  """
  attr(:value, :integer, required: true)
  attr(:max, :integer, default: 100)
  attr(:label, :string, default: nil)
  attr(:show_percentage, :boolean, default: true)
  attr(:size, :string, default: "md", values: ~w(sm md lg))
  attr(:variant, :string, default: "default", values: ~w(default success warning error))
  attr(:animated, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def progress_bar(assigns) do
    ~H"""
    <!-- Progress bar implementation will go here -->
    <div {@rest}>
      <div :if={@label || @show_percentage}>
        <span :if={@label}><%= @label %></span>
        <span :if={@show_percentage}><%= round(@value / @max * 100) %>%</span>
      </div>
      <div>
        <div style={"width: #{@value / @max * 100}%"}><!-- Progress fill --></div>
      </div>
    </div>
    """
  end
end
