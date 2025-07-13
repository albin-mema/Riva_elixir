defmodule RivaAshWeb.Components.Atoms.Avatar do
  @moduledoc """
  Avatar component for users and businesses with fallbacks.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Icon

  @doc """
  Renders an avatar with image or initials fallback.
  """
  attr :src, :string, default: nil
  attr :alt, :string, default: ""
  attr :initials, :string, default: nil
  attr :name, :string, default: nil
  attr :size, :string, default: "md", values: ~w(xs sm md lg xl 2xl)
  attr :shape, :string, default: "circle", values: ~w(circle square rounded)
  attr :status, :string, default: nil, values: ~w(online offline away busy)
  attr :class, :string, default: ""
  attr :rest, :global

  def avatar(assigns) do
    ~H"""
    <!-- Avatar implementation will go here -->
    <div {@rest}>
      <img :if={@src} src={@src} alt={@alt} />
      <div :if={!@src && @initials}><%= @initials %></div>
      <.icon :if={!@src && !@initials} name={:user} />
    </div>
    """
  end
end
