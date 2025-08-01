defmodule RivaAshWeb.Components.Molecules.NotificationToast do
  @moduledoc """
  Toast notification component for temporary messages.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Icon
  import RivaAshWeb.Components.Atoms.Button

  @doc """
  Renders a toast notification.
  """
  attr(:title, :string, default: nil)
  attr(:message, :string, required: true)
  attr(:type, :string, default: "info", values: ~w(success error warning info))
  attr(:duration, :integer, default: 5000)
  attr(:dismissible, :boolean, default: true)
  attr(:show, :boolean, default: false)

  attr(:position, :string,
    default: "top-right",
    values: ~w(top-left top-right bottom-left bottom-right)
  )

  attr(:on_dismiss, :string, default: nil)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def notification_toast(assigns) do
    ~H"""
    <!-- Toast notification implementation will go here -->
    <div :if={@show} {@rest}>
      <.icon name={:information_circle} />
      <div>
        <h4 :if={@title}><%= @title %></h4>
        <p><%= @message %></p>
      </div>
      <.button :if={@dismissible} variant="ghost" size="sm" phx-click={@on_dismiss}>
        <.icon name={:x_mark} />
      </.button>
    </div>
    """
  end
end
