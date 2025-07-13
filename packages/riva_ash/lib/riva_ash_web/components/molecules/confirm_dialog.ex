defmodule RivaAshWeb.Components.Molecules.ConfirmDialog do
  @moduledoc """
  Confirmation dialog component for destructive actions.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Icon

  @doc """
  Renders a confirmation dialog.
  """
  attr :title, :string, required: true
  attr :message, :string, required: true
  attr :confirm_label, :string, default: "Confirm"
  attr :cancel_label, :string, default: "Cancel"
  attr :variant, :string, default: "destructive", values: ~w(destructive warning info)
  attr :on_confirm, :string, required: true
  attr :on_cancel, :string, required: true
  attr :show, :boolean, default: false
  attr :class, :string, default: ""
  attr :rest, :global

  def confirm_dialog(assigns) do
    ~H"""
    <!-- Confirm dialog implementation will go here -->
    <div :if={@show} {@rest}>
      <div>
        <h3><%= @title %></h3>
        <p><%= @message %></p>
        <div>
          <.button variant="destructive" phx-click={@on_confirm}><%= @confirm_label %></.button>
          <.button variant="outline" phx-click={@on_cancel}><%= @cancel_label %></.button>
        </div>
      </div>
    </div>
    """
  end
end
