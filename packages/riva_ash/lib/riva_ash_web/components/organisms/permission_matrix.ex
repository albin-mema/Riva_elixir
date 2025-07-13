defmodule RivaAshWeb.Components.Organisms.PermissionMatrix do
  @moduledoc """
  Employee permission management matrix component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Checkbox
  import RivaAshWeb.Components.Atoms.Button

  @doc """
  Renders a permission matrix for employee role management.
  """
  attr :employee, :map, required: true
  attr :permissions, :list, required: true
  attr :current_permissions, :list, default: []
  attr :on_permission_change, :string, required: true
  attr :on_save, :string, required: true
  attr :on_cancel, :string, required: true
  attr :loading, :boolean, default: false
  attr :class, :string, default: ""
  attr :rest, :global

  def permission_matrix(assigns) do
    ~H"""
    <!-- Permission matrix implementation will go here -->
    <div {@rest}>
      <h3>Permissions for <%= @employee.first_name %> <%= @employee.last_name %></h3>
      
      <div>
        <div :for={permission <- @permissions}>
          <.checkbox
            value={permission.id}
            checked={permission.id in @current_permissions}
            label={permission.name}
            description={permission.description}
            phx-click={@on_permission_change}
            phx-value-permission={permission.id}
          />
        </div>
      </div>
      
      <div>
        <.button phx-click={@on_save} loading={@loading}>Save Permissions</.button>
        <.button variant="outline" phx-click={@on_cancel}>Cancel</.button>
      </div>
    </div>
    """
  end
end
