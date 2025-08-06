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
  attr(:employee, :map, required: true)
  attr(:permissions, :list, required: true)
  attr(:current_permissions, :list, default: [])
  attr(:on_permission_change, :string, required: true)
  attr(:on_save, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:loading, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @spec permission_matrix(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def permission_matrix(assigns) do
    # Render permission matrix using functional composition
    assigns
    |> Map.put_new(:matrix_class, build_matrix_class(assigns.class, assigns.variant))
    |> Map.put_new(:header_class, build_header_class(assigns.employee))
    |> Map.put_new(:title_class, build_title_class(assigns.employee))
    |> Map.put_new(:permissions_class, build_permissions_class(assigns.permissions))
    |> Map.put_new(:checkbox_class, build_checkbox_class(assigns.permissions))
    |> Map.put_new(:actions_class, build_actions_class(assigns.loading))
    |> render_permission_matrix_component()
  end

  # Private helper for permission matrix rendering
  @spec render_permission_matrix_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_permission_matrix_component(assigns) do
    ~H"""
    <div class={@matrix_class} {@rest}>
      <div class={@header_class}>
        <h3 class={@title_class}>
          Permissions for <%= @employee.first_name %> <%= @employee.last_name %>
        </h3>
      </div>

      <div class={@permissions_class}>
        <%= for permission <- @permissions do %>
          <div class={@checkbox_class}>
            <.checkbox
              value={permission.id}
              checked={permission.id in @current_permissions}
              label={permission.name}
              description={permission.description}
              phx-click={@on_permission_change}
              phx-value-permission={permission.id}
            />
          </div>
        <% end %>
      </div>

      <div class={@actions_class}>
        <.button phx-click={@on_save} loading={@loading}>Save Permissions</.button>
        <.button variant="outline" phx-click={@on_cancel}>Cancel</.button>
      </div>
    </div>
    """
  end

  # Helper function to build matrix classes
  @spec build_matrix_class(String.t(), String.t()) :: String.t()
  defp build_matrix_class(class, variant) do
    base =
      case variant do
        "compact" -> "space-y-3"
        "card" -> "bg-card rounded-lg p-6 shadow-sm space-y-4"
        _ -> "space-y-4"
      end

    Enum.join([base, class], " ")
  end

  # Helper function to build header classes
  @spec build_header_class(map()) :: String.t()
  defp build_header_class(employee) do
    if employee, do: "mb-4", else: "hidden"
  end

  # Helper function to build title classes
  @spec build_title_class(map()) :: String.t()
  defp build_title_class(employee) do
    if employee, do: "text-lg font-semibold", else: "hidden"
  end

  # Helper function to build permissions container classes
  @spec build_permissions_class(list()) :: String.t()
  defp build_permissions_class(permissions) do
    if permissions != [], do: "space-y-2", else: "hidden"
  end

  # Helper function to build checkbox container classes
  @spec build_checkbox_class(list()) :: String.t()
  defp build_checkbox_class(permissions) do
    if permissions != [], do: "flex items-start space-x-3", else: "hidden"
  end

  # Helper function to build actions classes
  @spec build_actions_class(boolean()) :: String.t()
  defp build_actions_class(loading) do
    "flex gap-3"
  end
end
