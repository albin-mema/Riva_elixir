defmodule RivaAshWeb.Components.Navigation.BreadcrumbTrail do
  @moduledoc """
  Dynamic breadcrumb navigation component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Icon

  @doc """
  Renders dynamic breadcrumb navigation.
  """
  attr(:items, :list, required: true)
  attr(:separator, :atom, default: :chevron_right)
  attr(:show_home, :boolean, default: true)
  attr(:home_path, :string, default: "/")
  attr(:max_items, :integer, default: 5)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @spec breadcrumb_trail(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def breadcrumb_trail(assigns) do
    # Render breadcrumb trail using functional composition
    assigns
    |> Map.put_new(:container_class, build_container_class(assigns.class))
    |> Map.put_new(:home_class, build_home_class(assigns.show_home))
    |> Map.put_new(:separator_class, build_separator_class(assigns.separator))
    |> render_breadcrumb_trail_component()
  end

  # Private helper for breadcrumb trail rendering
  @spec render_breadcrumb_trail_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_breadcrumb_trail_component(assigns) do
    ~H"""
    <!-- Breadcrumb trail implementation will go here -->
    <nav {@rest} class={["breadcrumb-trail", @container_class]} aria-label="Breadcrumb">
      <ol>
        <li :if={@show_home}>
          <a href={@home_path} class={["breadcrumb-item home", @home_class]}>
            <.icon name={:home} />
            <span>Home</span>
          </a>
          <.icon name={@separator} class={["separator", @separator_class]} />
        </li>
        
        <!-- Handle truncation if too many items -->
        <%= if length(@items) > @max_items do %>
          <li>
            <span class="breadcrumb-item truncated">...</span>
            <.icon name={@separator} class={["separator", @separator_class]} />
          </li>
          
          <!-- Show only last few items -->
          <li :for={{item, index} <- Enum.with_index(Enum.take(@items, -(@max_items - 1)))}>
            <%= if item[:current] do %>
              <span class="breadcrumb-item current" aria-current="page">
                <%= item[:label] %>
              </span>
            <% else %>
              <a href={item[:href]} class="breadcrumb-item">
                <%= item[:label] %>
              </a>
              <.icon :if={index < length(Enum.take(@items, -(@max_items - 1))) - 1} name={@separator} class={["separator", @separator_class]} />
            <% end %>
          </li>
        <% else %>
          <!-- Show all items -->
          <li :for={{item, index} <- Enum.with_index(@items)}>
            <%= if item[:current] do %>
              <span class="breadcrumb-item current" aria-current="page">
                <%= item[:label] %>
              </span>
            <% else %>
              <a href={item[:href]} class="breadcrumb-item">
                <%= item[:label] %>
              </a>
              <.icon :if={index < length(@items) - 1} name={@separator} class={["separator", @separator_class]} />
            <% end %>
          </li>
        <% end %>
      </ol>
    </nav>
    """
  end

  # Helper function to build container classes
  @spec build_container_class(String.t()) :: String.t()
  defp build_container_class(class) do
    class
  end

  # Helper function to build home classes
  @spec build_home_class(boolean()) :: String.t()
  defp build_home_class(show_home) do
    if show_home, do: "", else: "hidden"
  end

  # Helper function to build separator classes
  @spec build_separator_class(atom()) :: String.t()
  defp build_separator_class(separator) do
    "separator"
  end
end
