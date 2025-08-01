defmodule RivaAshWeb.Components.Molecules.BreadcrumbNav do
  @moduledoc """
  Breadcrumb navigation component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Icon

  @doc """
  Renders breadcrumb navigation.
  """
  attr(:items, :list, required: true)
  attr(:separator, :atom, default: :chevron_right)
  attr(:show_home, :boolean, default: true)
  attr(:home_path, :string, default: "/")
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def breadcrumb_nav(assigns) do
    ~H"""
    <!-- Breadcrumb navigation implementation will go here -->
    <nav {@rest}>
      <ol>
        <li :if={@show_home}>
          <a href={@home_path}>Home</a>
          <.icon name={@separator} />
        </li>
        <li :for={{item, index} <- Enum.with_index(@items)}>
          <a :if={!item[:current]} href={item[:href]}><%= item[:label] %></a>
          <span :if={item[:current]}><%= item[:label] %></span>
          <.icon :if={index < length(@items) - 1} name={@separator} />
        </li>
      </ol>
    </nav>
    """
  end
end
