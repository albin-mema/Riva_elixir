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

  def breadcrumb_trail(assigns) do
    ~H"""
    <!-- Breadcrumb trail implementation will go here -->
    <nav {@rest} class={["breadcrumb-trail", @class]} aria-label="Breadcrumb">
      <ol>
        <li :if={@show_home}>
          <a href={@home_path} class="breadcrumb-item home">
            <.icon name={:home} />
            <span>Home</span>
          </a>
          <.icon name={@separator} class="separator" />
        </li>
        
        <!-- Handle truncation if too many items -->
        <%= if length(@items) > @max_items do %>
          <li>
            <span class="breadcrumb-item truncated">...</span>
            <.icon name={@separator} class="separator" />
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
              <.icon :if={index < length(Enum.take(@items, -(@max_items - 1))) - 1} name={@separator} class="separator" />
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
              <.icon :if={index < length(@items) - 1} name={@separator} class="separator" />
            <% end %>
          </li>
        <% end %>
      </ol>
    </nav>
    """
  end
end
