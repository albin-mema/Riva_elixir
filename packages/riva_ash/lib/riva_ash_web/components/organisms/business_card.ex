defmodule RivaAshWeb.Components.Organisms.BusinessCard do
  @moduledoc """
  BusinessCard organism component for displaying business information.
  Uses atomic design components to provide a consistent card experience.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Text
  import RivaAshWeb.Components.Atoms.Icon
  import RivaAshWeb.Components.Atoms.Badge
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Molecules.Card

  @doc """
  Renders a business card with all business information and actions.

  ## Examples

      <.business_card
        business={@business}
        current_user={@current_user}
        is_admin={@is_admin}
        on_edit="edit_business"
        on_delete="delete_business"
      />
  """
  attr(:business, :map, required: true)
  attr(:current_user, :map, required: true)
  attr(:is_admin, :boolean, default: false)
  attr(:on_edit, :string, required: true)
  attr(:on_delete, :string, required: true)
  attr(:class, :string, default: "")

  def business_card(assigns) do
    ~H"""
    <.card variant="bordered" class={["hover:shadow-md transition-shadow", @class]}>
      <:body>
        <div class="flex justify-between items-start">
          <div class="flex-1 min-w-0">
            <.business_header business={@business} current_user={@current_user} is_admin={@is_admin} />
            <.business_content business={@business} />
            <.business_metadata business={@business} />
          </div>

          <.business_actions
            business_id={@business.id}
            on_edit={@on_edit}
            on_delete={@on_delete}
          />
        </div>
      </:body>
    </.card>
    """
  end

  defp business_header(assigns) do
    ~H"""
    <div class="flex items-center gap-3 mb-3">
      <.heading level={4} class="truncate">
        <%= @business.name %>
      </.heading>

      <.badge variant="secondary" size="sm">
        ID: <%= String.slice(@business.id, 0, 8) %>...
      </.badge>

      <%= if @business.owner_id == @current_user.id do %>
        <.badge variant="success" icon={:check} size="sm">
          Owner
        </.badge>
      <% end %>

      <%= if @is_admin && @business.owner_id != @current_user.id do %>
        <.badge variant="outline" icon={:user} size="sm">
          User <%= String.slice(@business.owner_id, 0, 8) %>...
        </.badge>
      <% end %>
    </div>
    """
  end

  defp business_content(assigns) do
    ~H"""
    <div class="mb-4">
      <%= if @business.description do %>
        <.text color="muted">
          <%= @business.description %>
        </.text>
      <% else %>
        <.text color="muted" italic>
          No description provided
        </.text>
      <% end %>
    </div>
    """
  end

  defp business_metadata(assigns) do
    ~H"""
    <div class="flex flex-wrap items-center gap-4">
      <div class="flex items-center gap-1.5 text-muted-foreground text-sm">
        <.icon name={:calendar} size="sm" />
        <span>Created: <%= format_date(@business.inserted_at) %></span>
      </div>

      <%= if @business.updated_at != @business.inserted_at do %>
        <div class="flex items-center gap-1.5 text-muted-foreground text-sm">
          <.icon name={:clock} size="sm" />
          <span>Updated: <%= format_date(@business.updated_at) %></span>
        </div>
      <% end %>
    </div>
    """
  end

  defp business_actions(assigns) do
    ~H"""
    <div class="flex flex-shrink-0 items-center gap-2 ml-4">
      <.button
        variant="outline"
        size="sm"
        phx-click={@on_edit}
        phx-value-id={@business_id}
        icon_left={:pencil}
      >
        Edit
      </.button>

      <.button
        variant="destructive"
        size="sm"
        phx-click={@on_delete}
        phx-value-id={@business_id}
        data-confirm="Are you sure you want to delete this business? This action cannot be undone."
        icon_left={:trash}
      >
        Delete
      </.button>
    </div>
    """
  end

  defp format_date(datetime) do
    Calendar.strftime(datetime, "%B %d, %Y")
  end
end
