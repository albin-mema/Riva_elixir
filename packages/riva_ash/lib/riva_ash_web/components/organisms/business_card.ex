defmodule RivaAshWeb.Components.Organisms.BusinessCard do
  @moduledoc """
  BusinessCard organism component for displaying business information.
  Uses atomic design components to provide a consistent card experience.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Text
  import RivaAshWeb.Components.Atoms.Icon
  import RivaAshWeb.Components.Atoms.Badge
  # Prefer canonical UI.Button at call sites; remove unused Atoms.Button import
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

  @spec business_card(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def business_card(assigns) do
    # Render business card using functional composition
    assigns
    |> Map.put_new(:container_class, build_container_class(assigns.class))
    |> Map.put_new(:body_class, build_body_class())
    |> Map.put_new(:content_class, build_content_class())
    |> Map.put_new(:actions_class, build_actions_class())
    |> render_business_card_component()
  end

  # Private helper for business card rendering
  @spec render_business_card_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_business_card_component(assigns) do
    ~H"""
    <.card variant="bordered" class={@container_class}>
      <:body>
        <div class={@body_class}>
          <div class={@content_class}>
            <.business_header business={@business} current_user={@current_user} is_admin={@is_admin} />
            <.business_content business={@business} />
            <.business_metadata business={@business} />
          </div>

          <.business_actions
            business_id={@business.id}
            on_edit={@on_edit}
            on_delete={@on_delete}
            class={@actions_class}
          />
        </div>
      </:body>
    </.card>
    """
  end

  # Helper function to build container classes
  @spec build_container_class(String.t()) :: String.t()
  defp build_container_class(class) do
    ["hover:shadow-md transition-shadow", class]
    |> Enum.filter(&(&1 != ""))
    |> Enum.join(" ")
  end

  # Helper function to build body classes
  @spec build_body_class() :: String.t()
  defp build_body_class, do: "flex justify-between items-start"

  # Helper function to build content classes
  @spec build_content_class() :: String.t()
  defp build_content_class, do: "flex-1 min-w-0"

  # Helper function to build actions classes
  @spec build_actions_class() :: String.t()
  defp build_actions_class, do: "flex flex-shrink-0 items-center gap-2 ml-4"

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
      <RivaAshWeb.Components.UI.Button.button
        variant="outline"
        size="sm"
        phx-click={@on_edit}
        phx-value-id={@business_id}
      >
        Edit
      </RivaAshWeb.Components.UI.Button.button>

      <RivaAshWeb.Components.UI.Button.button
        variant="destructive"
        size="sm"
        phx-click={@on_delete}
        phx-value-id={@business_id}
        data-confirm="Are you sure you want to delete this business? This action cannot be undone."
      >
        Delete
      </RivaAshWeb.Components.UI.Button.button>
    </div>
    """
  end

  defp format_date(datetime) do
    Calendar.strftime(datetime, "%B %d, %Y")
  end
end
