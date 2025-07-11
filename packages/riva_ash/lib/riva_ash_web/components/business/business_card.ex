defmodule RivaAshWeb.Components.Business.BusinessCard do
  @moduledoc """
  Reusable business card component for displaying business information.
  """
  use Phoenix.Component
  import SaladUI.Card
  import SaladUI.Badge
  import SaladUI.Button

  @doc """
  Renders a business card with actions.

  ## Examples

      <.business_card
        business={@business}
        current_user={@current_user}
        is_admin={@is_admin}
        on_edit="edit_business"
        on_delete="delete_business"
      />
  """
  attr :business, :map, required: true
  attr :current_user, :map, required: true
  attr :is_admin, :boolean, default: false
  attr :on_edit, :string, required: true
  attr :on_delete, :string, required: true
  attr :class, :string, default: ""

  def business_card(assigns) do
    ~H"""
    <.card class={["hover:shadow-md transition-shadow bg-card", @class]}>
      <.card_content class="p-6">
        <div class="flex justify-between items-start">
          <div class="flex-1">
            <div class="flex items-center gap-3 mb-2">
              <h3 class="font-semibold text-foreground text-lg"><%= @business.name %></h3>
              <.business_id_badge id={@business.id} />
              <.owner_badge
                business={@business}
                current_user={@current_user}
                is_admin={@is_admin}
              />
            </div>

            <.business_description description={@business.description} />
            <.business_metadata business={@business} />
          </div>

          <.business_actions
            business_id={@business.id}
            on_edit={@on_edit}
            on_delete={@on_delete}
          />
        </div>
      </.card_content>
    </.card>
    """
  end

  @doc """
  Renders the business ID badge.
  """
  attr :id, :string, required: true

  defp business_id_badge(assigns) do
    ~H"""
    <.badge variant="secondary">
      ID: <%= String.slice(@id, 0, 8) %>...
    </.badge>
    """
  end

  @doc """
  Renders the owner badge.
  """
  attr :business, :map, required: true
  attr :current_user, :map, required: true
  attr :is_admin, :boolean, required: true

  defp owner_badge(assigns) do
    ~H"""
    <.badge
      :if={@business.owner_id == @current_user.id}
      variant="outline"
      class="bg-green-50 text-green-700 border-green-200"
    >
      <span class="flex items-center gap-1">
        <svg xmlns="http://www.w3.org/2000/svg" class="h-3 w-3" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7" />
        </svg>
        Owner
      </span>
    </.badge>
    <.badge
      :if={@is_admin && @business.owner_id != @current_user.id}
      variant="outline"
      class="bg-blue-50 text-blue-700 border-blue-200"
    >
      <span class="flex items-center gap-1">
        <svg xmlns="http://www.w3.org/2000/svg" class="h-3 w-3" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z" />
        </svg>
        <%= @business.owner.name || "User #{String.slice(@business.owner_id, 0, 4)}..." %>
      </span>
    </.badge>
    """
  end

  @doc """
  Renders the business description.
  """
  attr :description, :string

  defp business_description(assigns) do
    ~H"""
    <p :if={@description} class="mb-4 text-muted-foreground">
      <%= @description %>
    </p>
    <p :if={!@description} class="mb-4 text-muted-foreground italic">
      No description provided
    </p>
    """
  end

  @doc """
  Renders business metadata (dates).
  """
  attr :business, :map, required: true

  defp business_metadata(assigns) do
    ~H"""
    <div class="flex flex-wrap items-center gap-4 text-muted-foreground text-sm">
      <span class="flex items-center gap-1">
        <svg xmlns="http://www.w3.org/2000/svg" class="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z" />
        </svg>
        Created: <%= Calendar.strftime(@business.inserted_at, "%B %d, %Y") %>
      </span>
      <span :if={@business.updated_at != @business.inserted_at} class="flex items-center gap-1">
        <svg xmlns="http://www.w3.org/2000/svg" class="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z" />
        </svg>
        Updated: <%= Calendar.strftime(@business.updated_at, "%B %d, %Y") %>
      </span>
    </div>
    """
  end

  @doc """
  Renders business action buttons.
  """
  attr :business_id, :string, required: true
  attr :on_edit, :string, required: true
  attr :on_delete, :string, required: true

  defp business_actions(assigns) do
    ~H"""
    <div class="flex items-center gap-2 ml-4">
      <.button
        variant="outline"
        size="sm"
        phx-click={@on_edit}
        phx-value-id={@business_id}
        class="flex items-center gap-1"
      >
        <svg xmlns="http://www.w3.org/2000/svg" class="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M11 5H6a2 2 0 00-2 2v11a2 2 0 002 2h11a2 2 0 002-2v-5m-1.414-9.414a2 2 0 112.828 2.828L11.828 15H9v-2.828l8.586-8.586z" />
        </svg>
        Edit
      </.button>
      <.button
        variant="destructive"
        size="sm"
        phx-click={@on_delete}
        phx-value-id={@business_id}
        data-confirm="Are you sure you want to delete this business? This action cannot be undone."
        class="flex items-center gap-1"
      >
        <svg xmlns="http://www.w3.org/2000/svg" class="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16" />
        </svg>
        Delete
      </.button>
    </div>
    """
  end
end
