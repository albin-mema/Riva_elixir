defmodule RivaAshWeb.Components.Business.BusinessForm do
  @moduledoc """
  Reusable business form component for creating and editing businesses.
  """
  use Phoenix.Component
  import SaladUI.Button
  import SaladUI.Card
  import SaladUI.Input
  import SaladUI.Label
  import SaladUI.Textarea

  @doc """
  Renders a business form.

  ## Examples

      <.business_form
        form={@form}
        editing={@editing_business}
        loading={@loading}
        on_submit="save_business"
        on_change="validate_business"
        on_cancel="cancel_form"
      />
  """
  attr :form, :map, required: true
  attr :editing, :boolean, default: false
  attr :loading, :boolean, default: false
  attr :on_submit, :string, required: true
  attr :on_change, :string, required: true
  attr :on_cancel, :string, required: true
  attr :class, :string, default: ""

  def business_form(assigns) do
    ~H"""
    <.card class={["bg-card", @class]}>
      <.card_header>
        <.card_title>
          <%= if @editing, do: "Edit Business", else: "Create New Business" %>
        </.card_title>
        <.card_description>
          <%= if @editing do %>
            Update the business information below.
          <% else %>
            Fill in the details to create a new business entity.
          <% end %>
        </.card_description>
      </.card_header>
      <.card_content>
        <.form
          for={@form}
          phx-submit={@on_submit}
          phx-change={@on_change}
          class="space-y-6"
        >
          <div class="space-y-2">
            <.label for="name">Business Name *</.label>
            <.input
              field={@form[:name]}
              type="text"
              placeholder="Enter business name"
              required
            />
            <.field_errors field={@form[:name]} />
          </div>

          <div class="space-y-2">
            <.label for="description">Description</.label>
            <.textarea
              field={@form[:description]}
              placeholder="Enter business description (optional)"
              rows="4"
            />
            <.field_errors field={@form[:description]} />
          </div>

          <div class="flex gap-3 pt-4">
            <.button
              type="submit"
              variant="default"
              disabled={@loading}
              class="flex items-center gap-2"
            >
              <span :if={@loading} class="animate-spin">⏳</span>
              <%= if @editing, do: "Update Business", else: "Create Business" %>
            </.button>

            <.button
              type="button"
              variant="outline"
              phx-click={@on_cancel}
            >
              Cancel
            </.button>
          </div>
        </.form>
      </.card_content>
    </.card>
    """
  end

  @doc """
  Renders field errors for a form field.
  """
  attr :field, :map, required: true

  defp field_errors(assigns) do
    ~H"""
    <div :if={@field.errors != []} class="text-destructive text-sm">
      <%= for error <- @field.errors do %>
        <p>• <%= error %></p>
      <% end %>
    </div>
    """
  end
end
