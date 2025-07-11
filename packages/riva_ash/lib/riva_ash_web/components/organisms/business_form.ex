defmodule RivaAshWeb.Components.Organisms.BusinessForm do
  @moduledoc """
  BusinessForm organism component for creating and editing businesses.
  Uses atomic design components to provide a consistent form experience.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Molecules.Card
  import RivaAshWeb.Components.Molecules.FormField

  @doc """
  Renders a business form for creating or editing businesses.

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
    <.card variant="elevated" class={@class}>
      <:header>
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
      </:header>

      <:body>
        <.form
          for={@form}
          phx-submit={@on_submit}
          phx-change={@on_change}
          class="space-y-6"
        >
          <.form_field
            field={@form[:name]}
            label="Business Name"
            placeholder="Enter business name"
            required={true}
            icon={:building_office_2}
          />

          <.textarea_field
            field={@form[:description]}
            label="Description"
            placeholder="Enter business description (optional)"
            helper_text="Provide a brief description of your business"
            rows={4}
          />

          <div class="flex gap-3 pt-4">
            <.button
              type="submit"
              variant="primary"
              disabled={@loading}
              loading={@loading}
              icon_left={if @editing, do: :check, else: :plus}
            >
              <%= if @editing, do: "Update Business", else: "Create Business" %>
            </.button>

            <.button
              type="button"
              variant="outline"
              phx-click={@on_cancel}
              icon_left={:x_mark}
            >
              Cancel
            </.button>
          </div>
        </.form>
      </:body>
    </.card>
    """
  end
end
