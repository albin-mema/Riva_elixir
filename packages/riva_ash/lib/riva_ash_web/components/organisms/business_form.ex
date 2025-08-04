defmodule RivaAshWeb.Components.Organisms.BusinessForm do
  @moduledoc """
  BusinessForm organism component for creating and editing businesses.
  Uses atomic design components to provide a consistent form experience.
  """
  use Phoenix.Component
  alias RivaAshWeb.Components.UI.Button, as: UIButton
  alias RivaAshWeb.Components.UI.Text, as: UIText
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
  attr(:form, :map, required: true)
  attr(:editing, :boolean, default: false)
  attr(:loading, :boolean, default: false)
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:class, :string, default: "")

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

          <!-- Public Search Settings -->
          <div class="space-y-4 p-4 bg-blue-50 rounded-lg border border-blue-200">
            <h3 class="text-lg font-medium text-blue-900">Public Search Settings</h3>
            <p class="text-sm text-blue-700">Control how your business appears in global search for unregistered users.</p>

            <.checkbox_field
              field={@form[:is_public_searchable]}
              label="Make this business discoverable in global search"
              helper_text="When enabled, unregistered users can find your business through the global search"
            />

            <.textarea_field
              field={@form[:public_description]}
              label="Public Description"
              placeholder="Enter a public-facing description for search results (optional)"
              helper_text="This description will be shown to unregistered users in search results. Leave empty to use the main description."
              rows={3}
            />
          </div>

          <!-- Location Information -->
          <div class="space-y-4 p-4 bg-green-50 rounded-lg border border-green-200">
            <h3 class="text-lg font-medium text-green-900">Location Information</h3>
            <p class="text-sm text-green-700">Help customers find you by providing location details.</p>

            <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
              <.form_field
                field={@form[:city]}
                label="City"
                placeholder="Enter city name"
                helper_text="City where your business is located"
              />

              <.form_field
                field={@form[:country]}
                label="Country"
                placeholder="Enter country name"
                helper_text="Country where your business operates"
              />
            </div>

            <.textarea_field
              field={@form[:address]}
              label="Full Address"
              placeholder="Enter complete address (optional)"
              helper_text="This helps with local search and customer directions"
              rows={2}
            />
          </div>

          <div class="flex gap-3 pt-4">
            <UIButton.button
              type="submit"
              variant="default"
              disabled={@loading}
              loading={@loading}
            >
              <%= if @editing, do: "Update Business", else: "Create Business" %>
            </UIButton.button>

            <UIButton.button
              type="button"
              variant="outline"
              phx-click={@on_cancel}
            >
              Cancel
            </UIButton.button>
          </div>
        </.form>
      </:body>
    </.card>
    """
  end
end
