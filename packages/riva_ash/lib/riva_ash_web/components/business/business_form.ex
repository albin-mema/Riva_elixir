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

          <!-- Public Search Settings -->
          <div class="space-y-4 p-4 bg-blue-50 rounded-lg border border-blue-200">
            <h3 class="text-lg font-medium text-blue-900">Public Search Settings</h3>
            <p class="text-sm text-blue-700">Control how your business appears in global search for unregistered users.</p>

            <div class="flex items-center space-x-3">
              <input
                type="checkbox"
                id="is_public_searchable"
                name={@form[:is_public_searchable].name}
                value="true"
                checked={@form[:is_public_searchable].value}
                class="h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded"
              />
              <.label for="is_public_searchable" class="text-sm font-medium text-gray-700">
                Make this business discoverable in global search
              </.label>
            </div>

            <div class="space-y-2">
              <.label for="public_description">Public Description</.label>
              <.textarea
                field={@form[:public_description]}
                placeholder="Enter a public-facing description for search results (optional)"
                rows="3"
              />
              <p class="text-xs text-gray-500">This description will be shown to unregistered users in search results. Leave empty to use the main description.</p>
              <.field_errors field={@form[:public_description]} />
            </div>
          </div>

          <!-- Location Information -->
          <div class="space-y-4 p-4 bg-green-50 rounded-lg border border-green-200">
            <h3 class="text-lg font-medium text-green-900">Location Information</h3>
            <p class="text-sm text-green-700">Help customers find you by providing location details.</p>

            <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div class="space-y-2">
                <.label for="city">City</.label>
                <.input
                  field={@form[:city]}
                  type="text"
                  placeholder="Enter city name"
                />
                <.field_errors field={@form[:city]} />
              </div>

              <div class="space-y-2">
                <.label for="country">Country</.label>
                <.input
                  field={@form[:country]}
                  type="text"
                  placeholder="Enter country name"
                />
                <.field_errors field={@form[:country]} />
              </div>
            </div>

            <div class="space-y-2">
              <.label for="address">Full Address</.label>
              <.textarea
                field={@form[:address]}
                placeholder="Enter complete address (optional)"
                rows="2"
              />
              <p class="text-xs text-gray-500">This helps with local search and customer directions.</p>
              <.field_errors field={@form[:address]} />
            </div>
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
