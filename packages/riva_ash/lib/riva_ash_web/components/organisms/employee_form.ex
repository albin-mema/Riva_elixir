defmodule RivaAshWeb.Components.Organisms.EmployeeForm do
  @moduledoc """
  EmployeeForm organism component for creating and editing employees.
  Uses atomic design components to provide a consistent form experience.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Molecules.Card
  import RivaAshWeb.Components.Molecules.FormField

  @doc """
  Renders an employee form for creating or editing employees.

  ## Examples

      <.employee_form
        form={@form}
        editing={@editing_employee}
        loading={@loading}
        businesses={@businesses}
        on_submit="save_employee"
        on_change="validate_employee"
        on_cancel="cancel_form"
      />
  """
  attr(:form, :map, required: true)
  attr(:editing, :boolean, default: false)
  attr(:loading, :boolean, default: false)
  attr(:businesses, :list, default: [])
  attr(:permissions, :list, default: [])
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:class, :string, default: "")

  @spec employee_form(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def employee_form(assigns) do
    # Render employee form using functional composition
    assigns
    |> Map.put_new(:container_class, build_container_class(assigns.class))
    |> Map.put_new(:header_class, build_header_class(assigns.editing))
    |> Map.put_new(:body_class, build_body_class())
    |> Map.put_new(:form_class, build_form_class())
    |> Map.put_new(:personal_info_class, build_personal_info_class())
    |> Map.put_new(:contact_info_class, build_contact_info_class())
    |> Map.put_new(:permissions_class, build_permissions_class(assigns.permissions))
    |> Map.put_new(:role_business_class, build_role_business_class(assigns.businesses))
    |> Map.put_new(:status_class, build_status_class())
    |> Map.put_new(:notes_class, build_notes_class())
    |> Map.put_new(:actions_class, build_actions_class())
    |> render_employee_form_component()
  end

  # Private helper for employee form rendering
  @spec render_employee_form_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_employee_form_component(assigns) do
    ~H"""
    <.card variant="elevated" class={@container_class}>
      <:header>
        <.card_title>
          <%= if @editing, do: "Edit Employee", else: "Add New Employee" %>
        </.card_title>
        <.card_description>
          <%= if @editing do %>
            Update the employee information below.
          <% else %>
            Fill in the details to add a new employee to your business.
          <% end %>
        </.card_description>
      </:header>

      <:body>
        <div class={@body_class}>
          <.form
            for={@form}
            phx-submit={@on_submit}
            phx-change={@on_change}
            class={@form_class}
          >
            <div class={@personal_info_class}>
              <.form_field
                field={@form[:first_name]}
                label="First Name"
                type="text"
                placeholder="John"
                required={true}
              />

              <.form_field
                field={@form[:last_name]}
                label="Last Name"
                type="text"
                placeholder="Doe"
                required={true}
              />
            </div>

            <.form_field
              field={@form[:email]}
              label="Email Address"
              type="email"
              placeholder="john.doe@example.com"
              required={true}
              helper_text="This will be used for employee login"
            />

            <div class={@contact_info_class}>
              <.form_field
                field={@form[:phone]}
                label="Phone Number"
                type="tel"
                placeholder="+1 (555) 123-4567"
              />

              <.form_field
               field={@form[:employee_number]}
               label="Employee Number"
               type="text"
               placeholder="EMP001"
               helper_text="Optional badge ID or employee number"
             />
          </div>

          <.form_field
            field={@form[:permission_ids]}
            label="Permissions"
            type="select"
            multiple={true}
            options={Enum.map(@permissions, &{&1.name, &1.id})}
            class={@permissions_class}
          />

            <div class={@role_business_class}>
              <div>
                <label for={@form[:role].id} class="block mb-1 font-medium text-gray-700 text-sm">
                  Role <span class="text-red-500">*</span>
                </label>
                <select
                  id={@form[:role].id}
                  name={@form[:role].name}
                  class="px-3 py-2 border border-gray-300 focus:border-transparent rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 w-full"
                  required
                >
                  <option value="">Select a role</option>
                  <option value="staff" selected={@form[:role].value == "staff"}>Staff</option>
                  <option value="manager" selected={@form[:role].value == "manager"}>Manager</option>
                  <option value="admin" selected={@form[:role].value == "admin"}>Admin</option>
                </select>
                <%= if @form[:role].errors != [] do %>
                  <p class="mt-1 text-red-600 text-sm">
                    <%= Enum.map_join(@form[:role].errors, ", ", &elem(&1, 0)) %>
                  </p>
                <% end %>
              </div>

              <.form_field
                field={@form[:hire_date]}
                label="Hire Date"
                type="date"
              />
            </div>

            <%= if @businesses != [] do %>
              <div>
                <label for={@form[:business_id].id} class="block mb-1 font-medium text-gray-700 text-sm">
                  Business <span class="text-red-500">*</span>
                </label>
                <select
                  id={@form[:business_id].id}
                  name={@form[:business_id].name}
                  class="px-3 py-2 border border-gray-300 focus:border-transparent rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 w-full"
                  required
                >
                  <option value="">Select a business</option>
                  <%= for business <- @businesses do %>
                    <option value={business.id} selected={@form[:business_id].value == business.id}>
                      <%= business.name %>
                    </option>
                  <% end %>
                </select>
                <%= if @form[:business_id].errors != [] do %>
                  <p class="mt-1 text-red-600 text-sm">
                    <%= Enum.map_join(@form[:business_id].errors, ", ", &elem(&1, 0)) %>
                  </p>
                <% end %>
              </div>
            <% end %>

            <div class={@status_class}>
              <input
                type="checkbox"
                id={@form[:is_active].id}
                name={@form[:is_active].name}
                value="true"
                checked={@form[:is_active].value}
                class="border-gray-300 rounded focus:ring-blue-500 w-4 h-4 text-blue-600"
              />
              <label for={@form[:is_active].id} class="block ml-2 text-gray-900 text-sm">
                Active Employee
              </label>
              <input type="hidden" name={@form[:is_active].name} value="false" />
            </div>

            <.form_field
              field={@form[:notes]}
              label="Notes"
              type="textarea"
              placeholder="Additional notes about the employee..."
              helper_text="Optional notes or comments"
              class={@notes_class}
            />

            <div class={@actions_class}>
              <.button
                type="button"
                variant="outline"
                phx-click={@on_cancel}
                disabled={@loading}
              >
                Cancel
              </.button>

              <.button
                type="submit"
                variant="primary"
                loading={@loading}
              >
                <%= if @editing, do: "Update Employee", else: "Add Employee" %>
              </.button>
            </div>
          </.form>
        </div>
      </:body>
    </.card>
    """
  end

  # Helper function to build container classes
  @spec build_container_class(String.t()) :: String.t()
  defp build_container_class(class) do
    class
  end

  # Helper function to build header classes
  @spec build_header_class(boolean()) :: String.t()
  defp build_header_class(editing) do
    ""
  end

  # Helper function to build body classes
  @spec build_body_class() :: String.t()
  defp build_body_class, do: ""

  # Helper function to build form classes
  @spec build_form_class() :: String.t()
  defp build_form_class, do: "space-y-6"

  # Helper function to build personal info classes
  @spec build_personal_info_class() :: String.t()
  defp build_personal_info_class, do: "gap-4 grid grid-cols-1 md:grid-cols-2"

  # Helper function to build contact info classes
  @spec build_contact_info_class() :: String.t()
  defp build_contact_info_class, do: "gap-4 grid grid-cols-1 md:grid-cols-2"

  # Helper function to build permissions classes
  @spec build_permissions_class(list()) :: String.t()
  defp build_permissions_class(permissions) do
    if permissions != [], do: "", else: "hidden"
  end

  # Helper function to build role business classes
  @spec build_role_business_class(list()) :: String.t()
  defp build_role_business_class(businesses) do
    "gap-4 grid grid-cols-1 md:grid-cols-2"
  end

  # Helper function to build status classes
  @spec build_status_class() :: String.t()
  defp build_status_class, do: "flex items-center"

  # Helper function to build notes classes
  @spec build_notes_class() :: String.t()
  defp build_notes_class, do: ""

  # Helper function to build actions classes
  @spec build_actions_class() :: String.t()
  defp build_actions_class, do: "flex justify-end space-x-3 pt-4 border-t"
end
