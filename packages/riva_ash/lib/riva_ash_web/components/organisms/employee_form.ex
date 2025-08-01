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

  def employee_form(assigns) do
    ~H"""
    <.card variant="elevated" class={@class}>
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
        <.form
          for={@form}
          phx-submit={@on_submit}
          phx-change={@on_change}
          class="space-y-6"
        >
          <div class="gap-4 grid grid-cols-1 md:grid-cols-2">
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

          <div class="gap-4 grid grid-cols-1 md:grid-cols-2">
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
         />

          <div class="gap-4 grid grid-cols-1 md:grid-cols-2">
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
                  <%= Enum.map(@form[:role].errors, &elem(&1, 0)) |> Enum.join(", ") %>
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
                  <%= Enum.map(@form[:business_id].errors, &elem(&1, 0)) |> Enum.join(", ") %>
                </p>
              <% end %>
            </div>
          <% end %>

          <div class="flex items-center">
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
          />

          <div class="flex justify-end space-x-3 pt-4 border-t">
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
      </:body>
    </.card>
    """
  end
end
