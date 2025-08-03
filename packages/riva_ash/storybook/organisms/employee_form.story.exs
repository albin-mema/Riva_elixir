defmodule Storybook.Organisms.EmployeeForm do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Organisms.EmployeeForm.employee_form/1

  def variations do
    [
      %Variation{
        id: :create_form,
        attributes: %{
          form: %{
            id: "employee-form",
            source: %{
              first_name: "",
              last_name: "",
              email: "",
              phone: "",
              employee_number: "",
              permission_ids: [],
              role: "",
              hire_date: "",
              business_id: "",
              is_active: true,
              notes: ""
            },
            errors: []
          },
          editing: false,
          loading: false,
          businesses: [
            %{id: "1", name: "Sunny Beach Resort"},
            %{id: "2", name: "Mountain View Hotel"}
          ],
          permissions: [
            %{id: "1", name: "manage_reservations", description: "Can manage reservations"},
            %{id: "2", name: "view_financials", description: "Can view financial reports"}
          ],
          on_submit: "save_employee",
          on_change: "validate_employee",
          on_cancel: "cancel_form"
        }
      },
      %Variation{
        id: :edit_form,
        attributes: %{
          form: %{
            id: "employee-form",
            source: %{
              first_name: "John",
              last_name: "Doe",
              email: "john.doe@business.com",
              phone: "+1 (555) 123-4567",
              employee_number: "EMP001",
              permission_ids: ["1"],
              role: "manager",
              hire_date: "2024-01-15",
              business_id: "1",
              is_active: true,
              notes: "Experienced manager with 5 years in hospitality"
            },
            errors: []
          },
          editing: true,
          loading: false,
          businesses: [
            %{id: "1", name: "Sunny Beach Resort"},
            %{id: "2", name: "Mountain View Hotel"}
          ],
          permissions: [
            %{id: "1", name: "manage_reservations", description: "Can manage reservations"},
            %{id: "2", name: "view_financials", description: "Can view financial reports"}
          ],
          on_submit: "save_employee",
          on_change: "validate_employee",
          on_cancel: "cancel_form"
        }
      },
      %Variation{
        id: :loading_form,
        attributes: %{
          form: %{
            id: "employee-form",
            source: %{
              first_name: "John",
              last_name: "Doe",
              email: "john.doe@business.com",
              phone: "+1 (555) 123-4567",
              employee_number: "EMP001",
              permission_ids: ["1"],
              role: "manager",
              hire_date: "2024-01-15",
              business_id: "1",
              is_active: true,
              notes: "Experienced manager with 5 years in hospitality"
            },
            errors: []
          },
          editing: true,
          loading: true,
          businesses: [
            %{id: "1", name: "Sunny Beach Resort"},
            %{id: "2", name: "Mountain View Hotel"}
          ],
          permissions: [
            %{id: "1", name: "manage_reservations", description: "Can manage reservations"},
            %{id: "2", name: "view_financials", description: "Can view financial reports"}
          ],
          on_submit: "save_employee",
          on_change: "validate_employee",
          on_cancel: "cancel_form"
        }
      }
    ]
  end
end
