defmodule Storybook.Organisms.PermissionMatrix do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Organisms.PermissionMatrix.permission_matrix/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          employee: %{
            id: "1",
            first_name: "John",
            last_name: "Doe"
          },
          permissions: [
            %{id: "1", name: "manage_reservations", description: "Can create, edit, and delete reservations"},
            %{id: "2", name: "view_financials", description: "Can view financial reports and revenue data"},
            %{id: "3", name: "manage_employees", description: "Can add, edit, and remove employees"},
            %{id: "4", name: "manage_business", description: "Can modify business settings and information"}
          ],
          current_permissions: ["1", "2"],
          on_permission_change: "permission_changed",
          on_save: "save_permissions",
          on_cancel: "cancel_permissions",
          loading: false
        }
      },
      %Variation{
        id: :loading,
        attributes: %{
          employee: %{
            id: "2",
            first_name: "Jane",
            last_name: "Smith"
          },
          permissions: [
            %{id: "1", name: "manage_reservations", description: "Can create, edit, and delete reservations"},
            %{id: "2", name: "view_financials", description: "Can view financial reports and revenue data"},
            %{id: "3", name: "manage_employees", description: "Can add, edit, and remove employees"},
            %{id: "4", name: "manage_business", description: "Can modify business settings and information"}
          ],
          current_permissions: ["1", "3", "4"],
          on_permission_change: "permission_changed",
          on_save: "save_permissions",
          on_cancel: "cancel_permissions",
          loading: true
        }
      },
      %Variation{
        id: :no_current_permissions,
        attributes: %{
          employee: %{
            id: "3",
            first_name: "Bob",
            last_name: "Johnson"
          },
          permissions: [
            %{id: "1", name: "manage_reservations", description: "Can create, edit, and delete reservations"},
            %{id: "2", name: "view_financials", description: "Can view financial reports and revenue data"},
            %{id: "3", name: "manage_employees", description: "Can add, edit, and remove employees"},
            %{id: "4", name: "manage_business", description: "Can modify business settings and information"}
          ],
          current_permissions: [],
          on_permission_change: "permission_changed",
          on_save: "save_permissions",
          on_cancel: "cancel_permissions",
          loading: false
        }
      }
    ]
  end
end
