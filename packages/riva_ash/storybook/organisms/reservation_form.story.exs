defmodule Storybook.Organisms.ReservationForm do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Organisms.ReservationForm.reservation_form/1

  def variations do
    [
      %Variation{
        id: :step_1,
        attributes: %{
          form: %{
            id: "reservation-form",
            source: %{
              client_id: "",
              item_id: "",
              reserved_date: "",
              start_time: "",
              end_time: ""
            },
            errors: []
          },
          clients: [
            %{id: "1", name: "John Doe"},
            %{id: "2", name: "Jane Smith"},
            %{id: "3", name: "Bob Johnson"}
          ],
          items: [
            %{id: "1", name: "Beach Umbrella"},
            %{id: "2", name: "Sunbed"},
            %{id: "3", name: "Cabin"}
          ],
          employees: [
            %{id: "1", name: "Alice Williams"},
            %{id: "2", name: "Charlie Brown"}
          ],
          on_submit: "create_reservation",
          on_change: "validate_reservation",
          on_cancel: "cancel_form",
          loading: false,
          step: 1
        }
      },
      %Variation{
        id: :step_2,
        attributes: %{
          form: %{
            id: "reservation-form",
            source: %{
              client_id: "1",
              item_id: "2",
              reserved_date: "",
              start_time: "",
              end_time: ""
            },
            errors: []
          },
          clients: [
            %{id: "1", name: "John Doe"},
            %{id: "2", name: "Jane Smith"},
            %{id: "3", name: "Bob Johnson"}
          ],
          items: [
            %{id: "1", name: "Beach Umbrella"},
            %{id: "2", name: "Sunbed"},
            %{id: "3", name: "Cabin"}
          ],
          employees: [
            %{id: "1", name: "Alice Williams"},
            %{id: "2", name: "Charlie Brown"}
          ],
          on_submit: "create_reservation",
          on_change: "validate_reservation",
          on_cancel: "cancel_form",
          loading: false,
          step: 2
        }
      },
      %Variation{
        id: :loading,
        attributes: %{
          form: %{
            id: "reservation-form",
            source: %{
              client_id: "1",
              item_id: "2",
              reserved_date: "2024-06-15",
              start_time: "10:00",
              end_time: "12:00"
            },
            errors: []
          },
          clients: [
            %{id: "1", name: "John Doe"},
            %{id: "2", name: "Jane Smith"},
            %{id: "3", name: "Bob Johnson"}
          ],
          items: [
            %{id: "1", name: "Beach Umbrella"},
            %{id: "2", name: "Sunbed"},
            %{id: "3", name: "Cabin"}
          ],
          employees: [
            %{id: "1", name: "Alice Williams"},
            %{id: "2", name: "Charlie Brown"}
          ],
          on_submit: "create_reservation",
          on_change: "validate_reservation",
          on_cancel: "cancel_form",
          loading: true,
          step: 2
        }
      }
    ]
  end
end
