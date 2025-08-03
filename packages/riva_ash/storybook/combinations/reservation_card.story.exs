defmodule RivaAshWeb.Storybook.Combinations.ReservationCard do
  use PhoenixStorybook.Story, :component
  use RivaAshWeb.Components.AtomicComponents

  def function, do: &reservation_card/1

  def reservation_card(assigns) do
    ~H"""
    <div class="p-4 max-w-md">
      <.card variant="elevated">
        <:body>
          <div class="flex items-center justify-between mb-4">
            <.badge variant={status_variant(@reservation.status)}>
              <%= String.capitalize(to_string(@reservation.status)) %>
            </.badge>
            <.text variant="small" color="muted"><%= @reservation.id %></.text>
          </div>

          <div class="mb-4">
            <.text variant="h4" class="mb-2">
              <%= @reservation.customer %>
            </.text>
            <.text variant="small" color="muted">
              <%= Date.to_string(@reservation.date) %>
            </.text>
          </div>

          <%= if @reservation[:items] do %>
            <div class="space-y-1">
              <.text variant="small" class="font-medium">Items:</.text>
              <%= for item <- @reservation.items do %>
                <.text variant="small" color="muted">• <%= item %></.text>
              <% end %>
            </div>
          <% end %>

          <%= if @reservation[:availability] do %>
            <div class="mt-4 p-2 bg-muted rounded">
              <.text variant="small">
                Availability: <%= @reservation.availability.booked %>/<%= @reservation.availability.total %>
              </.text>
            </div>
          <% end %>
        </:body>
      </.card>
    </div>
    """
  end

  defp status_variant(:confirmed), do: "secondary"
  defp status_variant(:conflict), do: "destructive"
  defp status_variant(_), do: "outline"

  def variations do
    [
      %Variation{
        id: :confirmed,
        attributes: %{
          reservation: %{
            id: "RSV-001",
            date: Date.utc_today(),
            status: :confirmed,
            customer: "John Doe",
            items: ["Table #5", "Chair set"],
            availability: %{total: 15, booked: 8}
          }
        }
      },
      %Variation{
        id: :with_conflict,
        attributes: %{
          reservation: %{
            id: "RSV-002",
            date: Date.utc_today(),
            status: :conflict,
            customer: "Jane Smith",
            items: ["VIP Booth"]
          }
        }
      }
    ]
  end
end
