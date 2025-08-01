defmodule Storybook.Business.BusinessCard do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Business.BusinessCard.business_card/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          business: %{
            id: "123e4567-e89b-12d3-a456-426614174000",
            name: "Sunny Beach Resort",
            description:
              "A beautiful beachfront resort with stunning ocean views and world-class amenities.",
            address: "123 Ocean Drive, Miami Beach, FL 33139",
            phone: "+1 (305) 555-0123",
            email: "info@sunnybeachresort.com",
            website: "https://sunnybeachresort.com",
            business_type: "resort",
            is_active: true,
            inserted_at: ~N[2024-01-15 10:30:00],
            updated_at: ~N[2024-01-15 10:30:00]
          },
          current_user: %{
            id: "123e4567-e89b-12d3-a456-426614174000",
            email: "owner@sunnybeachresort.com"
          },
          is_admin: true,
          on_edit: "edit_business",
          on_delete: "delete_business"
        }
      },
      %Variation{
        id: :restaurant,
        attributes: %{
          business: %{
            id: "456e7890-e89b-12d3-a456-426614174001",
            name: "The Garden Bistro",
            description:
              "Farm-to-table dining experience with seasonal menus and locally sourced ingredients.",
            address: "456 Main Street, Downtown, CA 90210",
            phone: "+1 (555) 123-4567",
            email: "reservations@gardenbistro.com",
            website: "https://gardenbistro.com",
            business_type: "restaurant",
            is_active: true,
            inserted_at: ~N[2024-02-01 14:20:00],
            updated_at: ~N[2024-02-01 14:20:00]
          },
          current_user: %{
            id: "different-user-id",
            email: "user@example.com"
          },
          is_admin: false,
          on_edit: "edit_business",
          on_delete: "delete_business"
        }
      },
      %Variation{
        id: :inactive_business,
        attributes: %{
          business: %{
            id: "789e0123-e89b-12d3-a456-426614174002",
            name: "Closed Venue",
            description: "This business is currently inactive.",
            address: "789 Closed Street, Nowhere, NY 10001",
            phone: "+1 (555) 999-0000",
            email: "contact@closedvenue.com",
            website: nil,
            business_type: "venue",
            is_active: false,
            inserted_at: ~N[2023-12-01 09:00:00],
            updated_at: ~N[2024-01-01 12:00:00]
          },
          current_user: %{
            id: "different-user-id",
            email: "user@example.com"
          },
          is_admin: false,
          on_edit: "edit_business",
          on_delete: "delete_business"
        }
      },
      %Variation{
        id: :with_custom_class,
        attributes: %{
          business: %{
            id: "abc123-def456-ghi789",
            name: "Custom Styled Business",
            description: "This business card has custom styling applied.",
            address: "123 Custom Ave, Style City, SC 12345",
            phone: "+1 (555) CUSTOM",
            email: "hello@customstyle.com",
            website: "https://customstyle.com",
            business_type: "other",
            is_active: true,
            inserted_at: ~N[2024-03-01 16:45:00],
            updated_at: ~N[2024-03-01 16:45:00]
          },
          current_user: %{
            id: "abc123-def456-ghi789",
            email: "hello@customstyle.com"
          },
          is_admin: true,
          on_edit: "edit_business",
          on_delete: "delete_business",
          class: "border-2 border-purple-300 bg-purple-50"
        }
      }
    ]
  end
end
