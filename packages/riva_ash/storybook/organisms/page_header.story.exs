defmodule Storybook.Organisms.PageHeader do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Organisms.PageHeader.page_header/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          title: "Business Management",
          description: "Manage your business entities and their information"
        },
        slots: [
          """
          <:badge>
            <.badge variant="outline">
              5 businesses
            </.badge>
          </:badge>

          <:action>
            <.button variant="default">
              <.icon name={:plus} class="mr-2 h-4 w-4" />
              Add Business
            </.button>
          </:action>
          """
        ]
      },
      %Variation{
        id: :with_icon_and_breadcrumbs,
        attributes: %{
          title: "Dashboard",
          icon: :home,
          breadcrumbs: [
            %{label: "Home", href: "/"},
            %{label: "Dashboard", current: true}
          ]
        }
      },
      %Variation{
        id: :compact,
        attributes: %{
          title: "User Profile",
          description: "View and edit your profile information",
          variant: "compact"
        },
        slots: [
          """
          <:action>
            <.button variant="outline">
              <.icon name={:pencil} class="mr-2 h-4 w-4" />
              Edit Profile
            </.button>
          </:action>
          """
        ]
      },
      %Variation{
        id: :card_variant,
        attributes: %{
          title: "Reservation Details",
          description: "View and manage reservation information",
          variant: "card"
        },
        slots: [
          """
          <:badge>
            <.badge variant="success">
              Confirmed
            </.badge>
          </:badge>

          <:action>
            <.button variant="outline">
              <.icon name={:printer} class="mr-2 h-4 w-4" />
              Print
            </.button>
            <.button variant="default">
              <.icon name={:pencil} class="mr-2 h-4 w-4" />
              Edit
            </.button>
          </:action>
          """
        ]
      }
    ]
  end
end
