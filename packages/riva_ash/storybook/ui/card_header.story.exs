defmodule Storybook.UI.CardHeader do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.UI.CardHeader.card_header/1

  def doc do
    """
    # CardHeader

    A header section component for the Card component. Used for organizing card content with a distinct header area.

    ## Features

    - Flexible container for card header content
    - Consistent styling with the design system
    - Supports custom CSS classes
    - Works with CardTitle and CardDescription components
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{class: "w-96"},
        slots: [
          """
          <Card.Title>Card Title</Card.Title>
          <Card.Description>Card description</Card.Description>
          """
        ]
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :with_custom_styling,
        description: "Card header with custom styling",
        template: """
        <Card.Header class="flex-row items-center justify-between w-96">
          <div>
            <Card.Title>Account Settings</Card.Title>
            <Card.Description>Manage your account preferences</Card.Description>
          </div>
          <Badge>Updated</Badge>
        </Card.Header>
        """
      },
      %Example{
        id: :simple,
        description: "Simple card header with title only",
        template: """
        <Card.Header class="w-96">
          <Card.Title>Notifications</Card.Title>
        </Card.Header>
        """
      }
    ]
  end
end
