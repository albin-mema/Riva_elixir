defmodule Storybook.UI.Card do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.UI.Card.card/1

  def doc do
    """
    # Card

    Displays a card with header, content, and footer.

    ## Features

    - Flexible container for grouping related content
    - Header, content, and footer sections
    - Consistent styling with the design system
    """
  end

  def variations do
    [
      %Variation{
        id: :basic,
        attributes: %{class: "w-96"},
        slots: [
          """
          <Card.Header>
            <Card.Title>Card Title</Card.Title>
            <Card.Description>Card description</Card.Description>
          </Card.Header>
          <Card.Content>
            <p>Card content</p>
          </Card.Content>
          """
        ]
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :with_footer,
        description: "Card with footer",
        template: """
        <Card class="w-96">
          <Card.Header>
            <Card.Title>Notifications</Card.Title>
            <Card.Description>You have 3 unread messages</Card.Description>
          </Card.Header>
          <Card.Content>
            <div class="space-y-4">
              <div class="flex items-center gap-4">
                <div class="bg-primary rounded-full w-8 h-8 flex items-center justify-center">
                  <svg class="h-4 w-4 text-primary-foreground" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 6v6m0 0v6m0-6h6m-6 0H6" />
                  </svg>
                </div>
                <div>
                  <p class="text-sm font-medium">New message</p>
                  <p class="text-sm text-muted-foreground">You have a new message from John</p>
                </div>
              </div>
            </div>
          </Card.Content>
          <Card.Footer>
            <Button variant="secondary">Mark all as read</Button>
          </Card.Footer>
        </Card>
        """
      },
      %Example{
        id: :simple,
        description: "Simple card",
        template: """
        <Card class="w-96">
          <Card.Content>
            <p>This is a simple card with only content.</p>
          </Card.Content>
        </Card>
        """
      }
    ]
  end
end
