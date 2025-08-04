defmodule Storybook.Molecules.Card do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Molecules.Card.card/1

  def doc do
    """
    # Card (Molecule)

    A composed card component that uses canonical UI components internally while providing
    a convenient header/body/footer slot API for page composition.

    ## Features

    - Uses `UI.Card` internally for consistent styling
    - Uses `UI.Text` for title and description components
    - Provides convenient slots for header, body, and footer content
    - Maintains backward compatibility with existing card usage

    ## When to Use

    - **Use Molecules.Card** for page composition where header/body/footer slots are desired
    - **Use UI.Card directly** for low-level layout control

    ## Internal Components

    This molecule uses these canonical UI components:
    - `UI.Card` for the container
    - `UI.Text` for title and description (via `.card_title` and `.card_description`)
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{},
        slots: [
          """
          <:header>
            <.card_title>Card Title</.card_title>
            <.card_description>Card subtitle</.card_description>
          </:header>
          <:body>
            This is the main content of the card. It can contain any HTML or Phoenix components.
          </:body>
          <:footer>
            <RivaAshWeb.Components.UI.Button.button variant="default">Action</RivaAshWeb.Components.UI.Button.button>
          </:footer>
          """
        ]
      },
      %Variation{
        id: :bordered,
        attributes: %{
          variant: "bordered"
        },
        slots: [
          """
          <:header>
            <.card_title>Bordered Card</.card_title>
            <.card_description>This card has a border</.card_description>
          </:header>
          <:body>
            This card uses the bordered variant which adds a border around the card.
          </:body>
          """
        ]
      },
      %Variation{
        id: :elevated,
        attributes: %{
          variant: "elevated"
        },
        slots: [
          """
          <:header>
            <.card_title>Elevated Card</.card_title>
            <.card_description>This card has elevation</.card_description>
          </:header>
          <:body>
            This card uses the elevated variant which adds a shadow to create depth.
          </:body>
          """
        ]
      },
      %Variation{
        id: :with_custom_class,
        attributes: %{
          class: "bg-blue-50 border-blue-200"
        },
        slots: [
          """
          <:header>
            <.card_title>Custom Styled Card</.card_title>
            <.card_description>With custom CSS classes</.card_description>
          </:header>
          <:body>
            This card demonstrates how you can apply custom CSS classes to style the card.
          </:body>
          """
        ]
      },
      %Variation{
        id: :content_only,
        attributes: %{},
        slots: [
          """
          <:body>
            This is a simple card with only content, no header or footer.
          </:body>
          """
        ]
      }
    ]
  end
end
