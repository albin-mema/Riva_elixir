defmodule Storybook.Molecules.Card do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Molecules.Card.card/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{},
        slots: [
          """
          <:header>
            <:title>Card Title</:title>
            <:subtitle>Card subtitle</:subtitle>
          </:header>
          <:content>
            This is the main content of the card. It can contain any HTML or Phoenix components.
          </:content>
          <:footer>
            <button class="px-4 py-2 bg-blue-500 text-white rounded">Action</button>
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
            <:title>Bordered Card</:title>
            <:subtitle>This card has a border</:subtitle>
          </:header>
          <:content>
            This card uses the bordered variant which adds a border around the card.
          </:content>
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
            <:title>Elevated Card</:title>
            <:subtitle>This card has elevation</:subtitle>
          </:header>
          <:content>
            This card uses the elevated variant which adds a shadow to create depth.
          </:content>
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
            <:title>Custom Styled Card</:title>
            <:subtitle>With custom CSS classes</:subtitle>
          </:header>
          <:content>
            This card demonstrates how you can apply custom CSS classes to style the card.
          </:content>
          """
        ]
      },
      %Variation{
        id: :content_only,
        attributes: %{},
        slots: [
          """
          <:content>
            This is a simple card with only content, no header or footer.
          </:content>
          """
        ]
      }
    ]
  end
end
