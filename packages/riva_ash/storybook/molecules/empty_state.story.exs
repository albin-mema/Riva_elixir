defmodule Storybook.Molecules.EmptyState do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Molecules.EmptyState.empty_state/1

  def doc do
    """
    # EmptyState

    EmptyState component for displaying when no data is available.

    ## Features

    - Consistent empty state experience
    - Configurable icon, title, and description
    - Different sizes (sm, md, lg)
    - Different variants (default, bordered, card)
    - Optional action slot
    - Custom CSS classes
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          icon: :inbox,
          title: "No items found",
          description: "There are no items to display."
        }
      },
      %Variation{
        id: :with_action,
        attributes: %{
          icon: :building_office_2,
          title: "No businesses found",
          description: "Create your first business to get started"
        },
        slots: [
          """
          <:action>
            <button class="px-4 py-2 bg-blue-500 text-white rounded">
              <span class="mr-2">+</span>
              Create Business
            </button>
          </:action>
          """
        ]
      },
      %Variation{
        id: :different_sizes,
        template: """
        <div class="space-y-8">
          <.empty_state
            icon={:magnifying_glass}
            title="Small Empty State"
            description="This is a small empty state."
            size="sm"
          />
          <.empty_state
            icon={:magnifying_glass}
            title="Medium Empty State"
            description="This is a medium empty state."
            size="md"
          />
          <.empty_state
            icon={:magnifying_glass}
            title="Large Empty State"
            description="This is a large empty state."
            size="lg"
          />
        </div>
        """
      },
      %Variation{
        id: :different_variants,
        template: """
        <div class="space-y-8">
          <.empty_state
            icon={:inbox}
            title="Default Variant"
            description="This is the default variant."
            variant="default"
          />
          <.empty_state
            icon={:inbox}
            title="Bordered Variant"
            description="This is the bordered variant."
            variant="bordered"
          />
          <.empty_state
            icon={:inbox}
            title="Card Variant"
            description="This is the card variant."
            variant="card"
          />
        </div>
        """
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :with_custom_styling,
        description: "Empty state with custom CSS classes",
        template: """
        <.empty_state
          icon={:exclamation_triangle}
          title="Custom Style"
          description="This empty state has custom styling applied."
          class="bg-yellow-50 border-yellow-200"
        />
        """
      },
      %Example{
        id: :inline_empty_state,
        description: "Inline empty state for use in smaller containers",
        template: """
        <.inline_empty_state text="No results found" icon={:magnifying_glass} />
        """
      }
    ]
  end
end
