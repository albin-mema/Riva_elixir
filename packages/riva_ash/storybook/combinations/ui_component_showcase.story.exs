defmodule Storybook.Combinations.UIComponentShowcase do
  use PhoenixStorybook.Story, :component

  def function, do: &__MODULE__.showcase/1

  def doc do
    """
    # UI Component Showcase

    This story demonstrates how canonical UI components work together in real-world scenarios.
    All components shown here use the canonical `RivaAshWeb.Components.UI.*` system.

    ## Featured Components

    - `UI.Card` - Container with consistent styling
    - `UI.Text` - Typography with semantic variants
    - `UI.Button` - Interactive elements with variants and states
    - `UI.Input` - Form inputs with validation states
    - `UI.Badge` - Status indicators and labels
    - `UI.Icon` - Consistent iconography
    - `UI.Spinner` - Loading states

    ## Design System Benefits

    - **Consistent styling** across all components
    - **Single source of truth** for design tokens
    - **Comprehensive prop APIs** with variants, sizes, and states
    - **Built-in accessibility** with proper ARIA attributes
    - **Type safety** with well-defined component interfaces
    """
  end

  def showcase(assigns) do
    ~H"""
    <div class="space-y-8 p-6 max-w-4xl mx-auto">
      <!-- Header Section -->
      <div class="text-center space-y-4">
        <RivaAshWeb.Components.UI.Text.text variant="h1">
          UI Component System
        </RivaAshWeb.Components.UI.Text.text>
        <RivaAshWeb.Components.UI.Text.text variant="lead" color="muted">
          A comprehensive showcase of canonical UI components working together
        </RivaAshWeb.Components.UI.Text.text>
      </div>

      <!-- Button Variants -->
      <RivaAshWeb.Components.UI.Card.card>
        <RivaAshWeb.Components.UI.Card.card_header>
          <RivaAshWeb.Components.UI.Card.card_title>Button Variants</RivaAshWeb.Components.UI.Card.card_title>
          <RivaAshWeb.Components.UI.Card.card_description>
            All button variants with consistent styling and behavior
          </RivaAshWeb.Components.UI.Card.card_description>
        </RivaAshWeb.Components.UI.Card.card_header>
        <RivaAshWeb.Components.UI.Card.card_content>
          <div class="flex flex-wrap gap-3">
            <RivaAshWeb.Components.UI.Button.button variant="default">Default</RivaAshWeb.Components.UI.Button.button>
            <RivaAshWeb.Components.UI.Button.button variant="secondary">Secondary</RivaAshWeb.Components.UI.Button.button>
            <RivaAshWeb.Components.UI.Button.button variant="destructive">Destructive</RivaAshWeb.Components.UI.Button.button>
            <RivaAshWeb.Components.UI.Button.button variant="outline">Outline</RivaAshWeb.Components.UI.Button.button>
            <RivaAshWeb.Components.UI.Button.button variant="ghost">Ghost</RivaAshWeb.Components.UI.Button.button>
            <RivaAshWeb.Components.UI.Button.button variant="link">Link</RivaAshWeb.Components.UI.Button.button>
          </div>
          <div class="flex flex-wrap gap-3 mt-4">
            <RivaAshWeb.Components.UI.Button.button size="sm">Small</RivaAshWeb.Components.UI.Button.button>
            <RivaAshWeb.Components.UI.Button.button size="default">Default</RivaAshWeb.Components.UI.Button.button>
            <RivaAshWeb.Components.UI.Button.button size="lg">Large</RivaAshWeb.Components.UI.Button.button>
            <RivaAshWeb.Components.UI.Button.button loading>Loading</RivaAshWeb.Components.UI.Button.button>
            <RivaAshWeb.Components.UI.Button.button disabled>Disabled</RivaAshWeb.Components.UI.Button.button>
          </div>
        </RivaAshWeb.Components.UI.Card.card_content>
      </RivaAshWeb.Components.UI.Card.card>

      <!-- Form Components -->
      <RivaAshWeb.Components.UI.Card.card>
        <RivaAshWeb.Components.UI.Card.card_header>
          <RivaAshWeb.Components.UI.Card.card_title>Form Components</RivaAshWeb.Components.UI.Card.card_title>
          <RivaAshWeb.Components.UI.Card.card_description>
            Form inputs with consistent styling and validation states
          </RivaAshWeb.Components.UI.Card.card_description>
        </RivaAshWeb.Components.UI.Card.card_header>
        <RivaAshWeb.Components.UI.Card.card_content>
          <div class="space-y-4 max-w-md">
            <div class="space-y-2">
              <RivaAshWeb.Components.UI.Text.text variant="label">Default Input</RivaAshWeb.Components.UI.Text.text>
              <RivaAshWeb.Components.UI.Input.input placeholder="Enter text..." />
            </div>
            <div class="space-y-2">
              <RivaAshWeb.Components.UI.Text.text variant="label">Error State</RivaAshWeb.Components.UI.Text.text>
              <RivaAshWeb.Components.UI.Input.input variant="error" placeholder="Invalid input" />
            </div>
            <div class="space-y-2">
              <RivaAshWeb.Components.UI.Text.text variant="label">Success State</RivaAshWeb.Components.UI.Text.text>
              <RivaAshWeb.Components.UI.Input.input variant="success" placeholder="Valid input" />
            </div>
            <div class="space-y-2">
              <RivaAshWeb.Components.UI.Text.text variant="label">Select Dropdown</RivaAshWeb.Components.UI.Text.text>
              <RivaAshWeb.Components.UI.Select.select 
                options={[{"Option 1", "1"}, {"Option 2", "2"}, {"Option 3", "3"}]}
                prompt="Choose an option..."
              />
            </div>
            <div class="flex items-center space-x-2">
              <RivaAshWeb.Components.UI.Checkbox.checkbox />
              <RivaAshWeb.Components.UI.Text.text variant="label">I agree to the terms</RivaAshWeb.Components.UI.Text.text>
            </div>
          </div>
        </RivaAshWeb.Components.UI.Card.card_content>
      </RivaAshWeb.Components.UI.Card.card>

      <!-- Status and Feedback -->
      <RivaAshWeb.Components.UI.Card.card>
        <RivaAshWeb.Components.UI.Card.card_header>
          <RivaAshWeb.Components.UI.Card.card_title>Status & Feedback</RivaAshWeb.Components.UI.Card.card_title>
          <RivaAshWeb.Components.UI.Card.card_description>
            Badges, icons, and loading states for user feedback
          </RivaAshWeb.Components.UI.Card.card_description>
        </RivaAshWeb.Components.UI.Card.card_header>
        <RivaAshWeb.Components.UI.Card.card_content>
          <div class="space-y-4">
            <div class="flex flex-wrap gap-2">
              <RivaAshWeb.Components.UI.Badge.badge variant="default">Default</RivaAshWeb.Components.UI.Badge.badge>
              <RivaAshWeb.Components.UI.Badge.badge variant="secondary">Secondary</RivaAshWeb.Components.UI.Badge.badge>
              <RivaAshWeb.Components.UI.Badge.badge variant="success">Success</RivaAshWeb.Components.UI.Badge.badge>
              <RivaAshWeb.Components.UI.Badge.badge variant="warning">Warning</RivaAshWeb.Components.UI.Badge.badge>
              <RivaAshWeb.Components.UI.Badge.badge variant="destructive">Error</RivaAshWeb.Components.UI.Badge.badge>
              <RivaAshWeb.Components.UI.Badge.badge variant="outline">Outline</RivaAshWeb.Components.UI.Badge.badge>
            </div>
            <div class="flex items-center gap-4">
              <div class="flex items-center gap-2">
                <RivaAshWeb.Components.UI.Icon.icon name={:check} size="sm" class="text-green-600" />
                <RivaAshWeb.Components.UI.Text.text variant="small">Success</RivaAshWeb.Components.UI.Text.text>
              </div>
              <div class="flex items-center gap-2">
                <RivaAshWeb.Components.UI.Icon.icon name={:x_mark} size="sm" class="text-red-600" />
                <RivaAshWeb.Components.UI.Text.text variant="small">Error</RivaAshWeb.Components.UI.Text.text>
              </div>
              <div class="flex items-center gap-2">
                <RivaAshWeb.Components.UI.Spinner.spinner size="sm" />
                <RivaAshWeb.Components.UI.Text.text variant="small">Loading</RivaAshWeb.Components.UI.Text.text>
              </div>
            </div>
          </div>
        </RivaAshWeb.Components.UI.Card.card_content>
      </RivaAshWeb.Components.UI.Card.card>

      <!-- Typography Scale -->
      <RivaAshWeb.Components.UI.Card.card>
        <RivaAshWeb.Components.UI.Card.card_header>
          <RivaAshWeb.Components.UI.Card.card_title>Typography Scale</RivaAshWeb.Components.UI.Card.card_title>
          <RivaAshWeb.Components.UI.Card.card_description>
            Consistent typography with semantic HTML elements
          </RivaAshWeb.Components.UI.Card.card_description>
        </RivaAshWeb.Components.UI.Card.card_header>
        <RivaAshWeb.Components.UI.Card.card_content>
          <div class="space-y-4">
            <RivaAshWeb.Components.UI.Text.text variant="h1">Heading 1</RivaAshWeb.Components.UI.Text.text>
            <RivaAshWeb.Components.UI.Text.text variant="h2">Heading 2</RivaAshWeb.Components.UI.Text.text>
            <RivaAshWeb.Components.UI.Text.text variant="h3">Heading 3</RivaAshWeb.Components.UI.Text.text>
            <RivaAshWeb.Components.UI.Text.text variant="h4">Heading 4</RivaAshWeb.Components.UI.Text.text>
            <RivaAshWeb.Components.UI.Text.text variant="p">
              This is a paragraph with regular body text. It demonstrates the default text styling
              and line height for readable content.
            </RivaAshWeb.Components.UI.Text.text>
            <RivaAshWeb.Components.UI.Text.text variant="lead">
              This is lead text that stands out from regular paragraphs.
            </RivaAshWeb.Components.UI.Text.text>
            <RivaAshWeb.Components.UI.Text.text variant="small" color="muted">
              This is small, muted text for captions and secondary information.
            </RivaAshWeb.Components.UI.Text.text>
          </div>
        </RivaAshWeb.Components.UI.Card.card_content>
      </RivaAshWeb.Components.UI.Card.card>
    </div>
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{}
      }
    ]
  end
end
