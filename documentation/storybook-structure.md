# Atomic Design Structure for Storybook Components

## Directory Organization
```
storybook/
├── atoms/          # Basic UI elements (buttons, inputs, icons)
├── molecules/      # Groups of atoms forming simple components (form fields, cards)
├── organisms/      # Complex UI sections (headers, sidebars, forms)
├── combinations/   # Business-specific component compositions
```

## Component Combinations
- **ReservationCard**: Combines Calendar + Badge + Card with live state management
- **FormWithValidation**: Real-time validation stack with Phoenix LiveView components
- **DataTable**: Paginated data display with interactive controls

## Component Guidelines
1. **Atoms** should be stateless and reusable
2. **Molecules** may contain basic interactivity
3. **Organisms** can manage local state
4. **Combinations** implement business logic

## Example Components
```elixir
# atoms/button.exs
defmodule Storybook.Atoms.Button do
  use PhoenixStorybook.Story, :component
  def component, do: &RivaAshWeb.Components.Atoms.Button.button/1
  # Stories...
end