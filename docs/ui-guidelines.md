# UI Guidelines

## Canonical UI components and deprecations

To ensure consistency and avoid duplication, the canonical primitives are defined under:

- RivaAshWeb.Components.UI.* — Design-system primitives (e.g., UI.Button, UI.Card)

Compatibility wrappers exist in other layers to preserve backwards compatibility while the codebase migrates to the canonical components:

- Atoms
  - RivaAshWeb.Components.Atoms.Button delegates to RivaAshWeb.Components.UI.Button
  - Continue using UI.Button directly for new code; Atoms.Button will be removed after migration

- Molecules
  - RivaAshWeb.Components.Molecules.Card uses UI.Card as the container while preserving the molecule’s header/body/footer API for composition
  - Prefer UI.Card for low-level layout; use Molecules.Card for page composition where header/body/footer slots are desired

## Migration Guidance

### For New Code
Always use canonical UI components directly:

```elixir
# ✅ Preferred - Use UI components directly
alias RivaAshWeb.Components.UI.Button, as: UIButton
alias RivaAshWeb.Components.UI.Input, as: UIInput

def my_component(assigns) do
  ~H"""
  <UIButton.button variant="primary" size="lg">
    Save Changes
  </UIButton.button>
  <UIInput.input placeholder="Enter text" />
  """
end
```

### For Existing Code
Keep existing atom component calls as-is during migration:

```elixir
# ✅ Acceptable during migration - Compatibility wrapper handles delegation
import RivaAshWeb.Components.Atoms.Button

def existing_component(assigns) do
  ~H"""
  <.button variant="primary" size="md">
    Save Changes
  </.button>
  """
end
```

### Component Import Patterns

**Preferred for new code:**
```elixir
# Import UI components with aliases to avoid conflicts
alias RivaAshWeb.Components.UI.Button, as: UIButton
alias RivaAshWeb.Components.UI.Input, as: UIInput
alias RivaAshWeb.Components.UI.Card, as: UICard
```

**Legacy pattern (being phased out):**
```elixir
# Old pattern - still works but deprecated
import RivaAshWeb.Components.Atoms.Button
import RivaAshWeb.Components.Atoms.Input
```

### Size Mapping for Compatibility Wrappers

Atom components map legacy sizes to UI component sizes:

| Legacy Size | UI Size | Description |
|-------------|---------|-------------|
| `sm` | `sm` | Small variant |
| `md` | `default` | Medium becomes default |
| `lg` | `lg` | Large variant |

### Molecule Component Guidelines

Molecule components should use UI components internally while preserving their composed APIs:

```elixir
# ✅ Good - Molecule uses UI components internally
defmodule MyApp.Components.Molecules.FormField do
  alias RivaAshWeb.Components.UI.Input, as: UIInput
  alias RivaAshWeb.Components.UI.Text, as: UIText

  def form_field(assigns) do
    ~H"""
    <div class="space-y-2">
      <UIText.text variant="label"><%= @label %></UIText.text>
      <UIInput.input field={@field} placeholder={@placeholder} />
    </div>
    """
  end
end
```

### Storybook Organization

- **UI Component Stories**: Point to canonical UI components for primitives
- **Molecule Stories**: Showcase composed patterns using UI components internally
- **Example Stories**: Show real-world usage combining multiple components

## Rationale

- **Single source of truth** for styling and interaction
- **Reduced drift** between design tokens and implementations
- **Simpler testing surface** focusing on UI primitives
- **Backward compatibility** during migration period
- **Consistent design system** across the entire application

## Accessibility and testing

- Provide ARIA roles and labels on interactive components
- Add component tests for UI.Button and Molecules.Card covering:
  - Variants, sizes, loading/disabled states
  - Slot content rendering (Card header/body/footer)
- Use phoenix_test for LiveView interactions and StreamData for prop value generation where applicable

## UI Component Architecture

**Atomic Design Pattern**: All UI components follow atomic design:
- **Atoms**: `lib/riva_ash_web/components/atoms/` (Button, Input, Text, etc.)
- **Molecules**: `lib/riva_ash_web/components/molecules/` (Card, FormField, etc.)
- **Organisms**: `lib/riva_ash_web/components/organisms/` (DataTable, CalendarView, etc.)

**Custom Components**: Always create reusable custom components instead of inline HTML.

**Flop Integration**: Use Flop library for ALL table functionality and pagination.

**Form Handling**: Use AshPhoenix.Form for all form operations with proper validation.

## UI Development

**LiveView First**: Prefer LiveView over React components unless specific interactivity is needed.

**Storybook**: Use `phoenix_storybook` for component documentation and development.

**Styling**: Use Tailwind CSS exclusively - no custom CSS.

**Tables**: Always use Flop library for table functionality.

## Component Patterns
- **Stateful Combinations**: Use LiveView to manage state across multiple atomic components
- **Validation Flows**: Implement real-time validation using Phoenix form handling
- **Paginated Data**: Combine tables with pagination controls for large datasets

## Common Patterns

### 3. UI Component Creation

1. Follow atomic design hierarchy
2. Create in appropriate component directory
3. Add to Storybook
4. **MANDATORY**: Comprehensive test suite:
   - Property-based tests for component props with random valid values
   - Interaction tests using `phoenix_test`
   - Accessibility tests for proper ARIA attributes
   - Responsive design tests across different screen sizes
   - Form validation tests with random invalid inputs
   - LiveView event handling tests
5. Document props and usage in Storybook

## Debugging and Development Tools

### 1. Available Tools
- **AshAdmin**: Web-based admin interface at `/admin`
- **GraphQL Playground**: Available for API exploration
- **LiveView Debugger**: Use for LiveView debugging
- **Ash Console**: `iex -S mix` for interactive development

## Troubleshooting Common Issues

### 2. LiveView Issues
- Check socket connections
- Verify proper assigns usage
- Debug with LiveView debugger
- Test with different browsers