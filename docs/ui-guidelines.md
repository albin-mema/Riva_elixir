# UI Guidelines

## Canonical UI components and deprecations

To ensure consistency and avoid duplication, the canonical primitives are defined under:

- RivaAshWeb.Components.UI.* — Design-system primitives (e.g., UI.Button, UI.Card)

**MANDATORY**: All new UI code MUST use the RivaAshWeb.Components.UI.* namespace. Direct usage of Atoms.* or other legacy component namespaces is prohibited for new code.

Compatibility wrappers exist in other layers to preserve backwards compatibility while the codebase migrates to the canonical components:

- Atoms
  - RivaAshWeb.Components.Atoms.Button delegates to RivaAshWeb.Components.UI.Button
  - Atoms.* components are DEPRECATED and will be removed in the next major version release
  - New code MUST NOT use Atoms.* components

- Molecules
  - RivaAshWeb.Components.Molecules.Card uses UI.Card as the container while preserving the molecule's header/body/footer API for composition
  - Prefer UI.Card for low-level layout; use Molecules.Card for page composition where header/body/footer slots are desired

## Migration Path: Atoms.* → UI.*

### Phase 1: New Code (Immediate)
- All new UI code MUST use RivaAshWeb.Components.UI.* components
- No new code should reference Atoms.* components
- Credo rules will flag new Atoms.* usage as errors

### Phase 2: Component Migration (Q3 2025)
- Atoms.* components will be updated to delegate to UI.* components
- Atoms.* modules will be marked as deprecated with warnings
- Documentation will be updated to reflect migration path

### Phase 3: Removal (Q1 2026)
- Atoms.* components will be removed from the codebase
- All remaining references must be updated to UI.* components

## Migration Guidance

### For New Code
MUST use canonical UI components directly:

```elixir
# ✅ REQUIRED - Use UI components directly
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
Existing atom component calls should be migrated to UI.* components:

```elixir
# ✅ MIGRATED - Using UI components
alias RivaAshWeb.Components.UI.Button, as: UIButton

def existing_component(assigns) do
  ~H"""
  <UIButton.button variant="primary" size="md">
    Save Changes
  </UIButton.button>
  """
end
```

```elixir
# ⚠️ DEPRECATED - Compatibility wrapper (will be removed)
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

**REQUIRED for new code:**
```elixir
# Import UI components with aliases to avoid conflicts
alias RivaAshWeb.Components.UI.Button, as: UIButton
alias RivaAshWeb.Components.UI.Input, as: UIInput
alias RivaAshWeb.Components.UI.Card, as: UICard
```

**Deprecated pattern:**
```elixir
# Old pattern - prohibited for new code
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

## Wrapper Strategy and Property-Based Testing (PBT)

- Stable app API: All UI components must be thin wrappers over third‑party libs (currently SaladUI), exposing only the minimal, app‑level API
- Opinionated defaults: Enforce accessibility and design tokens in the wrapper (aria-* states, focus-visible, disabled semantics)
- Passthrough: Provide a :rest (global) passthrough for advanced attributes; avoid mirroring vendor APIs 1:1
- Validation: Use `attr` validations and fail fast in dev/test on impossible combos
- Tokens mapping: Keep class/token mapping centralized in helpers inside the wrapper (e.g., classes_for/.. when needed)

### Testing requirements (per component)
- Unit tests: Basic rendering, variants, sizes, disabled/loading states, global attributes
- Property tests: Use StreamData to generate valid prop maps; assert crash‑free rendering and invariants (e.g., aria-busy when loading, disabled when loading)
- Visual/a11y: Small Playwright smoke that renders the wrapper and checks a couple of a11y attributes; screenshot optional

### Migration (SaladUI)
- Wrap atoms first (Button, Input, Icon/Spinner, Modal, Tooltip)
- UI.* components delegate to SaladUI primitives and use `TwMerge` to merge default + user classes
- Detect and progressively replace direct vendor usage; prefer `RivaAshWeb.Components.UI.*` in app code


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