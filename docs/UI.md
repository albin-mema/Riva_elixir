# UI Guidelines

## Component Architecture

### Canonical Components (Use These)
- **Location**: `RivaAshWeb.Components.UI.*`
- **Status**: Mandatory for all new code
- **Styling**: Tailwind + design tokens
- **Testing**: Unit + property tests required

### Deprecated Components (Don't Use)
- **Location**: `RivaAshWeb.Components.Atoms.*`
- **Status**: Legacy wrappers exist but avoid in new code
- **Migration**: Use canonical equivalents

### Component Hierarchy
```
UI (Atoms)
├── Alert, Avatar, Badge, Button, Card, Checkbox
├── Icon, Input, Label, Link, Select, Spinner
├── Text, Textarea, Tooltip, Progress
└── Container, Divider

Molecules (Combinations)
├── SearchBar, UserProfile, CardWithActions
├── DataTableHeader, FormField
└── NavigationBreadcrumb

Organisms (Complex)
├── DashboardStats, DataTable, FilterPanel
├── PageHeader, NavigationSidebar
└── ReservationCalendar
```

## Design System

### Colors (OKLCH)
```css
/* Primary */
--primary: 0.6 0.15 250;
--primary-foreground: 0.98 0.02 250;

/* Secondary */
--secondary: 0.96 0.02 250;
--secondary-foreground: 0.45 0.1 250;

/* Accent */
--accent: 0.85 0.1 180;
--accent-foreground: 0.2 0.05 180;

/* Status */
--success: 0.7 0.15 140;
--warning: 0.8 0.15 80;
--error: 0.65 0.2 25;
--info: 0.7 0.15 220;
```

### Typography
- **Font**: Inter (system fallback)
- **Scales**: text-xs to text-4xl
- **Weights**: font-normal, font-medium, font-semibold
- **Line Heights**: leading-tight, leading-normal, leading-relaxed

### Spacing
- **Scale**: 0.5, 1, 1.5, 2, 2.5, 3, 4, 5, 6, 8, 10, 12, 16, 20, 24
- **Usage**: Consistent spacing using Tailwind scale
- **Responsive**: Use responsive prefixes (sm:, md:, lg:)

## Component Usage

### Basic Components
```heex
<!-- Alert -->
<.alert variant="success" title="Success">
  Operation completed successfully
</.alert>

<!-- Button -->
<.button variant="primary" size="md" disabled={false}>
  Save Changes
</.button>

<!-- Card -->
<.card>
  <.card_header>
    <.card_title>Title</.card_title>
    <.card_description>Description</.card_description>
  </.card_header>
  <.card_content>Content</.card_content>
  <.card_footer>Footer</.card_footer>
</.card>

<!-- Input -->
<.input
  type="text"
  name="email"
  placeholder="Enter email"
  required
  errors={@form[:email].errors}
/>
```

### Form Patterns
```heex
<!-- Standard form field -->
<.form_field>
  <.label for="name">Name</.label>
  <.input type="text" name="name" id="name" />
  <.error :for={error <- @form[:name].errors}>{error}</.error>
</.form_field>

<!-- With AshPhoenix.Form -->
<.simple_form for={@form} phx-submit="save">
  <.input field={@form[:name]} label="Name" />
  <.input field={@form[:email]} label="Email" type="email" />
  <:actions>
    <.button type="submit">Save</.button>
  </:actions>
</.simple_form>
```

### Data Display
```heex
<!-- Table with Flop -->
<.data_table
  items={@reservations}
  meta={@meta}
  path={~p"/reservations"}
>
  <:col :let={reservation} label="Client" field={:client_name}>
    {reservation.client_name}
  </:col>
  <:col :let={reservation} label="Date" field={:date}>
    {Calendar.strftime(reservation.date, "%Y-%m-%d")}
  </:col>
  <:action :let={reservation}>
    <.link navigate={~p"/reservations/#{reservation.id}"}>
      View
    </.link>
  </:action>
</.data_table>
```

## Testing Guidelines

### Unit Tests
```elixir
defmodule RivaAshWeb.Components.UI.ButtonTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.LiveViewTest

  test "renders with default props" do
    html = render_component(&RivaAshWeb.Components.UI.button/1, %{})
    assert html =~ "button"
    assert html =~ "bg-primary"
  end

  test "applies variant classes" do
    html = render_component(&RivaAshWeb.Components.UI.button/1, %{variant: "secondary"})
    assert html =~ "bg-secondary"
  end
end
```

### Property Tests
```elixir
defmodule RivaAshWeb.Components.UI.ButtonPropertyTest do
  use RivaAshWeb.ConnCase, async: true
  use ExUnitProperties
  import Phoenix.LiveViewTest

  property "renders with any valid variant" do
    check all variant <- member_of(["primary", "secondary", "outline", "ghost"]),
              size <- member_of(["sm", "md", "lg"]) do
      
      html = render_component(&RivaAshWeb.Components.UI.button/1, %{
        variant: variant,
        size: size
      })
      
      assert html =~ "button"
      assert html =~ variant
      assert html =~ size
    end
  end
end
```

## Accessibility

### Requirements
- **ARIA Labels**: All interactive elements
- **Focus Management**: Visible focus indicators
- **Keyboard Navigation**: Tab order and shortcuts
- **Screen Readers**: Semantic HTML and ARIA
- **Color Contrast**: WCAG AA compliance

### Implementation
```heex
<!-- Accessible button -->
<.button
  aria-label="Save document"
  aria-describedby="save-help"
  disabled={@saving}
>
  <.icon name="save" aria-hidden="true" />
  {if @saving, do: "Saving...", else: "Save"}
</.button>

<!-- Accessible form -->
<.input
  type="email"
  name="email"
  aria-required="true"
  aria-invalid={@form[:email].errors != []}
  aria-describedby="email-error"
/>
<div id="email-error" role="alert">
  <.error :for={error <- @form[:email].errors}>{error}</.error>
</div>
```

## Migration Guide

### From Atoms to UI Components

#### 1. Identify Usage
```bash
# Find all Atoms usage
grep -r "Atoms\." lib/riva_ash_web/
```

#### 2. Replace Components
```heex
<!-- Before -->
<Atoms.button variant="primary">Save</Atoms.button>

<!-- After -->
<.button variant="primary">Save</.button>
```

#### 3. Update Imports
```elixir
# Before
import RivaAshWeb.Components.Atoms

# After  
import RivaAshWeb.Components.UI
```

#### 4. Test Changes
- Run existing tests
- Add property tests for new components
- Verify accessibility compliance

### Migration Checklist
- [ ] Audit current component usage
- [ ] Create UI component equivalents
- [ ] Update all references
- [ ] Add comprehensive tests
- [ ] Verify accessibility
- [ ] Update Storybook stories
- [ ] Remove deprecated Atoms

## Storybook

### Component Stories
```elixir
# stories/button_stories.exs
defmodule Storybook.Components.Button do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.UI.button/1

  def variations do
    [
      %Variation{
        id: :primary,
        attributes: %{variant: "primary"},
        slots: ["Primary Button"]
      },
      %Variation{
        id: :secondary,
        attributes: %{variant: "secondary"},
        slots: ["Secondary Button"]
      }
    ]
  end
end
```

### Running Storybook
```bash
# Development
mix phx.server
# Visit http://localhost:4000/storybook

# Build static
mix storybook.build
```

## Performance

### Optimization
- **CSS Purging**: Tailwind removes unused styles
- **Component Caching**: Phoenix.Component caches renders
- **Lazy Loading**: Use phx-viewport for large lists
- **Image Optimization**: WebP format, responsive sizes

### Monitoring
- **Core Web Vitals**: LCP, FID, CLS
- **Bundle Size**: Monitor JavaScript payload
- **Render Performance**: Phoenix LiveDashboard
- **User Experience**: Real user monitoring
