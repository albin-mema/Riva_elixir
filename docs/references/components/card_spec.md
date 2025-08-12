# Card Component Specification

## Component Name
Card

## Description
A flexible container component for grouping related content. Used for displaying information in a structured, visually distinct way.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
| Slot | Required | Description |
|------|----------|-------------|
| `inner_block` | Yes | The content to display inside the card |

## CSS Classes and Styling Approach
### Base Classes
```
rounded-lg border bg-card text-card-foreground shadow-sm
```

### CSS Variables Used
- `--card` / `--card-foreground` - Background and text colors
- `--border` - Border color
- `--radius` - Border radius (used via rounded-lg class)
- `--shadow-sm` - Box shadow (used via shadow-sm class)

## Accessibility Considerations
- Semantic div element with appropriate attributes
- ARIA attributes can be passed through via `rest` attribute
- Proper heading structure within card content
- Sufficient color contrast for text and background
- Focus management for interactive elements within cards

## Usage Examples
```heex
<!-- Basic card -->
<Card>
  <p>This is a simple card with some content.</p>
</Card>

<!-- Card with multiple elements -->
<Card>
  <h3 class="text-lg font-semibold">Card Title</h3>
  <p class="mt-2">This is the card content.</p>
  <div class="mt-4">
    <Button>Get Started</Button>
  </div>
</Card>

<!-- Card with custom styling -->
<Card class="max-w-md mx-auto">
  <div class="p-6">
    <h3 class="text-xl font-bold">Welcome</h3>
    <p class="mt-2">This is a centered card with custom width.</p>
  </div>
</Card>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Fully responsive design
- Consistent with the design system using CSS variables
- Content is passed via inner block for maximum flexibility
- Rounded-lg gives appropriate border radius
- Shadow-sm provides subtle elevation
- Border uses --border CSS variable for consistent styling
- Background uses --card and text uses --card-foreground for proper contrast