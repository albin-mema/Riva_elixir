# Empty State Component Specification

## Component Name
Empty State

## Description
A component for displaying a consistent empty state experience when no data is available. Used to inform users that there is no content to display and optionally provide a way to add content.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `icon` | atom | nil | Icon to display in the empty state |
| `title` | string | "" | Title for the empty state |
| `description` | string | "" | Description text for the empty state |
| `size` | string | "md" | Size of the empty state (sm, md, lg) |
| `variant` | string | "default" | Style variant (default, bordered, card) |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
| Slot | Required | Description |
|------|----------|-------------|
| `action` | No | Optional action element to display in the empty state |

## Variants
| Variant | Description | CSS Classes |
|---------|-------------|-------------|
| `default` | Standard empty state style | Basic styling with padding |
| `bordered` | Empty state with border | Adds border styling |
| `card` | Empty state styled as a card | Uses card styling with shadow |

## Sizes
| Size | Description | CSS Classes |
|------|-------------|-------------|
| `sm` | Small empty state | Compact padding and smaller text |
| `md` | Medium empty state | Default padding and text size |
| `lg` | Large empty state | Extra padding and larger text |

## CSS Classes and Styling Approach
### Base Classes
The component uses a flex container with centered content and applies size and variant-specific styling.

### CSS Variables Used
- `--foreground` - Text color
- `--muted-foreground` - Color for description text
- `--border` - Border color for bordered variant
- `--card` / `--card-foreground` - Background and text colors for card variant
- `--radius` - Border radius

## Accessibility Considerations
- Semantic HTML structure with appropriate heading levels
- Proper contrast between text and background
- Icons are decorative and hidden from screen readers with aria-hidden
- Focusable elements (if present) have visible focus states
- Text content is readable and provides clear information

## Usage Examples
```heex
<!-- Basic empty state -->
<.empty_state
  icon={:inbox}
  title="No items found"
  description="There are no items to display."
/>

<!-- Empty state with action -->
<.empty_state
  icon={:building_office_2}
  title="No businesses found"
  description="Create your first business to get started"
>
  <:action>
    <button class="px-4 py-2 bg-blue-500 text-white rounded">
      <span class="mr-2">+</span>
      Create Business
    </button>
  </:action>
</.empty_state>

<!-- Empty state with different sizes -->
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

<!-- Empty state with different variants -->
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
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- All sizes and variants are fully responsive
- Consistent with the design system using CSS variables
- Follows accessibility best practices with semantic HTML
- Icon is optional but recommended for better visual communication
- Action slot allows for custom call-to-action elements
- Text content should be clear and helpful to users