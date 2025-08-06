# Badge Component Specification

## Component Name
Badge

## Description
A badge component for displaying status, labels, or categorical information. Used for showing counts, statuses, or tags in a compact format.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `variant` | string | "default" | The badge style variant (default, secondary, destructive, outline) |
| `size` | string | "default" | The badge size (default, sm, lg) |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
| Slot | Required | Description |
|------|----------|-------------|
| `inner_block` | Yes | The content to display inside the badge |

## Variants
| Variant | Description | CSS Classes |
|---------|-------------|-------------|
| `default` | Primary badge style | `bg-primary text-primary-foreground` |
| `secondary` | Secondary badge style | `bg-secondary text-secondary-foreground` |
| `destructive` | Destructive badge style | `bg-destructive text-destructive-foreground` |
| `outline` | Outlined badge style | `text-foreground` |

## Sizes
| Size | Description | CSS Classes |
|------|-------------|-------------|
| `default` | Standard badge size | `px-2.5 py-0.5 text-xs` |
| `sm` | Small badge size | `px-2 py-0.5 text-xs` |
| `lg` | Large badge size | `px-3 py-1 text-sm` |

## CSS Classes and Styling Approach
### Base Classes
```
inline-flex items-center rounded-full border px-2.5 py-0.5 text-xs font-semibold transition-colors focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2
```

### CSS Variables Used
- `--primary` / `--primary-foreground` - Primary badge colors
- `--secondary` / `--secondary-foreground` - Secondary badge colors
- `--destructive` / `--destructive-foreground` - Destructive badge colors
- `--foreground` - Text color for outline variant
- `--ring` - Focus ring color
- `--radius` - Border radius (used via rounded-full class)

## Accessibility Considerations
- Proper focus states with visible focus ring
- Semantic div element with appropriate attributes
- ARIA attributes can be passed through via `rest` attribute
- Text content should be meaningful and concise
- Sufficient color contrast for all variants
- Font size is appropriate for readability

## Usage Examples
```heex
<!-- Default badge -->
<Badge>New</Badge>

<!-- Secondary badge -->
<Badge variant="secondary">Updated</Badge>

<!-- Destructive badge -->
<Badge variant="destructive">Deleted</Badge>

<!-- Outline badge -->
<Badge variant="outline">Draft</Badge>

<!-- Small badge -->
<Badge size="sm">Beta</Badge>

<!-- Large badge -->
<Badge size="lg">Important</Badge>

<!-- Badge with icon -->
<Badge>
  <Icon name={:check} class="mr-1 h-3 w-3" />
  Verified
</Badge>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- All variants and sizes are fully responsive
- Consistent with the design system using CSS variables
- Follows accessibility best practices with proper focus states
- Content is passed via inner block for flexibility
- Rounded-full gives pill-shaped appearance
- Font weight is semibold for better readability
- Border is present for all variants but only visible for outline variant