# Button Component Specification

## Component Name
Button

## Description
A versatile button component that supports multiple variants, sizes, and states. Used for triggering actions in forms, dialogs, and general UI interactions.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `type` | string | "button" | The button type (button, submit, reset) |
| `variant` | string | "default" | The button style variant (default, destructive, outline, secondary, ghost, link) |
| `size` | string | "default" | The button size (default, sm, lg) |
| `loading` | boolean | false | Whether the button is in a loading state |
| `disabled` | boolean | false | Whether the button is disabled |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
| Slot | Required | Description |
|------|----------|-------------|
| `inner_block` | Yes | The content to display inside the button |

## Variants
| Variant | Description | CSS Classes |
|---------|-------------|-------------|
| `default` | Primary action button | `bg-primary text-primary-foreground hover:bg-primary/90` |
| `destructive` | Destructive action button | `bg-destructive text-destructive-foreground hover:bg-destructive/90` |
| `outline` | Outlined button | `border border-input hover:bg-accent hover:text-accent-foreground` |
| `secondary` | Secondary action button | `bg-secondary text-secondary-foreground hover:bg-secondary/80` |
| `ghost` | Minimal button style | `hover:bg-accent hover:text-accent-foreground` |
| `link` | Link-style button | `underline-offset-4 hover:underline text-primary` |

## Sizes
| Size | Description | CSS Classes |
|------|-------------|-------------|
| `default` | Standard button size | `h-10 py-2 px-4` |
| `sm` | Small button size | `h-9 px-3 rounded-md` |
| `lg` | Large button size | `h-11 px-8 rounded-md` |

## CSS Classes and Styling Approach
### Base Classes
```
inline-flex items-center justify-center rounded-md text-sm font-medium transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:opacity-50 disabled:pointer-events-none ring-offset-background
```

### Loading State
When `loading` is true, adds `opacity-50 pointer-events-none` classes and renders a spinner.

### CSS Variables Used
- `--primary` / `--primary-foreground` - Primary button colors
- `--secondary` / `--secondary-foreground` - Secondary button colors
- `--destructive` / `--destructive-foreground` - Destructive button colors
- `--accent` / `--accent-foreground` - Accent colors for hover states
- `--input` - Border color for outline variant
- `--ring` - Focus ring color
- `--radius` - Border radius (used via rounded-md class)

## Accessibility Considerations
- Proper focus states with visible focus ring
- Disabled buttons have `disabled:opacity-50 disabled:pointer-events-none` to prevent interaction
- Loading state prevents interaction with `opacity-50 pointer-events-none`
- Semantic button element with appropriate type attribute
- ARIA attributes are automatically handled by the browser for standard button elements

## Usage Examples
```heex
<!-- Primary button -->
<Button>Save Changes</Button>

<!-- Secondary button -->
<Button variant="secondary">Cancel</Button>

<!-- Destructive button -->
<Button variant="destructive">Delete</Button>

<!-- Outline button -->
<Button variant="outline">Edit Profile</Button>

<!-- Small button -->
<Button size="sm">Small Button</Button>

<!-- Loading button -->
<Button loading>Loading...</Button>

<!-- Disabled button -->
<Button disabled>Disabled</Button>

<!-- Button with icon -->
<Button>
  <Icon name={:plus} class="mr-2 h-4 w-4" />
  Add Item
</Button>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Implements proper form field integration
- Supports global attributes for Phoenix LiveView events
- Loading state includes an animated spinner SVG
- All variants and sizes are fully responsive
- Consistent with the design system using CSS variables
- Follows accessibility best practices with proper focus states