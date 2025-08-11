# Tooltip Component Specification

## Component Name
Tooltip

## Description
A tooltip component for displaying help text or additional information on hover or focus. Supports different positions and trigger methods.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `content` | string | - | Required. The tooltip content to display |
| `position` | string | "top" | The tooltip position (top, bottom, left, right) |
| `trigger` | string | "hover" | The trigger method (hover, click, focus) |
| `delay` | integer | 200 | Delay in milliseconds before showing tooltip |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
| Slot | Required | Description |
|------|----------|-------------|
| `inner_block` | Yes | The trigger element for the tooltip |

## Positions
| Position | Description | CSS Classes |
|----------|-------------|-------------|
| `top` | Tooltip above trigger | Top positioning classes |
| `bottom` | Tooltip below trigger | Bottom positioning classes |
| `left` | Tooltip to the left of trigger | Left positioning classes |
| `right` | Tooltip to the right of trigger | Right positioning classes |

## Triggers
| Trigger | Description |
|---------|-------------|
| `hover` | Show tooltip on hover |
| `click` | Show tooltip on click |
| `focus` | Show tooltip on focus |

## CSS Classes and Styling Approach
### Base Container Classes
```
relative inline-block
```

### Tooltip Classes
```
absolute z-50 px-3 py-1.5 text-sm text-foreground bg-popover rounded-md border border-border shadow-md
```

### Position Classes
- Top: `bottom-full left-1/2 transform -translate-x-1/2 -translate-y-2 mb-2`
- Bottom: `top-full left-1/2 transform -translate-x-1/2 translate-y-2 mt-2`
- Left: `right-full top-1/2 transform -translate-y-1/2 -translate-x-2 mr-2`
- Right: `left-full top-1/2 transform -translate-y-1/2 translate-x-2 ml-2`

### Arrow Classes
```
absolute w-2 h-2 bg-popover border border-border rotate-45
```

### CSS Variables Used
- `--foreground` - Text color
- `--popover` - Background color
- `--border` - Border color
- `--radius` - Border radius (used via rounded-md class)
- `--shadow-md` - Box shadow (used via shadow-md class)

## Accessibility Considerations
- Proper ARIA attributes for tooltip relationship
- Tooltips should be accessible via keyboard focus
- Delay should be appropriate for users with motor impairments
- Content should be concise and informative
- Should not contain interactive elements
- Should be hidden when trigger loses focus/hover
- ARIA attributes can be passed through via `rest` attribute
- Should respect prefers-reduced-motion
- Should work with screen readers

## Usage Examples
```heex
<!-- Basic tooltip -->
<Tooltip content="This is helpful information">
  <Button>Hover me</Button>
</Tooltip>

<!-- Tooltip with custom position -->
<Tooltip content="Information below" position="bottom">
  <Icon name={:info} class="text-muted-foreground" />
</Tooltip>

<!-- Tooltip with click trigger -->
<Tooltip content="Click to learn more" trigger="click">
  <span class="underline cursor-help">Learn more</span>
</Tooltip>

<!-- Tooltip with focus trigger -->
<Tooltip content="Username requirements" trigger="focus">
  <Input placeholder="Username" />
</Tooltip>

<!-- Tooltip with custom delay -->
<Tooltip content="Loading..." delay={500}>
  <Spinner />
</Tooltip>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- All positions are fully responsive
- Consistent with the design system using CSS variables
- Follows accessibility best practices with proper ARIA attributes
- Trigger element is passed via inner block for flexibility
- Content prop contains the tooltip text
- Position prop controls where tooltip appears relative to trigger
- Trigger prop controls how tooltip is activated
- Delay prop controls timing of tooltip appearance
- Should have smooth fade in/out animations
- Should handle positioning near viewport edges
- Arrow element should match tooltip background and border
- Should be hidden from screen readers when not visible
- Should properly clean up event listeners
- Should work with disabled elements (via wrapper)
- Should handle overflow and text wrapping appropriately