# Icon Component Specification

## Component Name
Icon

## Description
An icon component that provides a consistent interface for rendering SVG icons. Supports various variants, sizes, and styling options.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `name` | atom | - | Required. The name of the icon to render |
| `variant` | string | "outline" | The icon variant (outline, solid, mini, micro) |
| `size` | string | "md" | The icon size (xs, sm, md, lg, xl) |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Variants
| Variant | Description |
|---------|-------------|
| `outline` | Outline style icons |
| `solid` | Solid fill icons |
| `mini` | Compact icons |
| `micro` | Ultra-compact icons |

## Sizes
| Size | Description | CSS Classes |
|------|-------------|-------------|
| `xs` | Extra small icon | `[&>svg]:h-3 [&>svg]:w-3` |
| `sm` | Small icon | `[&>svg]:h-4 [&>svg]:w-4` |
| `md` | Medium icon | `[&>svg]:h-5 [&>svg]:w-5` |
| `lg` | Large icon | `[&>svg]:h-6 [&>svg]:w-6` |
| `xl` | Extra large icon | `[&>svg]:h-8 [&>svg]:w-8` |

## CSS Classes and Styling Approach
### Base Container Classes
```
inline-block
```

### SVG Classes
Determined by size property:
- `xs`: `[&>svg]:h-3 [&>svg]:w-3`
- `sm`: `[&>svg]:h-4 [&>svg]:w-4`
- `md`: `[&>svg]:h-5 [&>svg]:w-5`
- `lg`: `[&>svg]:h-6 [&>svg]:w-6`
- `xl`: `[&>svg]:h-8 [&>svg]:w-8`

### CSS Variables Used
- No specific CSS variables are directly used in this component

## Accessibility Considerations
- Semantic span element with appropriate attributes
- ARIA attributes can be passed through via `rest` attribute
- Icons should have meaningful labels when used without text
- Icons used for interaction should be focusable and have proper roles
- Decorative icons should be hidden from screen readers with aria-hidden="true"
- Color contrast should be sufficient when icons are used with text

## Usage Examples
```heex
<!-- Basic icon -->
<Icon name={:home} />

<!-- Icon with custom size -->
<Icon name={:cog} size="lg" />

<!-- Icon with custom color -->
<Icon name={:check} class="text-green-500" />

<!-- Icon with click handler -->
<Icon name={:x_mark} size="sm" phx-click="close-modal" class="cursor-pointer" />

<!-- Icon in button -->
<Button>
  <Icon name={:plus} class="mr-2 h-4 w-4" />
  Add Item
</Button>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- All sizes are fully responsive
- Consistent with the design system
- Follows accessibility best practices
- Uses Heroicons library for SVG paths
- Icon names are atoms for performance and type safety
- SVG elements have proper viewBox and fill attributes
- Container is inline-block for proper alignment with text
- Size classes use [&>svg] selector to target the SVG element directly
- Supports all common Heroicons with appropriate path definitions
- Variant support allows for different icon styles where available