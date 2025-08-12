# Spinner Component Specification

## Component Name
Spinner

## Description
A loading spinner component with various sizes and styles. Used to indicate loading or processing states in the UI.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `size` | string | "md" | The spinner size (xs, sm, md, lg, xl) |
| `variant` | string | "default" | The spinner style variant (default, primary, secondary) |
| `label` | string | "Loading..." | The label text for screen readers |
| `show_label` | boolean | false | Whether to show the label visually |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Variants
| Variant | Description | CSS Classes |
|---------|-------------|-------------|
| `default` | Standard spinner style | `text-primary` |
| `primary` | Primary color spinner | `text-primary` |
| `secondary` | Secondary color spinner | `text-secondary` |

## Sizes
| Size | Description | CSS Classes (Spinner) | CSS Classes (Label) |
|------|-------------|-----------------------|---------------------|
| `xs` | Extra small spinner | `h-3 w-3` | `text-xs` |
| `sm` | Small spinner | `h-4 w-4` | `text-sm` |
| `md` | Medium spinner | `h-5 w-5` | `text-sm` |
| `lg` | Large spinner | `h-6 w-6` | `text-base` |
| `xl` | Extra large spinner | `h-8 w-8` | `text-lg` |

## CSS Classes and Styling Approach
### Container Classes
```
inline-flex items-center gap-2
```

### Spinner Classes
```
text-primary
```
(Plus size-specific classes for height and width)

### SVG Classes
```
w-full h-full animate-spin
```

### Circle Classes
```
opacity-25
```

### Path Classes
```
opacity-75
```

### Label Classes (when shown)
Determined by size property:
- `xs`: `text-xs`
- `sm`: `text-sm`
- `md`: `text-sm`
- `lg`: `text-base`
- `xl`: `text-lg`

### CSS Variables Used
- `--primary` - Primary color for default/primary variant
- `--secondary` - Secondary color for secondary variant

## Accessibility Considerations
- Proper aria-label for screen readers
- Animated elements should respect prefers-reduced-motion
- Semantic div element with appropriate attributes
- ARIA attributes can be passed through via `rest` attribute
- Label text provides context for screen reader users
- Visual label can be shown or hidden based on `show_label` prop
- Spinner should be hidden when loading is complete

## Usage Examples
```heex
<!-- Basic spinner -->
<Spinner />

<!-- Small spinner -->
<Spinner size="sm" />

<!-- Large spinner -->
<Spinner size="lg" />

<!-- Spinner with label -->
<Spinner show_label label="Loading data..." />

<!-- Secondary variant spinner -->
<Spinner variant="secondary" />

<!-- Spinner in button -->
<Button disabled>
  <Spinner class="mr-2" />
  Processing...
</Button>

<!-- Full screen loading -->
<div class="fixed inset-0 flex items-center justify-center bg-background/80 backdrop-blur-sm">
  <Spinner size="lg" show_label label="Loading application..." />
</div>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- All variants and sizes are fully responsive
- Consistent with the design system using CSS variables
- Follows accessibility best practices with proper labeling
- Uses animate-spin class for CSS animation
- SVG contains circle and path elements for the spinner design
- Circle provides the background track
- Path provides the animated indicator
- Gap-2 provides spacing between spinner and label
- Opacity values create visual distinction between track and indicator
- Text-primary provides consistent coloring
- Supports both visual and screen reader labels
- Can be used standalone or within other components