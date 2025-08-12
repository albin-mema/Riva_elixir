# Status Indicator Component Specification

## Component Name
Status Indicator

## Description
A status indicator component with colors and icons for visually representing the status of an item or system. Provides quick visual feedback about the current state of an entity.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `status` | string | "" | Status value used for determining styling |
| `label` | string | "" | Text label to display with the status indicator |
| `variant` | string | "default" | Style variant (default, success, warning, error, info) |
| `size` | string | "md" | Size of the status indicator (sm, md, lg) |
| `show_icon` | boolean | true | Whether to show the status icon |
| `show_pulse` | boolean | false | Whether to show a pulse animation |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Sizes
| Size | Description | CSS Classes |
|------|-------------|-------------|
| `sm` | Small status indicator | Smaller icon and text |
| `md` | Medium status indicator | Default size |
| `lg` | Large status indicator | Larger icon and text |

## Variants
| Variant | Description | Color Scheme | Default Icon |
|---------|-------------|--------------|--------------|
| `default` | Standard status indicator | Gray color scheme | check_circle |
| `success` | Success state | Green color scheme | check_circle |
| `warning` | Warning state | Yellow/orange color scheme | exclamation_triangle |
| `error` | Error state | Red color scheme | x_circle |
| `info` | Information state | Blue color scheme | information_circle |

## CSS Classes and Styling Approach
### Base Classes
The component uses a container with an icon and label, applying consistent styling to all parts.

### CSS Variables Used
- `--success` / `--success-foreground` - Colors for success variant
- `--warning` / `--warning-foreground` - Colors for warning variant
- `--destructive` / `--destructive-foreground` - Colors for error variant
- `--primary` / `--primary-foreground` - Colors for info variant
- `--muted` / `--muted-foreground` - Colors for default variant
- `--radius` - Border radius for pill-shaped indicator

## Accessibility Considerations
- Proper ARIA attributes for status information
- Semantic HTML structure with appropriate elements
- Sufficient color contrast for all text elements
- Icons are decorative and hidden from screen readers with aria-hidden
- Labels provide clear status information for screen readers
- Pulse animation respects user preference for reduced motion
- Visual indicators are perceivable by users with color blindness
- Text labels ensure status is understandable without relying solely on color

## Usage Examples
```heex
<!-- Basic status indicator -->
<.status_indicator
  status="active"
  label="Active"
/>

<!-- Status indicator without icon -->
<.status_indicator
  status="pending"
  label="Pending"
  show_icon={false}
/>

<!-- Status indicator with pulse animation -->
<.status_indicator
  status="online"
  label="Online"
  show_pulse={true}
/>

<!-- Success status indicator -->
<.status_indicator
  status="completed"
  label="Completed"
  variant="success"
/>

<!-- Warning status indicator -->
<.status_indicator
  status="warning"
  label="Warning"
  variant="warning"
/>

<!-- Error status indicator -->
<.status_indicator
  status="error"
  label="Error"
  variant="error"
/>

<!-- Info status indicator -->
<.status_indicator
  status="info"
  label="Info"
  variant="info"
/>

<!-- Status indicators in different sizes -->
<.status_indicator
  status="small"
  label="Small"
  size="sm"
/>

<.status_indicator
  status="medium"
  label="Medium"
  size="md"
/>

<.status_indicator
  status="large"
  label="Large"
  size="lg"
/>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Consistent with the design system using CSS variables
- Follows accessibility best practices with semantic HTML
- Icon selection is based on variant type
- Pulse animation provides visual feedback for active states
- All sizes follow consistent styling patterns
- Variants provide visual feedback for different states
- Status prop is used to determine styling when variant is not specified
- Component is responsive and works on all screen sizes
- Text labels ensure status is clear and understandable