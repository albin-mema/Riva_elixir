# Progress Bar Component Specification

## Component Name
Progress Bar

## Description
A progress bar component for indicating completion status or progress of an operation. Provides visual feedback to users about the status of ongoing processes.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `value` | number | 0 | Current progress value |
| `max` | number | 100 | Maximum value for the progress bar |
| `label` | string | "" | Optional label to display with the progress bar |
| `show_percentage` | boolean | false | Whether to display the percentage completion |
| `size` | string | "md" | Size of the progress bar (sm, md, lg) |
| `variant` | string | "default" | Style variant (default, success, warning, error) |
| `animated` | boolean | false | Whether to show animation on the progress bar |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Sizes
| Size | Description | CSS Classes |
|------|-------------|-------------|
| `sm` | Small progress bar | Smaller height |
| `md` | Medium progress bar | Default height |
| `lg` | Large progress bar | Larger height |

## Variants
| Variant | Description | Color Scheme |
|---------|-------------|--------------|
| `default` | Standard progress bar | Primary color |
| `success` | Success state | Green color scheme |
| `warning` | Warning state | Yellow/orange color scheme |
| `error` | Error state | Red color scheme |

## CSS Classes and Styling Approach
### Base Classes
The component uses a container with a progress track and fill element, applying consistent styling to all parts.

### CSS Variables Used
- `--primary` / `--primary-foreground` - Colors for default variant
- `--success` / `--success-foreground` - Colors for success variant
- `--warning` / `--warning-foreground` - Colors for warning variant
- `--destructive` / `--destructive-foreground` - Colors for error variant
- `--background` - Background color for track
- `--radius` - Border radius

## Accessibility Considerations
- Proper ARIA attributes (aria-valuenow, aria-valuemin, aria-valuemax)
- Semantic HTML structure with progress element
- Sufficient color contrast for all text elements
- Labels provide context for screen readers
- Percentage display is announced to screen readers
- Animation respects user preference for reduced motion
- Visual indicators are perceivable by users with color blindness

## Usage Examples
```heex
<!-- Basic progress bar -->
<.progress_bar
  value={50}
  max={100}
/>

<!-- Progress bar with label -->
<.progress_bar
  value={75}
  max={100}
  label="Processing..."
/>

<!-- Progress bar with percentage -->
<.progress_bar
  value={30}
  max={100}
  show_percentage={true}
/>

<!-- Success progress bar -->
<.progress_bar
  value={100}
  max={100}
  label="Complete"
  variant="success"
/>

<!-- Warning progress bar -->
<.progress_bar
  value={60}
  max={100}
  label="Warning"
  variant="warning"
/>

<!-- Error progress bar -->
<.progress_bar
  value={40}
  max={100}
  label="Error"
  variant="error"
/>

<!-- Progress bars in different sizes -->
<.progress_bar
  value={30}
  max={100}
  label="Small"
  size="sm"
/>

<.progress_bar
  value={50}
  max={100}
  label="Medium"
  size="md"
/>

<.progress_bar
  value={70}
  max={100}
  label="Large"
  size="lg"
/>

<!-- Animated progress bar -->
<.progress_bar
  value={60}
  max={100}
  label="Animated Progress"
  animated={true}
/>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Consistent with the design system using CSS variables
- Follows accessibility best practices with semantic HTML
- Value and max props control the progress percentage
- Label and percentage display provide additional context
- All sizes follow consistent styling patterns
- Variants provide visual feedback for different states
- Animation can be enabled for ongoing processes
- Progress bar uses proper ARIA attributes for accessibility
- Component is responsive and works on all screen sizes