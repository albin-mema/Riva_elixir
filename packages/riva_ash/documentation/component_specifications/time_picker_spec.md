# TimePicker Component Specification

## Component Name
TimePicker

## Description
A time picker component for selecting times with various formatting options. Supports min/max time constraints and different display formats.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `field` | Phoenix.HTML.FormField | nil | Form field for integration with AshPhoenix forms |
| `value` | string | nil | The selected time value |
| `min_time` | string | nil | Minimum selectable time |
| `max_time` | string | nil | Maximum selectable time |
| `step` | integer | 60 | Step interval in seconds |
| `placeholder` | string | "Select time" | Placeholder text |
| `disabled` | boolean | false | Whether the time picker is disabled |
| `required` | boolean | false | Whether the time picker is required |
| `format` | string | "24" | Time format (12 or 24 hour) |
| `size` | string | "md" | The input size (sm, md, lg) |
| `variant` | string | "default" | The input style variant (default, error, success) |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Variants
| Variant | Description | CSS Classes |
|---------|-------------|-------------|
| `default` | Standard time picker style | No additional classes |
| `error` | Error state time picker | `border-destructive focus-visible:ring-destructive` |
| `success` | Success state time picker | `border-[var(--chart-5)] focus-visible:ring-[var(--chart-5)]` |

## Sizes
| Size | Description | CSS Classes |
|------|-------------|-------------|
| `sm` | Small time picker size | `h-9 px-2 text-xs` |
| `md` | Medium time picker size | `h-10 px-3 text-sm` |
| `lg` | Large time picker size | `h-11 px-4 text-base` |

## CSS Classes and Styling Approach
### Base Classes
```
flex w-full rounded-md border border-input bg-background px-3 py-2 text-sm ring-offset-background focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50
```

### CSS Variables Used
- `--input` - Border color
- `--background` / `--foreground` - Background and text colors
- `--ring` - Focus ring color
- `--destructive` - Error state border and focus ring color
- `--chart-5` - Success state border and focus ring color
- `--radius` - Border radius (used via rounded-md class)

## Accessibility Considerations
- Proper focus states with visible focus ring
- Disabled time pickers have `disabled:cursor-not-allowed disabled:opacity-50` to prevent interaction
- Semantic input element with appropriate attributes
- Form field integration supports proper labeling and validation
- ARIA attributes can be passed through via `rest` attribute
- Time format should be clearly indicated
- Step attribute provides appropriate granularity

## Usage Examples
```heex
<!-- Basic time picker -->
<TimePicker placeholder="Select a time" />

<!-- Time picker with value -->
<TimePicker value="14:30" />

<!-- Required time picker -->
<TimePicker required placeholder="Select time (required)" />

<!-- Disabled time picker -->
<TimePicker disabled value="09:00" />

<!-- Time picker with min/max times -->
<TimePicker min_time="09:00" max_time="17:00" placeholder="Select time during business hours" />

<!-- 12-hour format -->
<TimePicker format="12" placeholder="Select time" />

<!-- Custom step (15 minutes) -->
<TimePicker step={900} placeholder="Select time (15 min intervals)" />

<!-- Error state -->
<TimePicker variant="error" placeholder="Invalid time" />

<!-- Success state -->
<TimePicker variant="success" placeholder="Valid time" />

<!-- Small time picker -->
<TimePicker size="sm" placeholder="Small time picker" />

<!-- Large time picker -->
<TimePicker size="lg" placeholder="Large time picker" />

<!-- With form field -->
<TimePicker field={@form[:start_time]} placeholder="Start time" />
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Implements proper form field integration with AshPhoenix forms
- Supports global attributes for Phoenix LiveView events
- All variants and sizes are fully responsive
- Consistent with the design system using CSS variables
- Follows accessibility best practices with proper focus states
- Supports min_time and max_time constraints
- Step attribute controls time increment granularity
- Format attribute controls 12/24 hour display
- Should support both mouse and keyboard interaction
- Value can be controlled or uncontrolled
- Properly handles disabled and required states
- Should integrate with native time input where possible
- Fallback to select dropdown for browsers that don't support time input