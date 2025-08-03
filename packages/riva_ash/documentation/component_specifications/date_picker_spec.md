# DatePicker Component Specification

## Component Name
DatePicker

## Description
A date picker component for selecting dates with calendar popup functionality. Supports min/max date constraints and various formatting options.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `field` | Phoenix.HTML.FormField | nil | Form field for integration with AshPhoenix forms |
| `value` | string | nil | The selected date value |
| `min_date` | string | nil | Minimum selectable date |
| `max_date` | string | nil | Maximum selectable date |
| `placeholder` | string | "Select date" | Placeholder text |
| `disabled` | boolean | false | Whether the date picker is disabled |
| `required` | boolean | false | Whether the date picker is required |
| `format` | string | "yyyy-mm-dd" | Date format for display |
| `size` | string | "md" | The input size (sm, md, lg) |
| `variant` | string | "default" | The input style variant (default, error, success) |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Variants
| Variant | Description | CSS Classes |
|---------|-------------|-------------|
| `default` | Standard date picker style | No additional classes |
| `error` | Error state date picker | `border-destructive focus-visible:ring-destructive` |
| `success` | Success state date picker | `border-[var(--chart-5)] focus-visible:ring-[var(--chart-5)]` |

## Sizes
| Size | Description | CSS Classes |
|------|-------------|-------------|
| `sm` | Small date picker size | `h-9 px-2 text-xs` |
| `md` | Medium date picker size | `h-10 px-3 text-sm` |
| `lg` | Large date picker size | `h-11 px-4 text-base` |

## CSS Classes and Styling Approach
### Base Classes
```
flex w-full rounded-md border border-input bg-background px-3 py-2 text-sm ring-offset-background file:border-0 file:bg-transparent file:text-sm file:font-medium placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50
```

### CSS Variables Used
- `--input` - Border color
- `--background` / `--foreground` - Background and text colors
- `--muted-foreground` - Placeholder text color
- `--ring` - Focus ring color
- `--destructive` - Error state border and focus ring color
- `--chart-5` - Success state border and focus ring color
- `--radius` - Border radius (used via rounded-md class)

## Accessibility Considerations
- Proper focus states with visible focus ring
- Disabled date pickers have `disabled:cursor-not-allowed disabled:opacity-50` to prevent interaction
- Placeholder text uses `--muted-foreground` for proper contrast
- Semantic input element with appropriate attributes
- Form field integration supports proper labeling and validation
- ARIA attributes can be passed through via `rest` attribute
- Calendar popup should be properly labeled and navigable via keyboard
- Date format should be clearly indicated

## Usage Examples
```heex
<!-- Basic date picker -->
<DatePicker placeholder="Select a date" />

<!-- Date picker with value -->
<DatePicker value="2023-06-15" />

<!-- Required date picker -->
<DatePicker required placeholder="Select date (required)" />

<!-- Disabled date picker -->
<DatePicker disabled value="2023-06-15" />

<!-- Date picker with min/max dates -->
<DatePicker min_date="2023-01-01" max_date="2023-12-31" placeholder="Select date in 2023" />

<!-- Error state -->
<DatePicker variant="error" placeholder="Invalid date" />

<!-- Success state -->
<DatePicker variant="success" placeholder="Valid date" />

<!-- Small date picker -->
<DatePicker size="sm" placeholder="Small date picker" />

<!-- Large date picker -->
<DatePicker size="lg" placeholder="Large date picker" />

<!-- With form field -->
<DatePicker field={@form[:start_date]} placeholder="Start date" />
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Implements proper form field integration with AshPhoenix forms
- Supports global attributes for Phoenix LiveView events
- All variants and sizes are fully responsive
- Consistent with the design system using CSS variables
- Follows accessibility best practices with proper focus states
- Supports min_date and max_date constraints
- Placeholder text has proper contrast with background
- Calendar popup functionality should be implemented with proper positioning
- Date format should be configurable but default to ISO format (yyyy-mm-dd)
- Should support both mouse and keyboard interaction
- Value can be controlled or uncontrolled
- Properly handles disabled and required states