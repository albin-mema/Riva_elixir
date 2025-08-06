# Checkbox Component Specification

## Component Name
Checkbox

## Description
A checkbox component that supports labels, descriptions, validation states, and various sizes. Used for boolean choices in forms and UI controls.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `field` | Phoenix.HTML.FormField | nil | Form field for integration with AshPhoenix forms |
| `checked` | boolean | false | Whether the checkbox is checked |
| `value` | string | "true" | The value when checked |
| `label` | string | nil | The label text for the checkbox |
| `description` | string | nil | Additional description text |
| `disabled` | boolean | false | Whether the checkbox is disabled |
| `variant` | string | "default" | The checkbox style variant (default, error, success) |
| `size` | string | "default" | The checkbox size (default, sm, lg) |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Variants
| Variant | Description | CSS Classes |
|---------|-------------|-------------|
| `default` | Standard checkbox style | `text-primary` |
| `error` | Error state checkbox | `border-destructive text-destructive focus:ring-destructive` |
| `success` | Success state checkbox | `border-[var(--chart-5)] text-[var(--chart-5)] focus:ring-[var(--chart-5)]` |

## Sizes
| Size | Description | CSS Classes (Checkbox) | CSS Classes (Label) |
|------|-------------|------------------------|---------------------|
| `default` | Standard checkbox size | `h-4 w-4` | `text-sm` |
| `sm` | Small checkbox size | `h-3 w-3` | `text-xs` |
| `lg` | Large checkbox size | `h-5 w-5` | `text-base` |

## CSS Classes and Styling Approach
### Base Classes (Checkbox Input)
```
rounded border border-input bg-background focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-ring disabled:cursor-not-allowed disabled:opacity-50
```

### Label Classes
```
text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70
```

### CSS Variables Used
- `--input` - Border color
- `--background` - Background color
- `--primary` - Checked state color for default variant
- `--ring` - Focus ring color
- `--destructive` - Error state colors
- `--chart-5` - Success state colors
- `--muted-foreground` - Description text color
- `--radius` - Border radius (used via rounded class)

## Accessibility Considerations
- Proper focus states with visible focus ring
- Disabled checkboxes have `disabled:cursor-not-allowed disabled:opacity-50` to prevent interaction
- Labels are properly associated with checkbox inputs
- Description text uses `--muted-foreground` for proper contrast
- Semantic input element with appropriate attributes
- Form field integration supports proper labeling and validation
- ARIA attributes can be passed through via `rest` attribute

## Usage Examples
```heex
<!-- Basic checkbox -->
<Checkbox label="Accept terms and conditions" />

<!-- Checked checkbox -->
<Checkbox checked label="Subscribe to newsletter" />

<!-- With description -->
<Checkbox label="Enable notifications" description="Receive email notifications about your account" />

<!-- Error state -->
<Checkbox variant="error" label="I agree to the terms" />

<!-- Success state -->
<Checkbox variant="success" label="Verified" />

<!-- Small checkbox -->
<Checkbox size="sm" label="Small checkbox" />

<!-- Large checkbox -->
<Checkbox size="lg" label="Large checkbox" />

<!-- Disabled checkbox -->
<Checkbox disabled label="Disabled option" />

<!-- With form field -->
<Checkbox field={@form[:terms]} label="Accept terms and conditions" required />
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Implements proper form field integration with AshPhoenix forms
- Supports global attributes for Phoenix LiveView events
- All variants and sizes are fully responsive
- Consistent with the design system using CSS variables
- Follows accessibility best practices with proper focus states
- Label and description are optional but recommended for usability
- Supports both controlled and uncontrolled usage patterns
- Properly handles disabled state for both checkbox and label