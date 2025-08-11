# Toggle Component Specification

## Component Name
Toggle

## Description
A toggle/switch component for boolean values. Supports labels, descriptions, validation states, and various sizes.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `field` | Phoenix.HTML.FormField | nil | Form field for integration with AshPhoenix forms |
| `checked` | boolean | false | Whether the toggle is checked |
| `value` | string | "true" | The value when checked |
| `label` | string | nil | The label text for the toggle |
| `description` | string | nil | Additional description text |
| `disabled` | boolean | false | Whether the toggle is disabled |
| `size` | string | "md" | The toggle size (sm, md, lg) |
| `variant` | string | "default" | The toggle style variant (default, success, warning, destructive) |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Variants
| Variant | Description | CSS Classes |
|---------|-------------|-------------|
| `default` | Standard toggle style | No additional classes |
| `success` | Success state toggle | Success state styling |
| `warning` | Warning state toggle | Warning state styling |
| `destructive` | Destructive state toggle | Destructive state styling |

## Sizes
| Size | Description | CSS Classes (Toggle) | CSS Classes (Label) |
|------|-------------|----------------------|---------------------|
| `sm` | Small toggle size | Small sizing classes | `text-xs` |
| `md` | Medium toggle size | Medium sizing classes | `text-sm` |
| `lg` | Large toggle size | Large sizing classes | `text-base` |

## CSS Classes and Styling Approach
### Base Classes (Toggle Input)
```
rounded-full border border-input bg-background focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-ring disabled:cursor-not-allowed disabled:opacity-50
```

### Label Classes
```
text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70
```

### CSS Variables Used
- `--input` - Border color
- `--background` - Background color
- `--ring` - Focus ring color
- `--radius` - Border radius (used via rounded-full class)
- `--primary` - Primary color for checked state
- `--success` - Success state colors
- `--warning` - Warning state colors
- `--destructive` - Destructive state colors

## Accessibility Considerations
- Proper focus states with visible focus ring
- Disabled toggles have `disabled:cursor-not-allowed disabled:opacity-50` to prevent interaction
- Labels are properly associated with toggle inputs
- Description text uses appropriate contrast
- Semantic input element with appropriate attributes
- Form field integration supports proper labeling and validation
- ARIA attributes can be passed through via `rest` attribute
- Toggle should be operable via keyboard (space/enter)
- Checked state should be visually distinct

## Usage Examples
```heex
<!-- Basic toggle -->
<Toggle label="Enable notifications" />

<!-- Checked toggle -->
<Toggle checked label="Dark mode" />

<!-- Toggle with description -->
<Toggle label="Auto-save" description="Automatically save changes every 5 minutes" />

<!-- Success variant -->
<Toggle variant="success" label="Enable feature" />

<!-- Warning variant -->
<Toggle variant="warning" label="Experimental feature" />

<!-- Destructive variant -->
<Toggle variant="destructive" label="Delete account" />

<!-- Small toggle -->
<Toggle size="sm" label="Small toggle" />

<!-- Large toggle -->
<Toggle size="lg" label="Large toggle" />

<!-- Disabled toggle -->
<Toggle disabled label="Disabled option" />

<!-- With form field -->
<Toggle field={@form[:notifications]} label="Email notifications" />
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
- Properly handles disabled state for both toggle and label
- Rounded-full gives switch-like appearance
- Border-input provides consistent border color
- Should have smooth transition animations for state changes
- Thumb element should be visually distinct from track
- Checked state should use primary color by default
- Variants should apply appropriate colors for different contexts
- Size variants should adjust both track and thumb dimensions