# Radio Component Specification

## Component Name
Radio

## Description
A radio button component for selecting a single option from a group. Supports labels, descriptions, validation states, and various sizes.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `field` | Phoenix.HTML.FormField | nil | Form field for integration with AshPhoenix forms |
| `value` | string | - | Required. The value when selected |
| `checked` | boolean | false | Whether the radio is checked |
| `label` | string | nil | The label text for the radio |
| `description` | string | nil | Additional description text |
| `disabled` | boolean | false | Whether the radio is disabled |
| `size` | string | "md" | The radio size (sm, md, lg) |
| `variant` | string | "default" | The radio style variant (default, error, success) |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Variants
| Variant | Description | CSS Classes |
|---------|-------------|-------------|
| `default` | Standard radio style | `text-primary` |
| `error` | Error state radio | `border-destructive text-destructive focus:ring-destructive` |
| `success` | Success state radio | `border-[var(--chart-5)] text-[var(--chart-5)] focus:ring-[var(--chart-5)]` |

## Sizes
| Size | Description | CSS Classes (Radio) | CSS Classes (Label) |
|------|-------------|---------------------|---------------------|
| `sm` | Small radio size | `h-3 w-3` | `text-xs` |
| `md` | Medium radio size | `h-4 w-4` | `text-sm` |
| `lg` | Large radio size | `h-5 w-5` | `text-base` |

## CSS Classes and Styling Approach
### Base Classes (Radio Input)
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
- `--primary` - Checked state color for default variant
- `--ring` - Focus ring color
- `--destructive` - Error state colors
- `--chart-5` - Success state colors
- `--muted-foreground` - Description text color
- `--radius` - Border radius (used via rounded-full class)

## Accessibility Considerations
- Proper focus states with visible focus ring
- Disabled radios have `disabled:cursor-not-allowed disabled:opacity-50` to prevent interaction
- Labels are properly associated with radio inputs
- Description text uses `--muted-foreground` for proper contrast
- Semantic input element with appropriate attributes
- Form field integration supports proper labeling and validation
- ARIA attributes can be passed through via `rest` attribute
- Radios in a group should have the same name attribute
- Only one radio in a group should be checked at a time

## Usage Examples
```heex
<!-- Basic radio -->
<Radio name="option" value="1" label="Option 1" />

<!-- Checked radio -->
<Radio name="option" value="2" checked label="Option 2" />

<!-- Radio with description -->
<Radio name="option" value="3" label="Option 3" description="This is option 3" />

<!-- Error state -->
<Radio name="option" value="4" variant="error" label="Option 4" />

<!-- Success state -->
<Radio name="option" value="5" variant="success" label="Option 5" />

<!-- Small radio -->
<Radio name="option" value="6" size="sm" label="Small radio" />

<!-- Large radio -->
<Radio name="option" value="7" size="lg" label="Large radio" />

<!-- Disabled radio -->
<Radio name="option" value="8" disabled label="Disabled option" />

<!-- With form field -->
<Radio field={@form[:category]} value="work" label="Work" />
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
- Properly handles disabled state for both radio and label
- Rounded-full gives circular appearance
- Border-input provides consistent border color
- Text-primary provides consistent checked state color