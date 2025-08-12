# Input Component Specification

## Component Name
Input

## Description
A versatile input component for text entry that supports various validation states, sizes, and form integration. Used for collecting user input in forms.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `type` | string | "text" | The input type (text, email, password, etc.) |
| `field` | Phoenix.HTML.FormField | nil | Form field for integration with AshPhoenix forms |
| `value` | string | nil | The input value |
| `placeholder` | string | "" | Placeholder text |
| `disabled` | boolean | false | Whether the input is disabled |
| `readonly` | boolean | false | Whether the input is read-only |
| `required` | boolean | false | Whether the input is required |
| `variant` | string | "default" | The input style variant (default, error, success) |
| `size` | string | "default" | The input size (default, sm, lg) |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Variants
| Variant | Description | CSS Classes |
|---------|-------------|-------------|
| `default` | Standard input style | No additional classes |
| `error` | Error state input | `border-destructive focus-visible:ring-destructive` |
| `success` | Success state input | `border-[var(--chart-5)] focus-visible:ring-[var(--chart-5)]` |

## Sizes
| Size | Description | CSS Classes |
|------|-------------|-------------|
| `default` | Standard input size | `h-10 px-3` |
| `sm` | Small input size | `h-9 px-2 text-xs` |
| `lg` | Large input size | `h-11 px-4 text-base` |

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
- Disabled inputs have `disabled:cursor-not-allowed disabled:opacity-50` to prevent interaction
- Placeholder text uses `--muted-foreground` for proper contrast
- Semantic input element with appropriate attributes
- Form field integration supports proper labeling and validation
- ARIA attributes can be passed through via `rest` attribute

## Usage Examples
```heex
<!-- Basic input -->
<Input placeholder="Enter your name" />

<!-- Input with value -->
<Input value={@user.name} />

<!-- Email input -->
<Input type="email" placeholder="Enter your email" />

<!-- Required input -->
<Input required placeholder="This field is required" />

<!-- Disabled input -->
<Input disabled value="Cannot edit this" />

<!-- Error state -->
<Input variant="error" placeholder="Invalid input" />

<!-- Success state -->
<Input variant="success" placeholder="Valid input" />

<!-- Small input -->
<Input size="sm" placeholder="Small input" />

<!-- Large input -->
<Input size="lg" placeholder="Large input" />

<!-- With form field -->
<Input field={@form[:email]} placeholder="Email address" />
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Implements proper form field integration with AshPhoenix forms
- Supports global attributes for Phoenix LiveView events
- All variants and sizes are fully responsive
- Consistent with the design system using CSS variables
- Follows accessibility best practices with proper focus states
- Supports file inputs with special styling
- Placeholder text has proper contrast with background