# Textarea Component Specification

## Component Name
Textarea

## Description
A textarea component for multi-line text input that supports validation states, sizes, and form integration. Used for collecting longer text input in forms.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `field` | Phoenix.HTML.FormField | nil | Form field for integration with AshPhoenix forms |
| `value` | string | nil | The textarea value |
| `placeholder` | string | "" | Placeholder text |
| `disabled` | boolean | false | Whether the textarea is disabled |
| `readonly` | boolean | false | Whether the textarea is read-only |
| `required` | boolean | false | Whether the textarea is required |
| `rows` | integer | 3 | Number of visible text lines |
| `variant` | string | "default" | The textarea style variant (default, error, success) |
| `size` | string | "default" | The textarea size (default, sm, lg) |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Variants
| Variant | Description | CSS Classes |
|---------|-------------|-------------|
| `default` | Standard textarea style | No additional classes |
| `error` | Error state textarea | `border-destructive focus-visible:ring-destructive` |
| `success` | Success state textarea | `border-[var(--chart-5)] focus-visible:ring-[var(--chart-5)]` |

## Sizes
| Size | Description | CSS Classes |
|------|-------------|-------------|
| `default` | Standard textarea size | `text-sm` |
| `sm` | Small textarea size | `text-xs` |
| `lg` | Large textarea size | `text-base` |

## CSS Classes and Styling Approach
### Base Classes
```
flex w-full rounded-md border border-input bg-background px-3 py-2 text-sm ring-offset-background placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50
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
- Disabled textareas have `disabled:cursor-not-allowed disabled:opacity-50` to prevent interaction
- Placeholder text uses `--muted-foreground` for proper contrast
- Semantic textarea element with appropriate attributes
- Form field integration supports proper labeling and validation
- ARIA attributes can be passed through via `rest` attribute
- Proper labeling should be provided via associated label element

## Usage Examples
```heex
<!-- Basic textarea -->
<Textarea placeholder="Enter your message" />

<!-- Textarea with value -->
<Textarea value={@user.bio} placeholder="Tell us about yourself" />

<!-- With rows -->
<Textarea rows="5" placeholder="Enter detailed description" />

<!-- Required textarea -->
<Textarea required placeholder="This field is required" />

<!-- Disabled textarea -->
<Textarea disabled value="Cannot edit this" />

<!-- Error state -->
<Textarea variant="error" placeholder="Invalid input" />

<!-- Success state -->
<Textarea variant="success" placeholder="Valid input" />

<!-- Small textarea -->
<Textarea size="sm" placeholder="Small textarea" />

<!-- Large textarea -->
<Textarea size="lg" placeholder="Large textarea" />

<!-- With form field -->
<Textarea field={@form[:description]} placeholder="Description" rows="4" />
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Implements proper form field integration with AshPhoenix forms
- Supports global attributes for Phoenix LiveView events
- All variants and sizes are fully responsive
- Consistent with the design system using CSS variables
- Follows accessibility best practices with proper focus states
- Supports placeholder text with proper contrast
- Value can be controlled or uncontrolled
- Properly handles disabled and readonly states
- Rows attribute controls the initial height