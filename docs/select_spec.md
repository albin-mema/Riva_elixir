# Select Component Specification

## Component Name
Select

## Description
A select dropdown component that supports options, prompts, validation states, and various sizes. Used for selecting from a list of options in forms.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `field` | Phoenix.HTML.FormField | nil | Form field for integration with AshPhoenix forms |
| `options` | list | [] | List of {label, value} tuples for select options |
| `prompt` | string | nil | Prompt text to show as first option |
| `multiple` | boolean | false | Whether multiple options can be selected |
| `disabled` | boolean | false | Whether the select is disabled |
| `required` | boolean | false | Whether the select is required |
| `variant` | string | "default" | The select style variant (default, error, success) |
| `size` | string | "default" | The select size (default, sm, lg) |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Variants
| Variant | Description | CSS Classes |
|---------|-------------|-------------|
| `default` | Standard select style | No additional classes |
| `error` | Error state select | `border-destructive focus:ring-destructive` |
| `success` | Success state select | `border-[var(--chart-5)] focus:ring-[var(--chart-5)]` |

## Sizes
| Size | Description | CSS Classes |
|------|-------------|-------------|
| `default` | Standard select size | `h-10 px-3 text-sm` |
| `sm` | Small select size | `h-9 px-2 text-xs` |
| `lg` | Large select size | `h-11 px-4 text-base` |

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
- Disabled selects have `disabled:cursor-not-allowed disabled:opacity-50` to prevent interaction
- Semantic select element with appropriate attributes
- Form field integration supports proper labeling and validation
- ARIA attributes can be passed through via `rest` attribute
- Options are properly labeled with text content
- Prompt option provides context for empty selection

## Usage Examples
```heex
<!-- Basic select -->
<Select options={[{"Option 1", "1"}, {"Option 2", "2"}, {"Option 3", "3"}]} />

<!-- With prompt -->
<Select prompt="Select an option" options={[{"Option 1", "1"}, {"Option 2", "2"}]} />

<!-- With selected value -->
<Select value="2" options={[{"Option 1", "1"}, {"Option 2", "2"}, {"Option 3", "3"}]} />

<!-- Multiple select -->
<Select multiple options={[{"Option 1", "1"}, {"Option 2", "2"}, {"Option 3", "3"}]} />

<!-- Required select -->
<Select required options={[{"Option 1", "1"}, {"Option 2", "2"}]} />

<!-- Disabled select -->
<Select disabled options={[{"Option 1", "1"}, {"Option 2", "2"}]} />

<!-- Error state -->
<Select variant="error" prompt="Select an option" options={[{"Option 1", "1"}, {"Option 2", "2"}]} />

<!-- Success state -->
<Select variant="success" prompt="Select an option" options={[{"Option 1", "1"}, {"Option 2", "2"}]} />

<!-- Small select -->
<Select size="sm" prompt="Small select" options={[{"Option 1", "1"}, {"Option 2", "2"}]} />

<!-- Large select -->
<Select size="lg" prompt="Large select" options={[{"Option 1", "1"}, {"Option 2", "2"}]} />

<!-- With form field -->
<Select field={@form[:category]} prompt="Select category" options={@categories} />
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Implements proper form field integration with AshPhoenix forms
- Supports global attributes for Phoenix LiveView events
- All variants and sizes are fully responsive
- Consistent with the design system using CSS variables
- Follows accessibility best practices with proper focus states
- Supports both single and multiple selection modes
- Prompt option is optional but recommended for usability
- Options are provided as a list of {label, value} tuples
- Properly handles disabled state