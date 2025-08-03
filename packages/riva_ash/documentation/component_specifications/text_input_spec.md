# TextInput Component Specification

## Component Name
TextInput

## Description
A text input component for single-line text entry. Supports various states, sizes, and form integration. This component is similar to the Input component but with a simpler API.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `type` | string | "text" | The input type (text, email, password, etc.) |
| `name` | string | - | The input name attribute |
| `id` | string | nil | The input id attribute (defaults to name if not provided) |
| `value` | string | nil | The input value |
| `placeholder` | string | nil | Placeholder text |
| `disabled` | boolean | false | Whether the input is disabled |
| `phx_debounce` | string | nil | Phoenix LiveView debounce setting |
| `phx_change` | string | nil | Phoenix LiveView change event |
| `phx_keydown` | string | nil | Phoenix LiveView keydown event |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Variants
This component does not have explicit variants, but supports different types and states through props.

## Sizes
This component does not have explicit size options, but can be styled with additional classes.

## CSS Classes and Styling Approach
### Base Classes
```
border px-3 py-2 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500
```

### Additional Classes
Passed via the `class` prop

### CSS Variables Used
- This component currently uses hardcoded colors rather than CSS variables and should be updated to use the design system variables

## Accessibility Considerations
- Proper focus states with visible focus ring
- Semantic input element with appropriate attributes
- ARIA attributes can be passed through via `rest` attribute
- Proper labeling should be provided via associated label element
- Placeholder text should be used sparingly and not replace proper labels

## Usage Examples
```heex
<!-- Basic text input -->
<TextInput name="username" placeholder="Enter your username" />

<!-- Email input -->
<TextInput type="email" name="email" placeholder="Enter your email" />

<!-- Password input -->
<TextInput type="password" name="password" placeholder="Enter your password" />

<!-- Input with value -->
<TextInput name="name" value={@user.name} />

<!-- Disabled input -->
<TextInput name="disabled_field" value="Cannot edit" disabled />

<!-- Input with Phoenix events -->
<TextInput name="search" placeholder="Search..." phx-change="search" phx-debounce="300" />

<!-- Input with custom styling -->
<TextInput name="custom" class="w-full max-w-md bg-muted" />
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Designed for simple text input scenarios
- Should be updated to use CSS variables for consistency with the design system
- Lacks some features of the full Input component like validation states and form field integration
- Focus ring uses hardcoded blue-500 color instead of design system ring color
- Border styling is basic and should be enhanced to match other input components
- Does not support size variants like the main Input component
- Should be updated to follow the same API patterns as other UI components
- Consider deprecating in favor of the full-featured Input component