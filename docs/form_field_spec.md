# Form Field Component Specification

## Component Name
Form Field

## Description
A complete form field component that combines a label, input, and error messages. Provides a consistent and accessible form field experience with built-in validation and helper text support.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `field` | map | %{name: "", id: "", value: "", errors: []} | Field data with name, id, value, and errors |
| `label` | string | "" | Label text for the field |
| `type` | string | "text" | Input type (text, email, password, tel, etc.) |
| `placeholder` | string | "" | Placeholder text for the input |
| `helper_text` | string | "" | Helper text to display below the input |
| `icon` | atom | nil | Icon to display in the input field |
| `icon_position` | string | "left" | Position of the icon (left or right) |
| `required` | boolean | false | Whether the field is required |
| `disabled` | boolean | false | Whether the field is disabled |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Input Types
| Type | Description | Implementation |
|------|-------------|----------------|
| `text` | Standard text input | Basic text input field |
| `email` | Email input with validation | Email-specific input with validation |
| `password` | Password input with masking | Password field with masked characters |
| `tel` | Telephone number input | Telephone-specific input |
| `number` | Numeric input | Number-specific input |
| `date` | Date input | Date-specific input with picker |

## CSS Classes and Styling Approach
### Base Classes
The component uses a container with label, input, and message elements, applying consistent styling to all parts.

### CSS Variables Used
- `--background` / `--foreground` - Background and text colors
- `--border` - Border color for input
- `--destructive` / `--destructive-foreground` - Colors for error states
- `--muted-foreground` - Color for helper text
- `--primary` - Focus ring color
- `--radius` - Border radius

## Accessibility Considerations
- Proper labeling with for/id association between label and input
- Error messages associated with input using aria-describedby
- Required fields marked with aria-required
- Disabled fields marked with aria-disabled
- Proper focus management and visible focus states
- Semantic HTML structure with label and input elements
- Sufficient color contrast for all text elements
- Helper text provides additional context for users

## Usage Examples
```heex
<!-- Basic form field -->
<.form_field
  field={%{name: "username", id: "username", value: "", errors: []}}
  label="Username"
  placeholder="Enter your username"
/>

<!-- Form field with icon -->
<.form_field
  field={%{name: "email", id: "email", value: "", errors: []}}
  label="Email Address"
  type="email"
  placeholder="user@example.com"
  icon={:envelope}
  icon_position="left"
/>

<!-- Form field with helper text -->
<.form_field
  field={%{name: "password", id: "password", value: "", errors: []}}
  label="Password"
  type="password"
  placeholder="Enter your password"
  helper_text="Password must be at least 8 characters long"
/>

<!-- Required form field -->
<.form_field
  field={%{name: "name", id: "name", value: "", errors: []}}
  label="Full Name"
  placeholder="Enter your full name"
  required={true}
/>

<!-- Form field with errors -->
<.form_field
  field={%{name: "phone", id: "phone", value: "invalid", errors: ["is invalid"]}}
  label="Phone Number"
  type="tel"
  placeholder="Enter your phone number"
/>

<!-- Disabled form field -->
<.form_field
  field={%{name: "disabled_field", id: "disabled_field", value: "Disabled value", errors: []}}
  label="Disabled Field"
  placeholder="This field is disabled"
  disabled={true}
/>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Integrates with AshPhoenix forms for automatic validation
- Consistent with the design system using CSS variables
- Follows accessibility best practices with semantic HTML
- Icon support allows for visual enhancement of fields
- Error messages are automatically displayed when present in field data
- Helper text provides additional context for users
- Required fields show visual indicators
- Disabled fields prevent user interaction
- All input types are supported with appropriate browser validation