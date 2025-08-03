# Client Form Component Specification

## Component Name
Client Form

## Description
A form component for creating and editing client information. Provides a structured interface for users to input and update client details including personal information, contact details, and registration status.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `form` | map | %{source: %{}, errors: []} | Form data with source fields and errors |
| `editing` | boolean | false | Whether the form is in edit mode |
| `show_registration_fields` | boolean | false | Whether to show registration-related fields |
| `loading` | boolean | false | Whether the form is in a loading state |
| `on_submit` | string | "" | Event to send when form is submitted |
| `on_change` | string | "" | Event to send when form values change |
| `on_cancel` | string | "" | Event to send when cancel action is triggered |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Form Fields
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `first_name` | string | Yes | Client's first name |
| `last_name` | string | Yes | Client's last name |
| `email` | string | Yes | Client's email address |
| `phone` | string | No | Client's phone number |
| `is_registered` | boolean | No | Whether client has registered an account |
| `password` | string | Conditional | Password for registration (required when registering) |

## CSS Classes and Styling Approach
### Base Classes
The component uses a form container with structured sections for different field groups, applying consistent styling to all form elements.

### CSS Variables Used
- `--background` / `--foreground` - Background and text colors
- `--border` - Border color
- `--primary` / `--primary-foreground` - Colors for primary actions
- `--secondary` / `--secondary-foreground` - Colors for secondary actions
- `--destructive` / `--destructive-foreground` - Colors for destructive actions
- `--radius` - Border radius

## Accessibility Considerations
- Semantic HTML structure with form and fieldset elements
- Proper labeling of all form fields
- Focus management between form elements
- Error messages associated with fields using aria-describedby
- Sufficient color contrast for all text elements
- Form buttons have visible focus states
- Loading state indicated with aria-busy
- Field groups are organized with appropriate headings
- Screen reader-friendly error notifications

## Usage Examples
```heex
<!-- Client registration form -->
<.client_form
  form={%{
    id: "client-form",
    source: %{
      first_name: "",
      last_name: "",
      email: "",
      phone: "",
      is_registered: false,
      password: ""
    },
    errors: []
  }}
  editing={false}
  show_registration_fields={true}
  on_submit="save_client"
  on_change="validate_client"
  on_cancel="cancel_form"
  loading={false}
/>

<!-- Edit client form -->
<.client_form
  form={%{
    id: "client-form",
    source: %{
      first_name: "John",
      last_name: "Doe",
      email: "john.doe@example.com",
      phone: "+1 (555) 123-4567",
      is_registered: true,
      password: ""
    },
    errors: []
  }}
  editing={true}
  show_registration_fields={true}
  on_submit="save_client"
  on_change="validate_client"
  on_cancel="cancel_form"
  loading={false}
/>

<!-- Loading client form -->
<.client_form
  form={%{
    id: "client-form",
    source: %{
      first_name: "John",
      last_name: "Doe",
      email: "john.doe@example.com",
      phone: "+1 (555) 123-4567",
      is_registered: true,
      password: ""
    },
    errors: []
  }}
  editing={true}
  show_registration_fields={true}
  on_submit="save_client"
  on_change="validate_client"
  on_cancel="cancel_form"
  loading={true}
/>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Consistent with the design system using CSS variables
- Follows accessibility best practices with semantic HTML
- Integrates with AshPhoenix forms for validation and error handling
- Form fields are organized into logical sections
- Registration fields are conditionally displayed
- Loading state provides visual feedback during form submission
- Edit mode shows existing client data for updates
- Create mode shows empty fields for new client entry
- Form validation is handled through on_change events
- Submit and cancel actions provide clear user interactions
- Error messages are displayed next to relevant fields
- Component adapts to different screen sizes with responsive design
- Password field is only required when registering new clients
- Phone number field is optional but validated when provided