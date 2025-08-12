# Organization Form Component Specification

## Component Name
Organization Form

## Description
A form component for creating and editing organization information. Provides a structured interface for users to input and update organization details including name, description, location, and public visibility settings.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `form` | map | %{source: %{}, errors: []} | Form data with source fields and errors |
| `editing` | boolean | false | Whether the form is in edit mode |
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
| `name` | string | Yes | Business name |
| `description` | string | Yes | Internal business description |
| `is_public_searchable` | boolean | No | Whether business appears in public search |
| `public_description` | string | No | Public-facing business description |
| `city` | string | Yes | Business city |
| `country` | string | Yes | Business country |
| `address` | string | Yes | Business full address |

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
<!-- Create business form -->
<.business_form
  form={%{
    id: "business-form",
    source: %{
      name: "",
      description: "",
      is_public_searchable: false,
      public_description: "",
      city: "",
      country: "",
      address: ""
    },
    errors: []
  }}
  editing={false}
  loading={false}
  on_submit="save_business"
  on_change="validate_business"
  on_cancel="cancel_form"
/>

<!-- Edit business form -->
<.business_form
  form={%{
    id: "business-form",
    source: %{
      name: "Sunny Beach Resort",
      description: "A beautiful beachfront resort with stunning ocean views and world-class amenities.",
      is_public_searchable: true,
      public_description: "Luxury beach resort with ocean views",
      city: "Miami Beach",
      country: "USA",
      address: "123 Ocean Drive, Miami Beach, FL 33139"
    },
    errors: []
  }}
  editing={true}
  loading={false}
  on_submit="save_business"
  on_change="validate_business"
  on_cancel="cancel_form"
/>

<!-- Loading business form -->
<.business_form
  form={%{
    id: "business-form",
    source: %{
      name: "Sunny Beach Resort",
      description: "A beautiful beachfront resort with stunning ocean views and world-class amenities.",
      is_public_searchable: true,
      public_description: "Luxury beach resort with ocean views",
      city: "Miami Beach",
      country: "USA",
      address: "123 Ocean Drive, Miami Beach, FL 33139"
    },
    errors: []
  }}
  editing={true}
  loading={true}
  on_submit="save_business"
  on_change="validate_business"
  on_cancel="cancel_form"
/>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Consistent with the design system using CSS variables
- Follows accessibility best practices with semantic HTML
- Integrates with AshPhoenix forms for validation and error handling
- Form fields are organized into logical sections
- Public visibility settings are grouped together
- Loading state provides visual feedback during form submission
- Edit mode shows existing business data for updates
- Create mode shows empty fields for new business entry
- Form validation is handled through on_change events
- Submit and cancel actions provide clear user interactions
- Error messages are displayed next to relevant fields
- Component adapts to different screen sizes with responsive design