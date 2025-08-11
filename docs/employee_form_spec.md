# User Form Component Specification

## Component Name
User Form

## Description
A form component for creating and editing user information. Provides a structured interface for users to input and update user details including personal information, employment details, permissions, and organization assignment.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `form` | map | %{source: %{}, errors: []} | Form data with source fields and errors |
| `editing` | boolean | false | Whether the form is in edit mode |
| `loading` | boolean | false | Whether the form is in a loading state |
| `businesses` | list | [] | List of businesses for assignment dropdown |
| `permissions` | list | [] | List of permissions for selection |
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
| `first_name` | string | Yes | Employee's first name |
| `last_name` | string | Yes | Employee's last name |
| `email` | string | Yes | Employee's email address |
| `phone` | string | No | Employee's phone number |
| `employee_number` | string | No | Unique employee identifier |
| `permission_ids` | list | No | List of selected permission IDs |
| `role` | string | No | Employee's role within the organization |
| `hire_date` | string | No | Employee's hire date (YYYY-MM-DD format) |
| `business_id` | string | Yes | ID of assigned business |
| `is_active` | boolean | No | Whether employee is currently active |
| `notes` | string | No | Additional notes about the employee |

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
- Date fields have appropriate input types for assistive technology

## Usage Examples
```heex
<!-- Create employee form -->
<.employee_form
  form={%{
    id: "employee-form",
    source: %{
      first_name: "",
      last_name: "",
      email: "",
      phone: "",
      employee_number: "",
      permission_ids: [],
      role: "",
      hire_date: "",
      business_id: "",
      is_active: true,
      notes: ""
    },
    errors: []
  }}
  editing={false}
  loading={false}
  businesses={[
    %{id: "1", name: "Sunny Beach Resort"},
    %{id: "2", name: "Mountain View Hotel"}
  ]}
  permissions={[
    %{id: "1", name: "manage_reservations", description: "Can manage reservations"},
    %{id: "2", name: "view_financials", description: "Can view financial reports"}
  ]}
  on_submit="save_employee"
  on_change="validate_employee"
  on_cancel="cancel_form"
/>

<!-- Edit employee form -->
<.employee_form
  form={%{
    id: "employee-form",
    source: %{
      first_name: "John",
      last_name: "Doe",
      email: "john.doe@business.com",
      phone: "+1 (555) 123-4567",
      employee_number: "EMP001",
      permission_ids: ["1"],
      role: "manager",
      hire_date: "2024-01-15",
      business_id: "1",
      is_active: true,
      notes: "Experienced manager with 5 years in hospitality"
    },
    errors: []
  }}
  editing={true}
  loading={false}
  businesses={[
    %{id: "1", name: "Sunny Beach Resort"},
    %{id: "2", name: "Mountain View Hotel"}
  ]}
  permissions={[
    %{id: "1", name: "manage_reservations", description: "Can manage reservations"},
    %{id: "2", name: "view_financials", description: "Can view financial reports"}
  ]}
  on_submit="save_employee"
  on_change="validate_employee"
  on_cancel="cancel_form"
/>

<!-- Loading employee form -->
<.employee_form
  form={%{
    id: "employee-form",
    source: %{
      first_name: "John",
      last_name: "Doe",
      email: "john.doe@business.com",
      phone: "+1 (555) 123-4567",
      employee_number: "EMP001",
      permission_ids: ["1"],
      role: "manager",
      hire_date: "2024-01-15",
      business_id: "1",
      is_active: true,
      notes: "Experienced manager with 5 years in hospitality"
    },
    errors: []
  }}
  editing={true}
  loading={true}
  businesses={[
    %{id: "1", name: "Sunny Beach Resort"},
    %{id: "2", name: "Mountain View Hotel"}
  ]}
  permissions={[
    %{id: "1", name: "manage_reservations", description: "Can manage reservations"},
    %{id: "2", name: "view_financials", description: "Can view financial reports"}
  ]}
  on_submit="save_employee"
  on_change="validate_employee"
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
- Business assignment uses dropdown with provided business list
- Permissions are selectable through checkboxes or multi-select
- Loading state provides visual feedback during form submission
- Edit mode shows existing employee data for updates
- Create mode shows empty fields for new employee entry
- Form validation is handled through on_change events
- Submit and cancel actions provide clear user interactions
- Error messages are displayed next to relevant fields
- Component adapts to different screen sizes with responsive design
- Date fields use appropriate input types for better UX
- Employee number is optional but useful for identification
- Role field allows for organizational hierarchy definition
- Active status toggle enables employee deactivation
- Notes field provides space for additional information