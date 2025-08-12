# Item Form Component Specification

## Component Name
Item Form

## Description
A form component for creating and editing item information. Provides a structured interface for users to input and update item details including name, description, section assignment, type, availability, and layout positioning.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `form` | map | %{source: %{}, errors: []} | Form data with source fields and errors |
| `sections` | list | [] | List of sections for assignment dropdown |
| `item_types` | list | [] | List of item types for selection |
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
| `name` | string | Yes | Item name |
| `description` | string | No | Internal item description |
| `section_id` | string | Yes | ID of assigned section |
| `item_type_id` | string | Yes | ID of item type |
| `is_active` | boolean | No | Whether item is currently active |
| `is_always_available` | boolean | No | Whether item is always available for booking |
| `capacity` | string | No | Item capacity (number of people) |
| `is_public_searchable` | boolean | No | Whether item appears in public search |
| `public_description` | string | No | Public-facing item description |
| `grid_row` | string | No | Grid row position for layout |
| `grid_column` | string | No | Grid column position for layout |

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
- Numeric fields have appropriate input types for assistive technology

## Usage Examples
```heex
<!-- Create item form -->
<.item_form
  form={%{
    id: "item-form",
    source: %{
      name: "",
      description: "",
      section_id: "",
      item_type_id: "",
      is_active: true,
      is_always_available: false,
      capacity: "",
      is_public_searchable: false,
      public_description: "",
      grid_row: "",
      grid_column: ""
    },
    errors: []
  }}
  sections={[
    {"Beach Area", "1"},
    {"Pool Area", "2"},
    {"Restaurant", "3"}
  ]}
  item_types={[
    {"Umbrella", "1"},
    {"Sunbed", "2"},
    {"Cabin", "3"}
  ]}
  editing={false}
  on_submit="save_item"
  on_change="validate_item"
  on_cancel="cancel_form"
  loading={false}
/>

<!-- Edit item form -->
<.item_form
  form={%{
    id: "item-form",
    source: %{
      name: "Premium Beach Umbrella",
      description: "Large umbrella with UV protection",
      section_id: "1",
      item_type_id: "1",
      is_active: true,
      is_always_available: false,
      capacity: "4",
      is_public_searchable: true,
      public_description: "Premium beach umbrella with UV protection",
      grid_row: "2",
      grid_column: "5"
    },
    errors: []
  }}
  sections={[
    {"Beach Area", "1"},
    {"Pool Area", "2"},
    {"Restaurant", "3"}
  ]}
  item_types={[
    {"Umbrella", "1"},
    {"Sunbed", "2"},
    {"Cabin", "3"}
  ]}
  editing={true}
  on_submit="save_item"
  on_change="validate_item"
  on_cancel="cancel_form"
  loading={false}
/>

<!-- Loading item form -->
<.item_form
  form={%{
    id: "item-form",
    source: %{
      name: "Premium Beach Umbrella",
      description: "Large umbrella with UV protection",
      section_id: "1",
      item_type_id: "1",
      is_active: true,
      is_always_available: false,
      capacity: "4",
      is_public_searchable: true,
      public_description: "Premium beach umbrella with UV protection",
      grid_row: "2",
      grid_column: "5"
    },
    errors: []
  }}
  sections={[
    {"Beach Area", "1"},
    {"Pool Area", "2"},
    {"Restaurant", "3"}
  ]}
  item_types={[
    {"Umbrella", "1"},
    {"Sunbed", "2"},
    {"Cabin", "3"}
  ]}
  editing={true}
  on_submit="save_item"
  on_change="validate_item"
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
- Section assignment uses dropdown with provided section list
- Item type selection uses dropdown with provided item type list
- Loading state provides visual feedback during form submission
- Edit mode shows existing item data for updates
- Create mode shows empty fields for new item entry
- Form validation is handled through on_change events
- Submit and cancel actions provide clear user interactions
- Error messages are displayed next to relevant fields
- Component adapts to different screen sizes with responsive design
- Capacity field uses numeric input for better UX
- Public visibility settings are grouped together
- Grid positioning fields allow for layout customization
- Always available toggle enables special booking rules
- Active status toggle enables item deactivation