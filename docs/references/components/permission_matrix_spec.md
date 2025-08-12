# Permission Matrix Component Specification

## Component Name
Permission Matrix

## Description
A permission matrix component for managing employee permissions. Provides a grid interface for viewing and toggling permissions for a specific employee, with save and cancel actions.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `employee` | map | %{id: "", first_name: "", last_name: ""} | Employee information |
| `permissions` | list | [] | List of available permissions with id, name, and description |
| `current_permissions` | list | [] | List of currently assigned permission IDs |
| `on_permission_change` | string | "" | Event to send when a permission is toggled |
| `on_save` | string | "" | Event to send when save button is clicked |
| `on_cancel` | string | "" | Event to send when cancel button is clicked |
| `loading` | boolean | false | Whether the component is in a loading state |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Permission Object Structure
| Field | Type | Description |
|-------|------|-------------|
| `id` | string | Unique identifier for the permission |
| `name` | string | Permission name/key |
| `description` | string | Human-readable description of the permission |

## CSS Classes and Styling Approach
### Base Classes
The component uses a container with employee information, permission grid, and action buttons, applying consistent styling to all elements.

### CSS Variables Used
- `--background` / `--foreground` - Background and text colors
- `--border` - Border color
- `--primary` / `--primary-foreground` - Colors for primary actions
- `--secondary` / `--secondary-foreground` - Colors for secondary actions
- `--destructive` / `--destructive-foreground` - Colors for destructive actions
- `--radius` - Border radius

## Accessibility Considerations
- Semantic HTML structure with appropriate elements
- Sufficient color contrast for all text elements
- Focus management for interactive elements
- Screen reader-friendly permission descriptions
- Loading state indicated with aria-busy
- Checkbox elements have proper labels
- Action buttons have visible focus states
- Employee name is displayed prominently for context

## Usage Examples
```heex
<!-- Default permission matrix -->
<.permission_matrix
  employee={%{
    id: "1",
    first_name: "John",
    last_name: "Doe"
  }}
  permissions={[
    %{id: "1", name: "manage_reservations", description: "Can create, edit, and delete reservations"},
    %{id: "2", name: "view_financials", description: "Can view financial reports and revenue data"},
    %{id: "3", name: "manage_employees", description: "Can add, edit, and remove employees"},
    %{id: "4", name: "manage_business", description: "Can modify business settings and information"}
  ]}
  current_permissions={["1", "2"]}
  on_permission_change="permission_changed"
  on_save="save_permissions"
  on_cancel="cancel_permissions"
  loading={false}
/>

<!-- Loading permission matrix -->
<.permission_matrix
  employee={%{
    id: "2",
    first_name: "Jane",
    last_name: "Smith"
  }}
  permissions={[
    %{id: "1", name: "manage_reservations", description: "Can create, edit, and delete reservations"},
    %{id: "2", name: "view_financials", description: "Can view financial reports and revenue data"},
    %{id: "3", name: "manage_employees", description: "Can add, edit, and remove employees"},
    %{id: "4", name: "manage_business", description: "Can modify business settings and information"}
  ]}
  current_permissions={["1", "3", "4"]}
  on_permission_change="permission_changed"
  on_save="save_permissions"
  on_cancel="cancel_permissions"
  loading={true}
/>

<!-- Permission matrix with no current permissions -->
<.permission_matrix
  employee={%{
    id: "3",
    first_name: "Bob",
    last_name: "Johnson"
  }}
  permissions={[
    %{id: "1", name: "manage_reservations", description: "Can create, edit, and delete reservations"},
    %{id: "2", name: "view_financials", description: "Can view financial reports and revenue data"},
    %{id: "3", name: "manage_employees", description: "Can add, edit, and remove employees"},
    %{id: "4", name: "manage_business", description: "Can modify business settings and information"}
  ]}
  current_permissions={[]}
  on_permission_change="permission_changed"
  on_save="save_permissions"
  on_cancel="cancel_permissions"
  loading={false}
/>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Consistent with the design system using CSS variables
- Follows accessibility best practices with semantic HTML
- Displays employee name for context
- Shows all available permissions in a grid layout
- Current permissions are pre-selected
- Toggling permissions triggers on_permission_change event
- Save and cancel actions trigger corresponding events
- Loading state provides visual feedback during save operations
- Component adapts to different screen sizes with responsive design
- Permission descriptions provide clarity on what each permission does
- Checkboxes are used for toggling permissions
- Save button is primary action, cancel is secondary
- Employee information is displayed prominently at the top
