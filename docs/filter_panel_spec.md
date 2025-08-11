# Filter Panel Component Specification

## Component Name
Filter Panel

## Description
An advanced filtering interface component for data tables and lists. Provides users with a way to filter data based on multiple criteria with different input types.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `filters` | list | [] | List of filter configurations with type, label, key, and options |
| `values` | map | %{} | Current filter values |
| `on_apply` | string | "" | Event to send when apply button is clicked |
| `on_clear` | string | "" | Event to send when clear button is clicked |
| `collapsible` | boolean | true | Whether the panel can be collapsed |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Filter Types
| Type | Description | Implementation |
|------|-------------|----------------|
| `text` | Text input filter | Uses text input component |
| `select` | Dropdown selection filter | Uses select component |
| `date` | Date selection filter | Uses date picker component |

## CSS Classes and Styling Approach
### Base Classes
The component uses a container with form elements and applies consistent styling to filter inputs and action buttons.

### CSS Variables Used
- `--background` / `--foreground` - Background and text colors
- `--border` - Border color
- `--primary` / `--primary-foreground` - Colors for primary actions
- `--secondary` / `--secondary-foreground` - Colors for secondary actions
- `--radius` - Border radius

## Accessibility Considerations
- Proper labeling of form elements with associated labels
- Focus management between form elements
- Semantic HTML structure with form and fieldset elements
- Proper contrast between text and background
- Buttons have visible focus states
- Form elements are keyboard navigable
- ARIA attributes for collapsible functionality (if applicable)

## Usage Examples
```heex
<!-- Basic filter panel -->
<.filter_panel
  filters={[
    %{type: "text", label: "Name", key: "name"},
    %{type: "select", label: "Status", key: "status", options: ["Active", "Inactive", "Pending"]},
    %{type: "date", label: "Created Date", key: "created_date"}
  ]}
  values={@filter_values}
  on_apply="apply_filters"
  on_clear="clear_filters"
/>

<!-- Filter panel with initial values -->
<.filter_panel
  filters={[
    %{type: "text", label: "Name", key: "name"},
    %{type: "select", label: "Status", key: "status", options: ["Active", "Inactive", "Pending"]},
    %{type: "date", label: "Created Date", key: "created_date"}
  ]}
  values={%{
    "name" => "John",
    "status" => "Active"
  }}
  on_apply="apply_filters"
  on_clear="clear_filters"
/>

<!-- Non-collapsible filter panel -->
<.filter_panel
  filters={[
    %{type: "text", label: "Search", key: "search"},
    %{type: "select", label: "Category", key: "category", options: ["A", "B", "C"]}
  ]}
  values={@filter_values}
  on_apply="apply_filters"
  on_clear="clear_filters"
  collapsible={false}
/>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Implements different filter types using appropriate input components
- Consistent with the design system using CSS variables
- Follows accessibility best practices with semantic HTML
- Collapsible functionality allows for space-saving when not in use
- Apply and clear actions provide clear user interactions
- Filter values are managed as a map for easy state handling
- Each filter type has specific implementation details:
  - Text filters use standard text input
  - Select filters use dropdown with options
  - Date filters use date picker component