# Data Table Component Specification

## Component Name
Data Table

## Description
A data table component for displaying tabular data with sorting, filtering, and pagination capabilities. Provides a flexible interface for viewing and interacting with structured data.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `items` | list | [] | List of data items to display in the table |
| `meta` | map | %{page: 1, per_page: 10, total_pages: 1, total_count: 0} | Pagination metadata |
| `path` | string | "" | Base path for pagination links |
| `id` | string | "" | Unique identifier for the table |
| `show_search` | boolean | true | Whether to show the search bar |
| `show_filters` | boolean | true | Whether to show the filter panel |
| `show_pagination` | boolean | true | Whether to show the pagination controls |
| `selectable` | boolean | false | Whether rows can be selected |
| `actions` | list | [] | List of action types to show (e.g., ["edit", "delete"]) |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
| Slot | Required | Description |
|------|----------|-------------|
| `col` | Yes | Column definitions with label and field attributes |

## Column Slot Attributes
| Attribute | Type | Required | Description |
|-----------|------|----------|-------------|
| `label` | string | Yes | Column header text |
| `field` | atom | Yes | Field name from item data to display |

## CSS Classes and Styling Approach
### Base Classes
The component uses a table container with search/filter controls, table element, and pagination, applying consistent styling to all elements.

### CSS Variables Used
- `--card` / `--card-foreground` - Background and text colors for table
- `--border` - Border color
- `--primary` / `--primary-foreground` - Colors for primary actions
- `--secondary` / `--secondary-foreground` - Colors for secondary actions
- `--muted` / `--muted-foreground` - Colors for secondary text
- `--radius` - Border radius

## Accessibility Considerations
- Semantic HTML structure with table, thead, tbody, and tr elements
- Proper column headers with scope attributes
- Sufficient color contrast for all text elements
- Focus management between table controls
- Screen reader-friendly pagination and search controls
- ARIA attributes for selectable rows
- Loading state indicated with aria-busy
- Sortable columns indicated with appropriate icons and labels
- Responsive design that works on different screen sizes

## Usage Examples
```heex
<!-- Basic data table -->
<.data_table
  items={[
    %{id: 1, name: "John Doe", email: "john@example.com", role: "Admin"},
    %{id: 2, name: "Jane Smith", email: "jane@example.com", role: "User"},
    %{id: 3, name: "Bob Johnson", email: "bob@example.com", role: "Manager"}
  ]}
  meta={%{
    page: 1,
    per_page: 10,
    total_pages: 5,
    total_count: 42
  }}
  path="/users"
  id="users-table"
  show_search={true}
  show_filters={true}
  show_pagination={true}
  selectable={false}
  actions={[]}
>
  <:col label="Name" field={:name} />
  <:col label="Email" field={:email} />
  <:col label="Role" field={:role} />
</.data_table>

<!-- Data table with actions -->
<.data_table
  items={[
    %{id: 1, name: "John Doe", email: "john@example.com", role: "Admin"},
    %{id: 2, name: "Jane Smith", email: "jane@example.com", role: "User"}
  ]}
  meta={%{
    page: 1,
    per_page: 10,
    total_pages: 1,
    total_count: 2
  }}
  path="/users"
  id="users-table-actions"
  show_search={true}
  show_filters={true}
  show_pagination={true}
  selectable={false}
  actions={["edit", "delete"]}
>
  <:col label="Name" field={:name} />
  <:col label="Email" field={:email} />
  <:col label="Role" field={:role} />
</.data_table>

<!-- Selectable data table -->
<.data_table
  items={[
    %{id: 1, name: "John Doe", email: "john@example.com", role: "Admin"},
    %{id: 2, name: "Jane Smith", email: "jane@example.com", role: "User"},
    %{id: 3, name: "Bob Johnson", email: "bob@example.com", role: "Manager"}
  ]}
  meta={%{
    page: 1,
    per_page: 10,
    total_pages: 5,
    total_count: 42
  }}
  path="/users"
  id="users-table-selectable"
  show_search={true}
  show_filters={true}
  show_pagination={true}
  selectable={true}
  actions={[]}
>
  <:col label="Name" field={:name} />
  <:col label="Email" field={:email} />
  <:col label="Role" field={:role} />
</.data_table>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Consistent with the design system using CSS variables
- Follows accessibility best practices with semantic HTML
- Integrates with pagination component for navigation
- Search and filter controls can be toggled
- Column definitions are provided via slots for flexibility
- Action buttons are displayed based on actions prop
- Selectable mode enables row selection with checkboxes
- Data items are displayed in a responsive table layout
- Pagination metadata controls page navigation
- Component adapts to different screen sizes with responsive design
- Loading state provides visual feedback during data fetching
- Sortable columns can be implemented with additional props
- Custom styling can be applied through class prop