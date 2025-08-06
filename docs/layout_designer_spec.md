# Layout Designer Component Specification

## Component Name
Layout Designer

## Description
A layout designer component for creating and editing grid-based layouts. Provides a visual interface for positioning items on a grid with drag-and-drop functionality and grid customization options.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `layout` | map | %{id: "", name: ""} | Layout information with id and name |
| `items` | list | [] | List of items positioned on the grid |
| `grid_rows` | integer | 10 | Number of rows in the grid |
| `grid_columns` | integer | 10 | Number of columns in the grid |
| `on_item_move` | string | "" | Event to send when an item is moved |
| `on_item_add` | string | "" | Event to send when an item is added |
| `on_item_remove` | string | "" | Event to send when an item is removed |
| `on_grid_resize` | string | "" | Event to send when grid is resized |
| `editable` | boolean | false | Whether the layout is editable |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Item Object Structure
| Field | Type | Description |
|-------|------|-------------|
| `id` | string | Unique identifier for the item |
| `name` | string | Display name of the item |
| `row` | integer | Grid row position (1-based) |
| `column` | integer | Grid column position (1-based) |

## CSS Classes and Styling Approach
### Base Classes
The component uses a grid container with positioned items, applying consistent styling to all elements.

### CSS Variables Used
- `--background` / `--foreground` - Background and text colors
- `--border` - Border color
- `--primary` / `--primary-foreground` - Colors for primary actions
- `--secondary` / `--secondary-foreground` - Colors for secondary actions
- `--muted` / `--muted-foreground` - Colors for secondary text
- `--radius` - Border radius

## Accessibility Considerations
- Semantic HTML structure with appropriate elements
- Sufficient color contrast for all text elements
- Focus management for interactive elements
- Keyboard navigation support for item movement
- Screen reader-friendly item descriptions
- Editable state indicated with appropriate ARIA attributes
- Grid dimensions announced for screen reader users
- Visual feedback for drag-and-drop operations

## Usage Examples
```heex
<!-- Editable layout designer -->
<.layout_designer
  layout={%{
    id: "layout-1",
    name: "Beach Resort Layout"
  }}
  items={[
    %{id: "1", name: "Umbrella 1", row: 2, column: 3},
    %{id: "2", name: "Sunbed 1", row: 2, column: 4},
    %{id: "3", name: "Cabin 1", row: 5, column: 7}
  ]}
  grid_rows={10}
  grid_columns={10}
  on_item_move="move_item"
  on_item_add="add_item"
  on_item_remove="remove_item"
  on_grid_resize="resize_grid"
  editable={true}
/>

<!-- Read-only layout designer -->
<.layout_designer
  layout={%{
    id: "layout-2",
    name: "Pool Area Layout"
  }}
  items={[
    %{id: "4", name: "Sunbed 2", row: 3, column: 2},
    %{id: "5", name: "Sunbed 3", row: 3, column: 3},
    %{id: "6", name: "Cabin 2", row: 7, column: 5}
  ]}
  grid_rows={8}
  grid_columns={8}
  on_item_move="move_item"
  on_item_add="add_item"
  on_item_remove="remove_item"
  on_grid_resize="resize_grid"
  editable={false}
/>

<!-- Empty grid layout designer -->
<.layout_designer
  layout={%{
    id: "layout-3",
    name: "Empty Layout"
  }}
  items={[]}
  grid_rows={5}
  grid_columns={5}
  on_item_move="move_item"
  on_item_add="add_item"
  on_item_remove="remove_item"
  on_grid_resize="resize_grid"
  editable={true}
/>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Consistent with the design system using CSS variables
- Follows accessibility best practices with semantic HTML
- Grid-based layout with configurable rows and columns
- Items can be positioned anywhere on the grid
- Editable mode enables drag-and-drop functionality
- Read-only mode displays layout without interaction
- Item movement triggers on_item_move event with new position
- Grid resizing triggers on_grid_resize event with new dimensions
- Item addition/removal triggers corresponding events
- Component adapts to different screen sizes with responsive design
- Visual feedback during drag operations
- Grid cells are clearly defined with borders
- Items are visually distinct from grid background
- Layout name is displayed prominently
- Empty grids show grid lines for reference