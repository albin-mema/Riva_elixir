# Search Bar Component Specification

## Component Name
Search Bar

## Description
A search bar component with optional filters and suggestions. Provides users with a way to search for content with additional filtering options and real-time suggestions.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `value` | string | "" | Current search value |
| `placeholder` | string | "Search..." | Placeholder text for the search input |
| `loading` | boolean | false | Whether the search is in progress |
| `show_filters` | boolean | false | Whether to show the filter options |
| `filters` | list | [] | List of filter configurations with label, key, and options |
| `suggestions` | list | [] | List of search suggestions to display |
| `on_search` | string | "" | Event to send when search is performed |
| `on_change` | string | "" | Event to send when search value changes |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## CSS Classes and Styling Approach
### Base Classes
The component uses a container with search input and optional filter elements, applying consistent styling to all parts.

### CSS Variables Used
- `--background` / `--foreground` - Background and text colors
- `--border` - Border color
- `--primary` / `--primary-foreground` - Colors for primary actions
- `--secondary` / `--secondary-foreground` - Colors for secondary actions
- `--radius` - Border radius
- `--input` - Input field border color

## Accessibility Considerations
- Proper labeling of search input with aria-label
- Focus management between input and suggestions
- Keyboard navigation support for suggestions
- Loading state indicated with aria-busy
- Sufficient color contrast for all text elements
- Input field has proper focus states
- Suggestions are accessible via keyboard navigation
- Filter options are properly labeled
- Clear indication of active filters

## Usage Examples
```heex
<!-- Basic search bar -->
<.search_bar
  placeholder="Search..."
  on_search="perform_search"
/>

<!-- Search bar with value -->
<.search_bar
  value="search term"
  placeholder="Search..."
  on_search="perform_search"
/>

<!-- Search bar with filters -->
<.search_bar
  placeholder="Search..."
  show_filters={true}
  filters={[
    %{label: "Category", key: "category", options: ["A", "B", "C"]},
    %{label: "Status", key: "status", options: ["Active", "Inactive"]}
  ]}
  on_search="perform_search"
/>

<!-- Search bar in loading state -->
<.search_bar
  placeholder="Searching..."
  loading={true}
  on_search="perform_search"
/>

<!-- Search bar with suggestions -->
<.search_bar
  placeholder="Search for items..."
  suggestions={["Apple", "Banana", "Cherry", "Date"]}
  on_search="search_items"
/>

<!-- Search bar with custom styling -->
<.search_bar
  placeholder="Custom styled search..."
  on_search="custom_search"
  class="bg-blue-50 border-blue-200"
/>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Consistent with the design system using CSS variables
- Follows accessibility best practices with semantic HTML
- Loading state provides visual feedback during searches
- Filter options allow for additional search refinement
- Suggestions provide real-time search assistance
- Search value can be controlled or uncontrolled
- Events are sent for search actions and value changes
- Component adapts to different screen sizes with responsive design
- Filter panel can be toggled for space efficiency
- Suggestions are displayed in a dropdown list
- Keyboard navigation supports selecting suggestions