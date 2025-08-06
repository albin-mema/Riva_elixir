# Pagination Component Specification

## Component Name
Pagination

## Description
A pagination component for navigating through pages of data in tables and lists. Provides controls for moving between pages and optionally changing the number of items displayed per page.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `meta` | map | %{page: 1, per_page: 10, total_count: 0, total_pages: 0} | Pagination metadata with current page, items per page, total count, and total pages |
| `path` | string | "" | Base path for pagination links |
| `page_sizes` | list | [10, 25, 50, 100] | Available page size options |
| `show_page_size` | boolean | true | Whether to show the page size selector |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## CSS Classes and Styling Approach
### Base Classes
The component uses a flex container with navigation controls and page size selector, applying consistent styling to all elements.

### CSS Variables Used
- `--background` / `--foreground` - Background and text colors
- `--border` - Border color
- `--primary` / `--primary-foreground` - Colors for active/current page
- `--secondary` / `--secondary-foreground` - Colors for navigation buttons
- `--radius` - Border radius

## Accessibility Considerations
- Proper ARIA labels for navigation controls
- Semantic HTML structure with nav element
- Current page clearly indicated with aria-current
- Sufficient color contrast for all text elements
- Navigation buttons have visible focus states
- Page links are properly labeled for screen readers
- Page size selector has appropriate labeling
- Keyboard navigable controls

## Usage Examples
```heex
<!-- Basic pagination -->
<.pagination
  meta={%{page: 1, per_page: 10, total_count: 100, total_pages: 10}}
  path="/items"
/>

<!-- Pagination with custom page sizes -->
<.pagination
  meta={%{page: 3, per_page: 20, total_count: 100, total_pages: 5}}
  path="/items"
  page_sizes={[10, 20, 50, 100]}
/>

<!-- Pagination without page size selector -->
<.pagination
  meta={%{page: 2, per_page: 10, total_count: 50, total_pages: 5}}
  path="/items"
  show_page_size={false}
/>

<!-- Pagination with custom styling -->
<.pagination
  meta={%{page: 1, per_page: 10, total_count: 100, total_pages: 10}}
  path="/items"
  class="bg-gray-50 p-4 rounded-lg"
/>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Integrates with AshPhoenix pagination metadata
- Consistent with the design system using CSS variables
- Follows accessibility best practices with semantic HTML
- Page size selector allows users to change items per page
- Navigation controls include first, previous, next, and last page buttons
- Page numbers are displayed with current page highlighted
- Links are generated with proper query parameters for page and per_page
- All navigation controls are properly disabled when not applicable
- Page size selector shows current selection and available options
- Component adapts to different screen sizes with responsive design