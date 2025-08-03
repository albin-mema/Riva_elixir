# Dashboard Stats Component Specification

## Component Name
Dashboard Stats

## Description
A component for displaying key metrics and statistics in a dashboard format. Shows important business metrics with icons, values, labels, and change indicators in a grid layout.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `stats` | list | [] | List of stat objects with icon, value, label, and change |
| `loading` | boolean | false | Whether the component is in a loading state |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Stat Object Structure
| Field | Type | Description |
|-------|------|-------------|
| `icon` | atom | Icon to display for the stat |
| `value` | string | Formatted value to display |
| `label` | string | Label describing the stat |
| `change` | string | Change indicator (e.g., "+12%", "-3%") |

## CSS Classes and Styling Approach
### Base Classes
The component uses a grid container with stat cards, applying consistent styling to all elements.

### CSS Variables Used
- `--card` / `--card-foreground` - Background and text colors for cards
- `--border` - Border color
- `--primary` / `--primary-foreground` - Colors for positive changes
- `--destructive` / `--destructive-foreground` - Colors for negative changes
- `--muted` / `--muted-foreground` - Colors for secondary text
- `--radius` - Border radius

## Accessibility Considerations
- Semantic HTML structure with appropriate elements
- Sufficient color contrast for all text elements
- Icons are decorative and hidden from screen readers with aria-hidden
- Stat values and labels are properly associated
- Change indicators use color and text for accessibility
- Loading state indicated with aria-busy
- Grid layout is responsive and adapts to screen size
- Screen reader-friendly content organization

## Usage Examples
```heex
<!-- Dashboard stats -->
<.dashboard_stats
  stats={[
    %{icon: :users, value: "1,234", label: "Total Clients", change: "+12%"},
    %{icon: :calendar, value: "56", label: "Upcoming Reservations", change: "-3%"},
    %{icon: :currency_dollar, value: "$12,345", label: "Revenue", change: "+8%"},
    %{icon: :building_office, value: "24", label: "Active Businesses", change: "+2%"}
  ]}
  loading={false}
/>

<!-- Loading dashboard stats -->
<.dashboard_stats
  stats={[
    %{icon: :users, value: "1,234", label: "Total Clients", change: "+12%"},
    %{icon: :calendar, value: "56", label: "Upcoming Reservations", change: "-3%"},
    %{icon: :currency_dollar, value: "$12,345", label: "Revenue", change: "+8%"},
    %{icon: :building_office, value: "24", label: "Active Businesses", change: "+2%"}
  ]}
  loading={true}
/>

<!-- Dashboard stats with custom styling -->
<.dashboard_stats
  stats={[
    %{icon: :users, value: "1,234", label: "Total Clients", change: "+12%"},
    %{icon: :calendar, value: "56", label: "Upcoming Reservations", change: "-3%"},
    %{icon: :currency_dollar, value: "$12,345", label: "Revenue", change: "+8%"},
    %{icon: :building_office, value: "24", label: "Active Businesses", change: "+2%"}
  ]}
  loading={false}
  class="bg-gray-100 p-4 rounded-lg"
/>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Consistent with the design system using CSS variables
- Follows accessibility best practices with semantic HTML
- Stats are displayed in a responsive grid layout
- Icons provide visual cues for each stat type
- Values are formatted for readability
- Change indicators show trends with color coding
- Loading state provides visual feedback while data is being fetched
- Component adapts to different screen sizes with responsive design
- Positive changes are displayed in green, negative in red
- Stat cards use consistent styling with appropriate spacing
- Icons are sized appropriately for visual hierarchy