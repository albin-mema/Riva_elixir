# Calendar View Component Specification

## Component Name
Calendar View

## Description
A calendar component for displaying events and appointments in day, week, or month views. Provides navigation between time periods and interaction with calendar events.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `events` | list | [] | List of event data with title and date |
| `current_date` | string | "" | Current date being displayed |
| `view_mode` | string | "month" | Current view mode (day, week, month) |
| `on_date_click` | string | "" | Event to send when a date is clicked |
| `on_event_click` | string | "" | Event to send when an event is clicked |
| `on_view_change` | string | "" | Event to send when view mode changes |
| `on_navigate` | string | "" | Event to send when navigating between periods |
| `editable` | boolean | false | Whether events can be edited |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## View Modes
| Mode | Description | Display Format |
|------|-------------|----------------|
| `day` | Single day view | Hourly breakdown of events |
| `week` | Weekly view | Days of the week with events |
| `month` | Monthly view | Calendar grid with dates and events |

## CSS Classes and Styling Approach
### Base Classes
The component uses a container with navigation controls and calendar grid, applying consistent styling to all elements.

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
- Keyboard navigation support for dates and events
- Proper labeling of navigation controls
- Focus management between calendar elements
- Screen reader-friendly date and event announcements
- ARIA attributes for current view and selected dates
- Visual indicators for today's date
- Event titles are accessible to screen readers

## Usage Examples
```heex
<!-- Month view calendar -->
<.calendar_view
  events={[
    %{title: "Meeting with client", date: "2024-06-15"},
    %{title: "Team lunch", date: "2024-06-18"},
    %{title: "Project deadline", date: "2024-06-20"}
  ]}
  current_date="June 2024"
  view_mode="month"
  on_date_click="date_clicked"
  on_event_click="event_clicked"
  on_view_change="view_changed"
  on_navigate="navigate"
  editable={false}
/>

<!-- Week view calendar -->
<.calendar_view
  events={[
    %{title: "Team standup", date: "2024-06-17"},
    %{title: "Client presentation", date: "2024-06-19"}
  ]}
  current_date="Week of June 16, 2024"
  view_mode="week"
  on_date_click="date_clicked"
  on_event_click="event_clicked"
  on_view_change="view_changed"
  on_navigate="navigate"
  editable={true}
/>

<!-- Day view calendar -->
<.calendar_view
  events={[
    %{title: "Morning meeting", date: "2024-06-17 09:00"},
    %{title: "Lunch break", date: "2024-06-17 12:00"},
    %{title: "Code review", date: "2024-06-17 14:00"}
  ]}
  current_date="June 17, 2024"
  view_mode="day"
  on_date_click="date_clicked"
  on_event_click="event_clicked"
  on_view_change="view_changed"
  on_navigate="navigate"
  editable={true}
/>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Consistent with the design system using CSS variables
- Follows accessibility best practices with semantic HTML
- Supports day, week, and month view modes
- Events are displayed with appropriate styling based on view mode
- Navigation controls allow moving between time periods
- View mode can be changed dynamically
- Editable mode enables event modification
- Click events are sent for dates and events
- Current date is displayed prominently
- Component adapts to different screen sizes with responsive design
- Time-based events are properly positioned in day/week views
- All-day events are displayed appropriately in month view
- Navigation respects user preferences and maintains context