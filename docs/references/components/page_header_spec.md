# Page Header Component Specification

## Component Name
Page Header

## Description
A page header component for displaying page titles, descriptions, breadcrumbs, badges, and actions. Provides a consistent header experience across different pages and contexts.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `title` | string | "" | Page title to display |
| `description` | string | "" | Page description or subtitle |
| `icon` | atom | nil | Icon to display next to the title |
| `breadcrumbs` | list | [] | List of breadcrumb items |
| `variant` | string | "default" | Style variant (default, compact, card) |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
| Slot | Required | Description |
|------|----------|-------------|
| `badge` | No | Badge element to display next to the title |
| `action` | No | Action elements to display in the header |

## Variants
| Variant | Description | Styling |
|---------|-------------|---------|
| `default` | Standard page header | Full width with description |
| `compact` | Compact header for smaller sections | Reduced spacing and smaller text |
| `card` | Card-style header | Bordered container with background |

## CSS Classes and Styling Approach
### Base Classes
The component uses a header container with title, description, and action areas, applying consistent styling to all elements.

### CSS Variables Used
- `--background` / `--foreground` - Background and text colors
- `--border` - Border color
- `--muted` / `--muted-foreground` - Colors for secondary text
- `--radius` - Border radius

## Accessibility Considerations
- Semantic HTML structure with appropriate heading levels
- Sufficient color contrast for all text elements
- Proper labeling of breadcrumb navigation
- Focus management for action buttons
- Screen reader-friendly content organization
- Breadcrumb navigation follows ARIA best practices
- Icons are decorative and hidden from screen readers with aria-hidden
- Title uses appropriate heading level (h1)

## Usage Examples
```heex
<!-- Default page header with badge and action -->
<.page_header
  title="Business Management"
  description="Manage your business entities and their information"
>
  <:badge>
    <.badge variant="outline">
      5 businesses
    </.badge>
  </:badge>

  <:action>
    <.button variant="default">
      <.icon name={:plus} class="mr-2 h-4 w-4" />
      Add Business
    </.button>
  </:action>
</.page_header>

<!-- Page header with icon and breadcrumbs -->
<.page_header
  title="Dashboard"
  icon={:home}
  breadcrumbs={[
    %{label: "Home", href: "/"},
    %{label: "Dashboard", current: true}
  ]}
/>

<!-- Compact page header -->
<.page_header
  title="User Profile"
  description="View and edit your profile information"
  variant="compact"
>
  <:action>
    <.button variant="outline">
      <.icon name={:pencil} class="mr-2 h-4 w-4" />
      Edit Profile
    </.button>
  </:action>
</.page_header>

<!-- Card variant page header -->
<.page_header
  title="Reservation Details"
  description="View and manage reservation information"
  variant="card"
>
  <:badge>
    <.badge variant="success">
      Confirmed
    </.badge>
  </:badge>

  <:action>
    <.button variant="outline">
      <.icon name={:printer} class="mr-2 h-4 w-4" />
      Print
    </.button>
    <.button variant="default">
      <.icon name={:pencil} class="mr-2 h-4 w-4" />
      Edit
    </.button>
  </:action>
</.page_header>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Consistent with the design system using CSS variables
- Follows accessibility best practices with semantic HTML
- Integrates with breadcrumb navigation component
- Supports multiple variants for different contexts
- Action slots allow for flexible button arrangements
- Badge slots enable status indicators
- Icon support provides visual cues for page types
- Breadcrumb navigation enhances user orientation
- Component adapts to different screen sizes with responsive design
- Description text provides additional context
- Title is prominently displayed as the main heading
- Variants offer different styling options for different use cases
- Compact variant saves vertical space
- Card variant provides visual separation