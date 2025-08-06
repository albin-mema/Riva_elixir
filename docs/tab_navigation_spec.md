# Tab Navigation Component Specification

## Component Name
Tab Navigation

## Description
A tab navigation component for switching between different views or sections of content. Provides a clear navigation interface with visual indication of the active tab.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `tabs` | list | [] | List of tab configurations with id and label |
| `active_tab` | string | "" | ID of the currently active tab |
| `on_tab_change` | string | "" | Event to send when tab is changed |
| `variant` | string | "default" | Style variant (default, pills, underline) |
| `size` | string | "md" | Size of the tab navigation (sm, md, lg) |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Sizes
| Size | Description | CSS Classes |
|------|-------------|-------------|
| `sm` | Small tab navigation | Smaller text and padding |
| `md` | Medium tab navigation | Default size |
| `lg` | Large tab navigation | Larger text and padding |

## Variants
| Variant | Description | Styling Approach |
|---------|-------------|------------------|
| `default` | Standard tab navigation | Box-style tabs with borders |
| `pills` | Pill-style tabs | Rounded tabs with background |
| `underline` | Underline-style tabs | Underlined active tab |

## CSS Classes and Styling Approach
### Base Classes
The component uses a container with tab buttons, applying consistent styling to all tabs and highlighting the active tab.

### CSS Variables Used
- `--background` / `--foreground` - Background and text colors
- `--primary` / `--primary-foreground` - Colors for active tab
- `--secondary` / `--secondary-foreground` - Colors for inactive tabs
- `--border` - Border color
- `--radius` - Border radius for pill variant

## Accessibility Considerations
- Proper ARIA attributes for tab navigation (role, aria-selected, aria-controls)
- Keyboard navigation support (arrow keys, Enter, Space)
- Semantic HTML structure with nav and tablist elements
- Sufficient color contrast for all text elements
- Active tab is clearly indicated visually and programmatically
- Focus management between tabs
- Tab panels should be properly associated with tabs
- Screen reader announcements for tab changes

## Usage Examples
```heex
<!-- Basic tab navigation -->
<.tab_navigation
  tabs={[
    %{id: "tab1", label: "Tab 1"},
    %{id: "tab2", label: "Tab 2"},
    %{id: "tab3", label: "Tab 3"}
  ]}
  active_tab="tab1"
  on_tab_change="change_tab"
/>

<!-- Pill-style tab navigation -->
<.tab_navigation
  tabs={[
    %{id: "home", label: "Home"},
    %{id: "profile", label: "Profile"},
    %{id: "settings", label: "Settings"}
  ]}
  active_tab="home"
  on_tab_change="change_tab"
  variant="pills"
/>

<!-- Underline-style tab navigation -->
<.tab_navigation
  tabs={[
    %{id: "overview", label: "Overview"},
    %{id: "analytics", label: "Analytics"},
    %{id: "reports", label: "Reports"}
  ]}
  active_tab="overview"
  on_tab_change="change_tab"
  variant="underline"
/>

<!-- Tab navigation in different sizes -->
<.tab_navigation
  tabs={[
    %{id: "small", label: "Small"},
    %{id: "tab", label: "Tab"}
  ]}
  active_tab="small"
  on_tab_change="change_tab"
  size="sm"
/>

<.tab_navigation
  tabs={[
    %{id: "medium", label: "Medium"},
    %{id: "tab", label: "Tab"}
  ]}
  active_tab="medium"
  on_tab_change="change_tab"
  size="md"
/>

<.tab_navigation
  tabs={[
    %{id: "large", label: "Large"},
    %{id: "tab", label: "Tab"}
  ]}
  active_tab="large"
  on_tab_change="change_tab"
  size="lg"
/>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Consistent with the design system using CSS variables
- Follows accessibility best practices with semantic HTML
- Tab selection triggers an event with the selected tab ID
- Active tab is visually distinct from inactive tabs
- All sizes follow consistent styling patterns
- Variants provide different visual styles for different contexts
- Component is responsive and works on all screen sizes
- Keyboard navigation follows ARIA best practices
- Tab IDs are used to associate tabs with content panels