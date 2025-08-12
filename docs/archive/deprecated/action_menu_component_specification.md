# Action Menu Component Specification

## Component Name
Action Menu

## Description
A dropdown action menu component that displays a list of actions when triggered. Used for providing users with contextual actions related to an item or section.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `actions` | list | [] | List of action items with label and action properties |
| `trigger_label` | string | "Actions" | Label for the trigger button |
| `trigger_icon` | atom | nil | Icon to display in the trigger button |
| `position` | string | "bottom-left" | Position of the dropdown menu (top-left, top-right, bottom-left, bottom-right) |
| `size` | string | "md" | Size of the trigger button (sm, md, lg) |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Variants
| Variant | Description | CSS Classes |
|---------|-------------|-------------|
| `default` | Standard action menu style | Uses button component variants |

## Sizes
| Size | Description | CSS Classes |
|------|-------------|-------------|
| `sm` | Small trigger button | Uses button component small size |
| `md` | Medium trigger button | Uses button component default size |
| `lg` | Large trigger button | Uses button component large size |

## CSS Classes and Styling Approach
### Base Classes
The component uses the button component for the trigger and applies dropdown-specific styling for the menu.

### CSS Variables Used
- Inherits CSS variables from the button component
- `--background` / `--foreground` - Background and text colors
- `--border` - Border color
- `--radius` - Border radius

## Accessibility Considerations
- Proper focus management between trigger and menu items
- Keyboard navigation support (arrow keys, Enter, Escape)
- ARIA attributes for dropdown state (aria-expanded, aria-haspopup)
- Semantic HTML structure with button trigger and list menu
- Focus trapping within the open menu
- Screen reader announcements for menu state changes

## Usage Examples
```heex
<!-- Basic action menu -->
<.action_menu
  actions={[
    %{label: "Edit", action: "edit"},
    %{label: "Delete", action: "delete"},
    %{label: "View Details", action: "view"}
  ]}
/>

<!-- Action menu with custom trigger -->
<.action_menu
  trigger_label="More Options"
  trigger_icon={:cog}
  actions={[
    %{label: "Settings", action: "settings"},
    %{label: "Preferences", action: "preferences"},
    %{label: "Help", action: "help"}
  ]}
/>

<!-- Action menu with different position -->
<.action_menu
  actions={[%{label: "Top Right", action: "top-right"}]}
  position="top-right"
  trigger_label="Top Right"
/>

<!-- Action menu with different size -->
<.action_menu
  actions={[%{label: "Large", action: "large"}]}
  size="lg"
  trigger_label="Large Menu"
/>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Implements proper focus management and keyboard navigation
- Supports global attributes for Phoenix LiveView events
- Positioning is handled with CSS classes
- All sizes are fully responsive
- Consistent with the design system using CSS variables
- Follows accessibility best practices with proper ARIA attributes
- Menu items are rendered as a list for semantic structure