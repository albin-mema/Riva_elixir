# Confirm Dialog Component Specification

## Component Name
Confirm Dialog

## Description
A modal dialog component used to confirm actions, particularly destructive or important operations. Presents users with a clear choice to proceed or cancel an action.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `title` | string | "" | Title for the dialog |
| `message` | string | "" | Message content for the dialog |
| `confirm_label` | string | "Confirm" | Label for the confirm button |
| `cancel_label` | string | "Cancel" | Label for the cancel button |
| `variant` | string | "default" | Style variant (default, destructive, warning, info) |
| `on_confirm` | string | "" | Event to send when confirm is clicked |
| `on_cancel` | string | "" | Event to send when cancel is clicked |
| `show` | boolean | false | Whether the dialog is visible |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Variants
| Variant | Description | CSS Classes |
|---------|-------------|-------------|
| `default` | Standard confirm dialog style | Uses default button styling |
| `destructive` | For destructive actions like delete | Uses destructive button styling |
| `warning` | For cautionary actions | Uses warning button styling |
| `info` | For informational confirmations | Uses info button styling |

## CSS Classes and Styling Approach
### Base Classes
The component uses a modal overlay with a dialog container and applies variant-specific styling to buttons.

### CSS Variables Used
- `--background` / `--foreground` - Background and text colors
- `--destructive` / `--destructive-foreground` - Colors for destructive variant
- `--warning` / `--warning-foreground` - Colors for warning variant
- `--primary` / `--primary-foreground` - Colors for primary actions
- `--border` - Border color
- `--radius` - Border radius

## Accessibility Considerations
- Proper focus management with focus trap within dialog
- Escape key to close dialog
- Semantic dialog role with aria-modal
- Proper labeling with aria-labelledby and aria-describedby
- Focus returns to triggering element after closing
- High contrast text for readability
- Buttons have proper focus states
- Screen reader announcements for dialog state

## Usage Examples
```heex
<!-- Basic confirm dialog -->
<.confirm_dialog
  title="Confirm Action"
  message="Are you sure you want to perform this action?"
  on_confirm="confirm_action"
  on_cancel="cancel_action"
  show={@show_confirm_dialog}
/>

<!-- Destructive confirm dialog -->
<.confirm_dialog
  title="Delete Item"
  message="This action cannot be undone. Are you sure you want to delete this item?"
  confirm_label="Delete"
  cancel_label="Cancel"
  variant="destructive"
  on_confirm="delete_item"
  on_cancel="cancel_delete"
  show={@show_delete_dialog}
/>

<!-- Warning confirm dialog -->
<.confirm_dialog
  title="Warning"
  message="This action may have unintended consequences. Please proceed with caution."
  confirm_label="Proceed"
  cancel_label="Go Back"
  variant="warning"
  on_confirm="proceed_action"
  on_cancel="go_back"
  show={@show_warning_dialog}
/>

<!-- Info confirm dialog -->
<.confirm_dialog
  title="Information"
  message="Please confirm that you have reviewed all the information before proceeding."
  confirm_label="Continue"
  cancel_label="Review"
  variant="info"
  on_confirm="continue_action"
  on_cancel="review_info"
  show={@show_info_dialog}
/>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Implements proper focus management and keyboard navigation
- Supports global attributes for Phoenix LiveView events
- All variants follow consistent styling patterns
- Consistent with the design system using CSS variables
- Follows accessibility best practices with proper ARIA attributes
- Modal overlay prevents interaction with background content
- Buttons use the standard button component for consistency