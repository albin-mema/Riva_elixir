# Notification Toast Component Specification

## Component Name
Notification Toast

## Description
A toast notification component for displaying temporary messages to users. Provides feedback on user actions or system events with different types and positions.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `message` | string | "" | Message content for the toast |
| `title` | string | "" | Optional title for the toast |
| `type` | string | "default" | Type of notification (default, success, error, warning, info) |
| `show` | boolean | false | Whether the toast is visible |
| `dismissible` | boolean | true | Whether the toast can be dismissed by the user |
| `duration` | integer | 5000 | Duration in milliseconds before auto-dismiss (0 for no auto-dismiss) |
| `position` | string | "top-right" | Position of the toast (top-left, top-right, bottom-left, bottom-right) |
| `on_close` | string | "" | Event to send when toast is closed |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Types
| Type | Description | Color Scheme |
|------|-------------|--------------|
| `default` | Standard notification | Neutral colors |
| `success` | Success messages | Green color scheme |
| `error` | Error messages | Red color scheme |
| `warning` | Warning messages | Yellow/orange color scheme |
| `info` | Informational messages | Blue color scheme |

## Positions
| Position | Description | CSS Classes |
|----------|-------------|-------------|
| `top-left` | Top left corner of viewport | Fixed positioning |
| `top-right` | Top right corner of viewport | Fixed positioning |
| `bottom-left` | Bottom left corner of viewport | Fixed positioning |
| `bottom-right` | Bottom right corner of viewport | Fixed positioning |

## CSS Classes and Styling Approach
### Base Classes
The component uses a fixed position container with animation effects for entry and exit.

### CSS Variables Used
- `--background` / `--foreground` - Background and text colors
- `--success` / `--success-foreground` - Colors for success type |
- `--destructive` / `--destructive-foreground` - Colors for error type
- `--warning` / `--warning-foreground` - Colors for warning type
- `--primary` / `--primary-foreground` - Colors for info type
- `--border` - Border color
- `--radius` - Border radius

## Accessibility Considerations
- Proper ARIA roles (alert, status) for notification types
- Focus management that doesn't interrupt user workflow
- Sufficient color contrast for text and background
- Dismissible notifications have clear close buttons
- Screen reader announcements for notification content
- Auto-dismissal respects user preference for reduced motion
- Toasts are positioned to avoid overlapping with other content

## Usage Examples
```heex
<!-- Basic notification toast -->
<.notification_toast
  message="This is a notification message."
  show={@show_notification}
/>

<!-- Notification toast with title -->
<.notification_toast
  title="Notification Title"
  message="This is a notification with a title."
  show={@show_notification}
/>

<!-- Success notification -->
<.notification_toast
  message="Operation completed successfully."
  type="success"
  show={@show_success}
/>

<!-- Error notification -->
<.notification_toast
  message="An error occurred while processing your request."
  type="error"
  show={@show_error}
/>

<!-- Warning notification -->
<.notification_toast
  message="Please review the information before proceeding."
  type="warning"
  show={@show_warning}
/>

<!-- Info notification -->
<.notification_toast
  message="This is an informational message."
  type="info"
  show={@show_info}
/>

<!-- Non-dismissible toast -->
<.notification_toast
  message="This notification cannot be dismissed."
  show={@show_notification}
  dismissible={false}
/>

<!-- Toast with custom position -->
<.notification_toast
  message="Top Right Notification"
  show={@show_notification}
  position="top-right"
/>

<!-- Toast with custom duration -->
<.notification_toast
  message="This will dismiss after 10 seconds."
  show={@show_notification}
  duration={10000}
/>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Implements proper animation for entry and exit
- Supports global attributes for Phoenix LiveView events
- All types follow consistent styling patterns
- Consistent with the design system using CSS variables
- Follows accessibility best practices with proper ARIA attributes
- Positioning uses fixed positioning relative to viewport
- Auto-dismissal can be configured or disabled
- Dismissible notifications show a close button
- Toasts stack appropriately when multiple are shown
- Duration can be customized or disabled for persistent notifications