# Alert Component Specification

## Component Name
Alert

## Description
An alert component for displaying important messages to users with different severity levels. Used for notifications, warnings, errors, and success messages.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `variant` | string | "default" | The alert style variant (default, destructive, success, warning) |
| `title` | string | nil | Optional title for the alert |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
| Slot | Required | Description |
|------|----------|-------------|
| `inner_block` | Yes | The content to display inside the alert |

## Variants
| Variant | Description | CSS Classes |
|---------|-------------|-------------|
| `default` | Standard alert style | `bg-background text-foreground` |
| `destructive` | Error/Destructive alert style | `border-destructive/50 text-destructive dark:border-destructive [&>svg]:text-destructive` |
| `success` | Success alert style | `border-[var(--chart-5)]/50 text-[var(--chart-5)] dark:border-[var(--chart-5)] [&>svg]:text-[var(--chart-5)]` |
| `warning` | Warning alert style | `border-amber-500/50 text-amber-500 dark:border-amber-500 [&>svg]:text-amber-500` |

## CSS Classes and Styling Approach
### Base Classes
```
relative w-full rounded-lg border p-4 [&>svg]:absolute [&>svg]:text-foreground [&>svg]:left-4 [&>svg]:top-4 [&>svg+div]:translate-y-[-3px] [&:has(svg)]:pl-11
```

### Title Classes
```
mb-1 font-medium leading-none tracking-tight
```

### Content Classes
```
text-sm [&_p]:leading-relaxed
```

### CSS Variables Used
- `--background` / `--foreground` - Background and text colors for default variant
- `--destructive` - Error state colors
- `--chart-5` - Success state colors
- `--border` - Border color for default variant
- `--radius` - Border radius (used via rounded-lg class)

## Accessibility Considerations
- Proper semantic structure with appropriate heading levels
- Sufficient color contrast for all text and background combinations
- ARIA attributes can be passed through via `rest` attribute
- Role="alert" or role="status" should be added for important notifications
- Focus management for interactive elements within alerts
- Text content should be clear and actionable

## Usage Examples
```heex
<!-- Default alert -->
<Alert>
  This is a default alert message.
</Alert>

<!-- Alert with title -->
<Alert title="Information">
  This is an informational alert with a title.
</Alert>

<!-- Destructive alert -->
<Alert variant="destructive" title="Error">
  Something went wrong. Please try again.
</Alert>

<!-- Success alert -->
<Alert variant="success" title="Success">
  Your changes have been saved successfully.
</Alert>

<!-- Warning alert -->
<Alert variant="warning" title="Warning">
  This action cannot be undone.
</Alert>

<!-- Alert with icon -->
<Alert variant="success">
  <Icon name={:check_circle} class="h-4 w-4" />
  <div>
    <Alert.Title>Success</Alert.Title>
    <p>Your account has been created successfully.</p>
  </div>
</Alert>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- All variants are fully responsive
- Consistent with the design system using CSS variables
- Follows accessibility best practices with proper semantic structure
- Content is passed via inner block for flexibility
- Title is optional but recommended for better organization
- Special styling when containing SVG icons
- Rounded-lg gives appropriate border radius
- Padding and spacing are consistent with design system