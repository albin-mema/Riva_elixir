# CardContent Component Specification

## Component Name
CardContent

## Description
A content section component for the Card component. Used for displaying the main content within a card.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
| Slot | Required | Description |
|------|----------|-------------|
| `inner_block` | Yes | The content to display inside the card content section |

## CSS Classes and Styling Approach
### Base Classes
```
p-6 pt-0
```

### CSS Variables Used
- No specific CSS variables are directly used in this component

## Accessibility Considerations
- Semantic div element with appropriate attributes
- ARIA attributes can be passed through via `rest` attribute
- Padding provides proper spacing from card edges
- Pt-0 ensures proper spacing when used after CardHeader
- Content should maintain proper heading hierarchy

## Usage Examples
```heex
<!-- Basic card with content -->
<Card>
  <Card.Header>
    <Card.Title>User Profile</Card.Title>
  </Card.Header>
  <Card.Content>
    <div class="space-y-4">
      <div>
        <Text variant="label">Name</Text>
        <p>John Doe</p>
      </div>
      <div>
        <Text variant="label">Email</Text>
        <p>john@example.com</p>
      </div>
    </div>
  </Card.Content>
</Card>

<!-- Card content with custom styling -->
<Card>
  <Card.Header>
    <Card.Title>Statistics</Card.Title>
  </Card.Header>
  <Card.Content class="grid grid-cols-3 gap-4">
    <div class="text-center">
      <p class="text-2xl font-bold">24</p>
      <p class="text-sm text-muted-foreground">Users</p>
    </div>
    <div class="text-center">
      <p class="text-2xl font-bold">12</p>
      <p class="text-sm text-muted-foreground">Projects</p>
    </div>
    <div class="text-center">
      <p class="text-2xl font-bold">8</p>
      <p class="text-sm text-muted-foreground">Tasks</p>
    </div>
  </Card.Content>
</Card>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Fully responsive design
- Content is passed via inner block for maximum flexibility
- Padding of p-6 provides consistent spacing from card edges
- Pt-0 ensures proper spacing when following CardHeader (which has p-6)
- Designed to work within the Card component
- Can contain any type of content including text, forms, lists, etc.
- Flexible container that adapts to its content