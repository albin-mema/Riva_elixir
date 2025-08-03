# CardHeader Component Specification

## Component Name
CardHeader

## Description
A header section component for the Card component. Used for organizing card content with a distinct header area.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
| Slot | Required | Description |
|------|----------|-------------|
| `inner_block` | Yes | The content to display inside the card header |

## CSS Classes and Styling Approach
### Base Classes
```
flex flex-col space-y-1.5 p-6
```

### CSS Variables Used
- No specific CSS variables are directly used in this component

## Accessibility Considerations
- Semantic div element with appropriate attributes
- ARIA attributes can be passed through via `rest` attribute
- Proper heading structure should be used within the header
- Sufficient spacing between elements (space-y-1.5)

## Usage Examples
```heex
<!-- Card with header -->
<Card>
  <Card.Header>
    <Card.Title>Notifications</Card.Title>
    <Card.Description>You have 3 unread messages</Card.Description>
  </Card.Header>
  <Card.Content>
    <p>Card content goes here</p>
  </Card.Content>
</Card>

<!-- Header with custom content -->
<Card>
  <Card.Header class="flex-row items-center justify-between">
    <div>
      <Card.Title>Account Settings</Card.Title>
      <Card.Description>Manage your account preferences</Card.Description>
    </div>
    <Badge>Updated</Badge>
  </Card.Header>
  <Card.Content>
    <p>Settings content</p>
  </Card.Content>
</Card>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Fully responsive design
- Content is passed via inner block for maximum flexibility
- Padding of p-6 provides consistent spacing
- Flex column layout with space-y-1.5 creates proper vertical spacing between elements
- Designed to work with CardTitle and CardDescription components
- Can be customized with additional classes for different layouts