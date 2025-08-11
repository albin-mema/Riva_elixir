# CardDescription Component Specification

## Component Name
CardDescription

## Description
A description component for the CardHeader section. Used for displaying supplementary information or context within a card.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
| Slot | Required | Description |
|------|----------|-------------|
| `inner_block` | Yes | The content to display as the card description |

## CSS Classes and Styling Approach
### Base Classes
```
text-sm text-muted-foreground
```

### CSS Variables Used
- `--muted-foreground` - Text color for description

## Accessibility Considerations
- Semantic p element with appropriate attributes
- ARIA attributes can be passed through via `rest` attribute
- Text-muted-foreground provides proper contrast while being subtle
- Text-sm ensures appropriate sizing relative to title
- Should provide meaningful context without being verbose

## Usage Examples
```heex
<!-- Basic card with description -->
<Card>
  <Card.Header>
    <Card.Title>Account Settings</Card.Title>
    <Card.Description>Manage your account preferences and security settings</Card.Description>
  </Card.Header>
  <Card.Content>
    <p>Settings content goes here</p>
  </Card.Content>
</Card>

<!-- Card description with custom styling -->
<Card>
  <Card.Header>
    <Card.Title>Notifications</Card.Title>
    <Card.Description class="text-xs">You have 3 unread messages</Card.Description>
  </Card.Header>
  <Card.Content>
    <p>Notification content</p>
  </Card.Content>
</Card>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Fully responsive design
- Content is passed via inner block for maximum flexibility
- Text-sm provides appropriate font size relative to title
- Text-muted-foreground gives subtle appearance while maintaining readability
- Designed to work within the CardHeader component
- Semantic p element by default
- Complements CardTitle in the header section