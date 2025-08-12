# CardTitle Component Specification

## Component Name
CardTitle

## Description
A title component for the CardHeader section. Used for displaying the main heading within a card.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
| Slot | Required | Description |
|------|----------|-------------|
| `inner_block` | Yes | The content to display as the card title |

## CSS Classes and Styling Approach
### Base Classes
```
text-lg font-semibold leading-none tracking-tight
```

### CSS Variables Used
- No specific CSS variables are directly used in this component

## Accessibility Considerations
- Semantic h3 element with appropriate attributes (can be overridden)
- ARIA attributes can be passed through via `rest` attribute
- Proper heading hierarchy should be maintained
- Font size and weight provide good readability
- Leading-none ensures tight spacing for single line titles

## Usage Examples
```heex
<!-- Basic card title -->
<Card>
  <Card.Header>
    <Card.Title>Profile Information</Card.Title>
  </Card.Header>
  <Card.Content>
    <p>Card content goes here</p>
  </Card.Content>
</Card>

<!-- Card title with custom styling -->
<Card>
  <Card.Header>
    <Card.Title class="text-xl text-primary">Important Notice</Card.Title>
  </Card.Header>
  <Card.Content>
    <p>Notice content</p>
  </Card.Content>
</Card>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Fully responsive design
- Content is passed via inner block for maximum flexibility
- Text-lg provides appropriate font size
- Font-semibold gives proper weight for headings
- Leading-none and tracking-tight create a compact, focused appearance
- Designed to work within the CardHeader component
- Semantic h3 element by default but can be overridden