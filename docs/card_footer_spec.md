# CardFooter Component Specification

## Component Name
CardFooter

## Description
A footer section component for the Card component. Used for displaying actions or additional information at the bottom of a card.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
| Slot | Required | Description |
|------|----------|-------------|
| `inner_block` | Yes | The content to display inside the card footer |

## CSS Classes and Styling Approach
### Base Classes
```
flex items-center p-6 pt-0
```

### CSS Variables Used
- No specific CSS variables are directly used in this component

## Accessibility Considerations
- Semantic div element with appropriate attributes
- ARIA attributes can be passed through via `rest` attribute
- Padding provides proper spacing from card edges
- Pt-0 ensures proper spacing when used after CardContent
- Flex layout allows for proper alignment of footer elements
- Interactive elements should have proper focus states

## Usage Examples
```heex
<!-- Basic card with footer -->
<Card>
  <Card.Header>
    <Card.Title>Confirm Action</Card.Title>
  </Card.Header>
  <Card.Content>
    <p>Are you sure you want to proceed with this action?</p>
  </Card.Content>
  <Card.Footer>
    <Button variant="secondary" class="mr-2">Cancel</Button>
    <Button variant="destructive">Delete</Button>
  </Card.Footer>
</Card>

<!-- Card footer with custom styling -->
<Card>
  <Card.Header>
    <Card.Title>Newsletter</Card.Title>
  </Card.Header>
  <Card.Content>
    <Input placeholder="Enter your email" />
  </Card.Content>
  <Card.Footer class="justify-between">
    <Text variant="small" color="muted">We respect your privacy</Text>
    <Button>Subscribe</Button>
  </Card.Footer>
</Card>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Fully responsive design
- Content is passed via inner block for maximum flexibility
- Padding of p-6 provides consistent spacing from card edges
- Pt-0 ensures proper spacing when following CardContent (which has p-6)
- Flex layout with items-center provides proper alignment of footer elements
- Designed to work within the Card component
- Commonly used for action buttons or additional information
- Can be customized with additional classes for different layouts (e.g., justify-between)