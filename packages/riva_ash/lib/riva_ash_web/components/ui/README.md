# UI Component Library

This is the UI component library for the Riva Ash project, built with accessibility and consistency in mind. The components are designed to work seamlessly with the project's design system based on OKLCH colors and CSS variables.

## Components

### Core Components

1. **Button** - For triggering actions
2. **Input** - For text input
3. **Checkbox** - For boolean choices
4. **Select** - For selecting from options
5. **Textarea** - For multi-line text input
6. **Badge** - For displaying status or labels
7. **Alert** - For important messages

### Card Components

1. **Card** - Container for grouping related content
2. **CardHeader** - Header section of a card
3. **CardTitle** - Title within a card header
4. **CardDescription** - Description within a card header
5. **CardContent** - Main content section of a card
6. **CardFooter** - Footer section of a card

## Usage

To use any component from the library, import the UI module:

```elixir
import RivaAshWeb.Components.UI
```

Then use any component in your HEEx templates:

```heex
<Button variant="primary">Click me</Button>
<Input placeholder="Enter text" />
<Checkbox label="Accept terms" />
<Select prompt="Choose an option" options={[{"Option 1", "1"}, {"Option 2", "2"}]} />
<Textarea placeholder="Enter description" rows="4" />
<Badge variant="secondary">New</Badge>
<Alert variant="success" title="Success">Operation completed successfully</Alert>
```

### Card Example

```heex
<Card>
  <Card.Header>
    <Card.Title>Notifications</Card.Title>
    <Card.Description>You have 3 unread messages</Card.Description>
  </Card.Header>
  <Card.Content>
    <p>Card content goes here</p>
  </Card.Content>
  <Card.Footer>
    <Button variant="secondary">Mark all as read</Button>
  </Card.Footer>
</Card>
```

## Design System

All components are built using the project's design system:

### Colors
- Primary: `var(--primary)` with `var(--primary-foreground)` for text
- Secondary: `var(--secondary)` with `var(--secondary-foreground)` for text
- Accent: `var(--accent)` with `var(--accent-foreground)` for text
- Destructive: `var(--destructive)` with `var(--destructive-foreground)` for text
- Background: `var(--background)` with `var(--foreground)` for text
- Muted: `var(--muted)` with `var(--muted-foreground)` for text
- Border: `var(--border)`
- Input: `var(--input)`
- Ring: `var(--ring)`

### Typography
- Font family: `var(--font-sans)`
- Font sizes: text-xs, text-sm, text-base, text-lg, etc.
- Font weights: font-normal, font-medium, font-semibold, font-bold

### Spacing
- Base unit: `var(--spacing)`
- Common spacing: gap-2, gap-4, space-y-2, space-y-4, etc.

### Radius
- Default: `var(--radius)`
- Small: `calc(var(--radius) - 2px)`
- Large: `calc(var(--radius) + 2px)`

### Shadows
- Small: `var(--shadow-sm)`
- Medium: `var(--shadow-md)`
- Large: `var(--shadow-lg)`

## Accessibility

All components are designed with accessibility in mind:
- Proper focus states
- Semantic HTML
- ARIA attributes where needed
- Keyboard navigation support
- Sufficient color contrast

## Contributing

To add new components or modify existing ones:
1. Create the component in the `ui` directory
2. Follow the existing patterns for props and styling
3. Use CSS variables consistently
4. Add a storybook entry
5. Update this documentation if needed