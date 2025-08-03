# Text Component Specification

## Component Name
Text

## Description
A versatile text component for consistent typography across the application. Supports various semantic elements, colors, alignments, and weights.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `variant` | string | "p" | The text variant (h1, h2, h3, h4, h5, h6, p, lead, small, label, caption, code) |
| `color` | string | "default" | The text color (default, primary, secondary, muted, destructive, success, warning) |
| `align` | string | "left" | The text alignment (left, center, right, justify) |
| `weight` | string | "normal" | The font weight (light, normal, medium, semibold, bold) |
| `italic` | boolean | false | Whether the text is italic |
| `required` | boolean | false | Whether to show a required indicator (for label variant) |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
| Slot | Required | Description |
|------|----------|-------------|
| `inner_block` | Yes | The content to display as text |

## Variants
| Variant | Description | Semantic Element | Base Classes |
|---------|-------------|------------------|--------------|
| `h1` | Page title | h1 | `text-4xl lg:text-5xl tracking-tight` |
| `h2` | Section heading | h2 | `text-3xl lg:text-4xl tracking-tight` |
| `h3` | Subsection heading | h3 | `text-2xl lg:text-3xl tracking-tight` |
| `h4` | Heading level 4 | h4 | `text-xl lg:text-2xl` |
| `h5` | Heading level 5 | h5 | `text-lg lg:text-xl` |
| `h6` | Heading level 6 | h6 | `text-base lg:text-lg` |
| `p` | Paragraph | p | `text-base leading-7` |
| `lead` | Lead paragraph | p | `text-xl text-muted-foreground` |
| `small` | Small text | p | `text-sm leading-6` |
| `label` | Form label | label | `text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70` |
| `caption` | Caption text | p | `text-xs text-muted-foreground` |
| `code` | Inline code | code | `font-mono text-sm bg-muted px-[0.3rem] py-[0.2rem] rounded` |

## Colors
| Color | Description | CSS Classes |
|-------|-------------|-------------|
| `default` | Default text color | `text-foreground` |
| `primary` | Primary color | `text-primary` |
| `secondary` | Secondary color | `text-secondary-foreground` |
| `muted` | Muted color | `text-muted-foreground` |
| `destructive` | Destructive color | `text-destructive` |
| `success` | Success color | `text-green-600 dark:text-green-500` |
| `warning` | Warning color | `text-yellow-600 dark:text-yellow-500` |

## Weights
| Weight | Description | CSS Classes |
|--------|-------------|-------------|
| `light` | Light font weight | `font-light` |
| `normal` | Normal font weight | `font-normal` |
| `medium` | Medium font weight | `font-medium` |
| `semibold` | Semibold font weight | `font-semibold` |
| `bold` | Bold font weight | `font-bold` |

## Alignments
| Alignment | Description | CSS Classes |
|-----------|-------------|-------------|
| `left` | Left aligned | `text-left` |
| `center` | Center aligned | `text-center` |
| `right` | Right aligned | `text-right` |
| `justify` | Justified | `text-justify` |

## CSS Classes and Styling Approach
### Base Classes
Determined by variant property (see Variants table above)

### Color Classes
Determined by color property (see Colors table above)

### Alignment Classes
Determined by align property (see Alignments table above)

### Weight Classes
Determined by weight property (see Weights table above)

### Style Classes
- Italic: `italic`

### Required Indicator Classes (for label variant)
```
text-destructive ml-1
```

### CSS Variables Used
- `--foreground` - Default text color
- `--primary` - Primary text color
- `--secondary-foreground` - Secondary text color
- `--muted-foreground` - Muted text color
- `--destructive` - Destructive text color
- `--radius` - Border radius for code variant (used via rounded class)

## Accessibility Considerations
- Proper semantic elements based on variant
- Sufficient color contrast for all text colors
- ARIA attributes can be passed through via `rest` attribute
- Required indicator for label variant is visually distinct
- Font sizes are responsive and readable
- Leading values provide good line spacing
- Code variant has appropriate background for contrast
- Italic text should be used sparingly for accessibility

## Usage Examples
```heex
<!-- Heading -->
<Text variant="h1">Page Title</Text>

<!-- Paragraph with custom color -->
<Text color="muted">This is some muted text.</Text>

<!-- Label with required indicator -->
<Text variant="label" required>Username</Text>

<!-- Centered text -->
<Text align="center">Centered content</Text>

<!-- Bold text -->
<Text weight="bold">Important information</Text>

<!-- Italic text -->
<Text italic>Emphasized content</Text>

<!-- Lead paragraph -->
<Text variant="lead">This is a lead paragraph to introduce the content.</Text>

<!-- Small text -->
<Text variant="small">This is small text for fine print.</Text>

<!-- Code text -->
<Text variant="code">console.log("Hello, world!");</Text>

<!-- Success message -->
<Text color="success" weight="medium">Operation completed successfully!</Text>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- All variants are fully responsive
- Consistent with the design system using CSS variables
- Follows accessibility best practices with proper semantic elements
- Content is passed via inner block for flexibility
- Variant determines the semantic element and base styling
- Color, alignment, and weight can be combined with any variant
- Required indicator only appears on label variant
- Code variant has special styling with background and padding
- Lead variant uses muted foreground for secondary content
- Small and caption variants use smaller font sizes
- Responsive text sizes adjust at different breakpoints
- Font weights provide visual hierarchy