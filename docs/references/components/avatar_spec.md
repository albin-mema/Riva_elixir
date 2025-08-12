# Avatar Component Specification

## Component Name
Avatar

## Description
An avatar component for displaying user or business images with fallbacks to initials or icons. Supports various sizes, shapes, and status indicators.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `src` | string | nil | The image source URL |
| `alt` | string | "" | Alternative text for the image |
| `initials` | string | nil | Initials to display as fallback |
| `name` | string | nil | Name to generate initials from (if initials not provided) |
| `size` | string | "md" | The avatar size (xs, sm, md, lg, xl, 2xl) |
| `shape` | string | "circle" | The avatar shape (circle, square, rounded) |
| `status` | string | nil | Status indicator (online, away, busy, offline) |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Variants
This component does not have explicit variants, but supports different shapes and status indicators.

## Sizes
| Size | Description | CSS Classes (Container) | CSS Classes (Text) | CSS Classes (Status) |
|------|-------------|-------------------------|--------------------|----------------------|
| `xs` | Extra small avatar | `h-6 w-6 text-xs` | `text-xs` | `h-2 w-2` |
| `sm` | Small avatar | `h-8 w-8 text-sm` | `text-sm` | `h-2.5 w-2.5` |
| `md` | Medium avatar | `h-10 w-10 text-base` | `text-base` | `h-3 w-3` |
| `lg` | Large avatar | `h-12 w-12 text-lg` | `text-lg` | `h-3.5 w-3.5` |
| `xl` | Extra large avatar | `h-16 w-16 text-xl` | `text-xl` | `h-4 w-4` |
| `2xl` | 2x extra large avatar | `h-20 w-20 text-2xl` | `text-2xl` | `h-5 w-5` |

## Shapes
| Shape | Description | CSS Classes |
|-------|-------------|-------------|
| `circle` | Circular avatar | `rounded-full` |
| `square` | Square avatar | `rounded-none` |
| `rounded` | Rounded square avatar | `rounded-md` |

## Status Indicators
| Status | Description | CSS Classes |
|--------|-------------|-------------|
| `online` | User is online | `bg-green-500` |
| `away` | User is away | `bg-yellow-500` |
| `busy` | User is busy | `bg-red-500` |
| `offline` | User is offline | `bg-gray-400` |

## CSS Classes and Styling Approach
### Base Container Classes
```
relative inline-flex items-center justify-center overflow-hidden bg-muted
```

### Initials Classes
```
font-medium text-foreground select-none
```

### Status Indicator Classes
```
absolute border-2 border-background rounded-full
```

### CSS Variables Used
- `--muted` - Background color for fallback
- `--foreground` - Text color for initials
- `--background` - Border color for status indicator
- `--radius` - Border radius for rounded shape (used via rounded-md class)

## Accessibility Considerations
- Proper alt text for images
- Sufficient color contrast for initials and background
- Semantic div element with appropriate attributes
- ARIA attributes can be passed through via `rest` attribute
- Status indicators should have appropriate ARIA labels when meaningful
- Images have appropriate loading behavior
- Text content is readable at all sizes

## Usage Examples
```heex
<!-- Avatar with image -->
<Avatar src="/images/user.jpg" alt="John Doe" />

<!-- Avatar with initials -->
<Avatar initials="JD" />

<!-- Avatar with name (generates initials) -->
<Avatar name="John Doe" />

<!-- Small circular avatar -->
<Avatar size="sm" initials="JD" />

<!-- Large rounded avatar -->
<Avatar size="lg" shape="rounded" src="/images/user.jpg" alt="John Doe" />

<!-- Avatar with status -->
<Avatar initials="JD" status="online" />

<!-- Square avatar with offline status -->
<Avatar shape="square" initials="JD" status="offline" />
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- All sizes and shapes are fully responsive
- Consistent with the design system using CSS variables
- Follows accessibility best practices with proper alt text
- Supports three fallback mechanisms: image → initials → icon
- Status indicators are positioned absolutely in the bottom-right
- Border-2 with background color creates a clean separation for status indicators
- Initials are center-aligned with font-medium for better readability
- Overflow-hidden ensures content stays within bounds
- Select-none prevents text selection on initials