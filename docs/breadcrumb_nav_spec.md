# Breadcrumb Nav Component Specification

## Component Name
Breadcrumb Nav

## Description
A breadcrumb navigation component that shows the user's current location within the application hierarchy. Used for improving navigation and providing context about the current page's position.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `items` | list | [] | List of breadcrumb items with label, href, and current properties |
| `separator` | atom | :chevron_right | Icon to use as separator between items |
| `show_home` | boolean | true | Whether to show the home link at the beginning |
| `home_path` | string | "/" | Path for the home link |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Variants
| Variant | Description | CSS Classes |
|---------|-------------|-------------|
| `default` | Standard breadcrumb navigation style | Uses consistent link styling and separators |

## CSS Classes and Styling Approach
### Base Classes
The component uses a nav element with an ordered list structure for semantic markup.

### CSS Variables Used
- `--foreground` - Text color for breadcrumb items
- `--muted-foreground` - Color for separators and non-current items
- `--primary` - Color for current item and hover states

## Accessibility Considerations
- Semantic nav element with aria-label="Breadcrumb"
- Ordered list structure for proper hierarchy
- Current page marked with aria-current="page"
- Proper contrast between text and background
- Links are focusable with visible focus states
- Separators are hidden from screen readers with aria-hidden="true"

## Usage Examples
```heex
<!-- Basic breadcrumb navigation -->
<.breadcrumb_nav
  items={[
    %{label: "Dashboard", href: "/dashboard"},
    %{label: "Products", href: "/products"},
    %{label: "Product Details", current: true}
  ]}
/>

<!-- Breadcrumb with custom separator -->
<.breadcrumb_nav
  items={[
    %{label: "Home", href: "/"},
    %{label: "Library", href: "/library"},
    %{label: "Data", current: true}
  ]}
  separator={:chevron_double_right}
/>

<!-- Breadcrumb without home link -->
<.breadcrumb_nav
  items={[
    %{label: "Products", href: "/products"},
    %{label: "Categories", href: "/categories"},
    %{label: "Electronics", current: true}
  ]}
  show_home={false}
/>

<!-- Breadcrumb with custom home path -->
<.breadcrumb_nav
  items={[
    %{label: "Projects", href: "/projects"},
    %{label: "Project Alpha", current: true}
  ]}
  home_path="/dashboard"
/>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Implements semantic HTML with nav and ol elements
- Supports global attributes for Phoenix LiveView events
- Fully responsive design
- Consistent with the design system using CSS variables
- Follows accessibility best practices with proper ARIA attributes
- Separator icons can be customized
- Home link can be hidden or customized