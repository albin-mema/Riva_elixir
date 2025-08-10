# Riva Ash UI Component Library Specifications

This document provides detailed specifications for all components in the Riva Ash UI Component Library, organized by category.

## Table of Contents
1. [Core Components](#core-components)
2. [Form Components](#form-components)
3. [Card Components](#card-components)
4. [Status Components](#status-components)
5. [Utility Components](#utility-components)
6. [Components to Update](#components-to-update)

## Core Components
w
### Button
A versatile button component that supports multiple variants, sizes, and states.

**Key Features:**
- Multiple variants: default, destructive, outline, secondary, ghost, link
- Multiple sizes: default, sm, lg
- Loading state support
- Full accessibility support

[View detailed specification](component_specifications/button_spec.md)

### Icon
An icon component that provides a consistent interface for rendering SVG icons.

**Key Features:**
- Multiple variants: outline, solid, mini, micro
- Multiple sizes: xs, sm, md, lg, xl
- Heroicons library integration
- Accessible with proper labeling

[View detailed specification](component_specifications/icon_spec.md)

## Form Components

### Input
A versatile input component for text entry with validation states.

**Key Features:**
- Form field integration with AshPhoenix forms
- Multiple variants: default, error, success
- Multiple sizes: default, sm, lg
- Full accessibility support

[View detailed specification](component_specifications/input_spec.md)

### Textarea
A textarea component for multi-line text input.

**Key Features:**
- Form field integration with AshPhoenix forms
- Multiple variants: default, error, success
- Multiple sizes: default, sm, lg
- Rows attribute for height control

[View detailed specification](component_specifications/textarea_spec.md)

### Checkbox
A checkbox component with labels and validation states.

**Key Features:**
- Label and description support
- Multiple variants: default, error, success
- Multiple sizes: default, sm, lg
- Form field integration

[View detailed specification](component_specifications/checkbox_spec.md)

### Radio
A radio button component for single selection.

**Key Features:**
- Label and description support
- Multiple variants: default, error, success
- Multiple sizes: sm, md, lg
- Form field integration

[View detailed specification](component_specifications/radio_spec.md)

### Select
A select dropdown component with options and prompts.

**Key Features:**
- Options and prompt support
- Multiple variants: default, error, success
- Multiple sizes: default, sm, lg
- Multiple selection support
- Form field integration

[View detailed specification](component_specifications/select_spec.md)

### DatePicker
A date picker component with calendar popup.

**Key Features:**
- Min/max date constraints
- Multiple variants: default, error, success
- Multiple sizes: sm, md, lg
- Format customization
- Form field integration

[View detailed specification](component_specifications/date_picker_spec.md)

### TimePicker
A time picker component with various formats.

**Key Features:**
- Min/max time constraints
- Multiple variants: default, error, success
- Multiple sizes: sm, md, lg
- 12/24 hour format support
- Step interval control
- Form field integration

[View detailed specification](component_specifications/time_picker_spec.md)

### Toggle
A toggle/switch component for boolean values.

**Key Features:**
- Label and description support
- Multiple variants: default, success, warning, destructive
- Multiple sizes: sm, md, lg
- Form field integration

[View detailed specification](component_specifications/toggle_spec.md)

## Card Components

### Card
A flexible container component for grouping related content.

**Key Features:**
- Consistent styling with design system
- Full accessibility support
- Flexible content container

[View detailed specification](component_specifications/card_spec.md)

### CardHeader
A header section component for the Card component.

**Key Features:**
- Flex layout for title and description
- Consistent padding
- Designed to work with CardTitle and CardDescription

[View detailed specification](component_specifications/card_header_spec.md)

### CardTitle
A title component for the CardHeader section.

**Key Features:**
- Semantic heading element
- Appropriate font sizing and weight
- Designed to work within CardHeader

[View detailed specification](component_specifications/card_title_spec.md)

### CardDescription
A description component for the CardHeader section.

**Key Features:**
- Subtle text styling
- Appropriate sizing relative to title
- Designed to work within CardHeader

[View detailed specification](component_specifications/card_description_spec.md)

### CardContent
A content section component for the Card component.

**Key Features:**
- Consistent padding
- Flexible container for any content
- Designed to work within Card

[View detailed specification](component_specifications/card_content_spec.md)

### CardFooter
A footer section component for the Card component.

**Key Features:**
- Flex layout for action buttons
- Consistent padding
- Designed to work within Card

[View detailed specification](component_specifications/card_footer_spec.md)

## Status Components

### Badge
A badge component for displaying status or labels.

**Key Features:**
- Multiple variants: default, secondary, destructive, outline
- Multiple sizes: default, sm, lg
- Compact pill-shaped design

[View detailed specification](component_specifications/badge_spec.md)

### Alert
An alert component for displaying important messages.

**Key Features:**
- Multiple variants: default, destructive, success, warning
- Optional title support
- Icon-ready styling
- Accessible with proper semantics

[View detailed specification](component_specifications/alert_spec.md)

### Spinner
A loading spinner component with various sizes.

**Key Features:**
- Multiple variants: default, primary, secondary
- Multiple sizes: xs, sm, md, lg, xl
- Accessible with proper labeling
- Smooth animation

[View detailed specification](component_specifications/spinner_spec.md)

### Text
A versatile text component for consistent typography.

**Key Features:**
- Multiple variants: h1-h6, p, lead, small, label, caption, code
- Multiple colors: default, primary, secondary, muted, destructive, success, warning
- Font weight and alignment options
- Required indicator for labels

[View detailed specification](component_specifications/text_spec.md)

## Utility Components

### Avatar
An avatar component for displaying user or business images.

**Key Features:**
- Image, initials, and icon fallbacks
- Multiple sizes: xs, sm, md, lg, xl, 2xl
- Multiple shapes: circle, square, rounded
- Status indicator support

[View detailed specification](component_specifications/avatar_spec.md)

### Tooltip
A tooltip component for displaying help text.

**Key Features:**
- Multiple positions: top, bottom, left, right
- Multiple triggers: hover, click, focus
- Customizable delay
- Accessible with proper ARIA attributes

[View detailed specification](component_specifications/tooltip_spec.md)

## Organisms

### InboxList
A composite component for displaying inbox items with various states and actions.

**Key Features:**
- Multiple variants: default, notification, task
- Multiple sizes: default, compact
- Responsive behavior (collapses to mobile view)
- Full accessibility support (ARIA attributes)
- Pagination integration
- Empty state support
- Mobile touch targets (≥44x44)

[View detailed specification](component_specifications/inbox_list_spec.md)


## Components to Update

### TextInput
A text input component that should be updated to follow UI library patterns.

**Issues:**
- Uses hardcoded colors instead of CSS variables
- Lacks validation states
- Missing form field integration
- Limited accessibility features

[View detailed specification](component_specifications/text_input_spec.md)

## Design System Consistency

All components follow these UI library patterns:

### Consistent Attribute Naming
- `variant` for style variants
- `size` for sizing options
- `class` for additional CSS classes
- `rest` for global attributes
- Form field integration via `field` attribute

### CSS Variables Usage
All components use the design system CSS variables:
- Colors: `--primary`, `--secondary`, `--accent`, `--destructive`, etc.
- Typography: `--font-sans`, `--font-serif`, `--font-mono`
- Spacing: `--spacing`, `--radius`
- Shadows: `--shadow-sm`, `--shadow-md`, `--shadow-lg`

### Accessibility Features
- Proper focus states with visible focus rings
- Semantic HTML elements
- ARIA attributes where needed
- Keyboard navigation support
- Sufficient color contrast

### Composable and Reusable
- Slot-based content where appropriate
- Consistent API patterns across components
- Flexible styling through class merging
- Phoenix LiveView event support
## Kbd

The `Kbd` component is used to display keyboard keys with consistent styling.

### Variants
- **default**: Standard size keyboard key
- **small**: Compact size for modifier keys

### Props
| Prop      | Type    | Default   | Description                     |
|-----------|---------|-----------|---------------------------------|
| variant   | atom    | :default  | Size variant of the key         |
| text      | string  | nil       | Text to display in the key      |

### Examples
```elixir
<.kbd>Enter</.kbd>
<.kbd variant={:small}>Ctrl</.kbd>
```