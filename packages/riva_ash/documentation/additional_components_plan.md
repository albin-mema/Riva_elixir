# Additional Components Plan

## Overview

This document outlines the plan for implementing additional atomic components based on the shadcn-style design system.

## Checkbox Component

### Current Implementation Issues

The current checkbox component in `packages/riva_ash/lib/riva_ash_web/components/atoms/checkbox.ex` has:
1. Hardcoded Tailwind classes instead of using CSS variables
2. Inconsistent API with other components
3. Limited styling options

### Proposed New Implementation

### Component API

```elixir
attr :field, Phoenix.HTML.FormField, default: nil
attr :checked, :boolean, default: false
attr :value, :string, default: "true"
attr :label, :string, default: nil
attr :description, :string, default: nil
attr :disabled, :boolean, default: false
attr :variant, :string, default: "default", values: ~w(default error success)
attr :size, :string, default: "default", values: ~w(default sm lg)
attr :class, :string, default: ""
attr :rest, :global
```

### CSS Classes Structure

#### Base Classes for Checkbox Input
```
h-4 w-4 rounded border border-input bg-background focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-ring disabled:cursor-not-allowed disabled:opacity-50
```

#### Variants for Checkbox Input

1. **Default**:
   ```
   text-primary
   ```

2. **Error**:
   ```
   border-destructive text-destructive focus:ring-destructive
   ```

3. **Success**:
   ```
   border-[var(--chart-5)] text-[var(--chart-5)] focus:ring-[var(--chart-5)]
   ```

#### Sizes for Checkbox Input

1. **Default**:
   ```
   h-4 w-4
   ```

2. **Small**:
   ```
   h-3 w-3
   ```

3. **Large**:
   ```
   h-5 w-5
   ```

#### Label Classes
```
text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70
```

#### Label Sizes

1. **Default**:
   ```
   text-sm
   ```

2. **Small**:
   ```
   text-xs
   ```

3. **Large**:
   ```
   text-base
   ```

## Select Component

### Current Implementation Issues

The current select component in `packages/riva_ash/lib/riva_ash_web/components/atoms/select.ex` has:
1. Hardcoded Tailwind classes instead of using CSS variables
2. Inconsistent API with other components
3. Limited variant support

### Proposed New Implementation

### Component API

```elixir
attr :field, Phoenix.HTML.FormField, default: nil
attr :options, :list, default: []
attr :prompt, :string, default: nil
attr :multiple, :boolean, default: false
attr :disabled, :boolean, default: false
attr :required, :boolean, default: false
attr :variant, :string, default: "default", values: ~w(default error success)
attr :size, :string, default: "default", values: ~w(default sm lg)
attr :class, :string, default: ""
attr :rest, :global
```

### CSS Classes Structure

#### Base Classes
```
flex w-full rounded-md border border-input bg-background px-3 py-2 text-sm ring-offset-background focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50
```

#### Variants

1. **Default**:
   ```
   (no additional classes)
   ```

2. **Error**:
   ```
   border-destructive focus:ring-destructive
   ```

3. **Success**:
   ```
   border-[var(--chart-5)] focus:ring-[var(--chart-5)]
   ```

#### Sizes

1. **Default**:
   ```
   h-10 px-3
   ```

2. **Small**:
   ```
   h-9 px-2 text-xs
   ```

3. **Large**:
   ```
   h-11 px-4 text-base
   ```

## Textarea Component

### New Component

We should also implement a Textarea component with similar API and styling.

### Component API

```elixir
attr :field, Phoenix.HTML.FormField, default: nil
attr :value, :string, default: nil
attr :placeholder, :string, default: ""
attr :disabled, :boolean, default: false
attr :readonly, :boolean, default: false
attr :required, :boolean, default: false
attr :rows, :integer, default: 3
attr :variant, :string, default: "default", values: ~w(default error success)
attr :size, :string, default: "default", values: ~w(default sm lg)
attr :class, :string, default: ""
attr :rest, :global
```

## Badge Component

### New Component

A Badge component for displaying status or labels.

### Component API

```elixir
attr :variant, :string, default: "default", values: ~w(default secondary destructive outline)
attr :size, :string, default: "default", values: ~w(default sm lg)
attr :class, :string, default: ""
attr :rest, :global

slot :inner_block, required: true
```

## Card Component

### New Component

A Card component for grouping related content.

### Component API

```elixir
attr :class, :string, default: ""
attr :rest, :global

slot :header
slot :inner_block, required: true
slot :footer
```

## Alert Component

### New Component

An Alert component for displaying important messages.

### Component API

```elixir
attr :variant, :string, default: "default", values: ~w(default destructive success warning)
attr :title, :string, default: nil
attr :class, :string, default: ""
attr :rest, :global

slot :inner_block, required: true
```

## Implementation Plan

1. Create new component files in `packages/riva_ash/lib/riva_ash_web/components/ui/`
2. Implement each component with proper CSS variable usage
3. Ensure consistent APIs across all components
4. Add proper focus states and accessibility features
5. Include support for form fields and validation states where applicable
6. Create corresponding story files for each component
7. Update documentation

## Component Directory Structure

```
lib/riva_ash_web/components/ui/
├── button.ex
├── input.ex
├── checkbox.ex
├── select.ex
├── textarea.ex
├── badge.ex
├── card.ex
├── alert.ex
└── ui.ex
```

## Migration Strategy

1. Create new components in the `ui` directory
2. Update existing usage to use new components gradually
3. Deprecate old components after migration
4. Maintain backward compatibility during transition