# CSS Variables Consistency Plan

## Overview

This document outlines the plan for ensuring all components use CSS variables consistently according to the design system defined in `app.css`.

## Current State Analysis

The project already has a comprehensive set of CSS variables defined in `packages/riva_ash/assets/css/app.css`:

### Color Variables

#### Light Mode
- `--background`: oklch(0.99 0.002 260)
- `--foreground`: oklch(0.17 0.005 260)
- `--card`: oklch(0.98 0.002 260)
- `--card-foreground`: oklch(0.20 0.006 260)
- `--popover`: oklch(0.97 0.003 260)
- `--popover-foreground`: oklch(0.18 0.005 260)
- `--primary`: oklch(0.52 0.18 275)
- `--primary-foreground`: oklch(0.98 0.001 260)
- `--secondary`: oklch(0.65 0.10 185)
- `--secondary-foreground`: oklch(0.15 0.008 260)
- `--accent`: oklch(0.70 0.22 75)
- `--accent-foreground`: oklch(0.15 0.008 260)
- `--destructive`: oklch(0.55 0.20 25)
- `--destructive-foreground`: oklch(0.98 0.001 260)
- `--muted`: oklch(0.92 0.003 260)
- `--muted-foreground`: oklch(0.45 0.008 260)
- `--border`: oklch(0.85 0.005 260)
- `--input`: oklch(0.90 0.004 260)
- `--ring`: oklch(0.52 0.18 275)
- `--chart-1` to `--chart-5`: Various chart colors

#### Dark Mode
- `--background`: oklch(0.14 0.004 260)
- `--foreground`: oklch(0.92 0.002 260)
- And corresponding dark mode adjustments for all other colors

### Typography Variables
- `--font-sans`: "Inter", system-ui, sans-serif
- `--font-serif`: Lora, ui-serif, serif
- `--font-mono`: "IBM Plex Mono", monospace

### Spacing & Radius Variables
- `--radius`: 6px
- `--spacing`: 0.3rem

### Shadow Variables
- `--shadow-sm`: 0 2px 4px oklch(0 0 0 / 0.08)
- `--shadow-md`: 0 4px 8px oklch(0 0 0 / 0.12)
- `--shadow-lg`: 0 8px 16px oklch(0 0 0 / 0.16)

## Consistency Issues

1. Components are using hardcoded Tailwind classes instead of CSS variables
2. Some components are using direct color values instead of variables
3. Inconsistent use of spacing and radius variables
4. Missing proper focus states and accessibility features

## Implementation Plan

### 1. Color Variable Usage

Replace hardcoded colors with CSS variables:

#### Before (Hardcoded)
```html
class="bg-blue-500 text-white hover:bg-blue-600"
```

#### After (CSS Variables)
```html
class="bg-primary text-primary-foreground hover:bg-primary/90"
```

### 2. Typography Variable Usage

Use font variables consistently:

#### Before (Hardcoded)
```html
class="font-sans text-base"
```

#### After (CSS Variables)
```html
class="font-sans text-sm" style="font-family: var(--font-sans)"
```

### 3. Spacing & Radius Variable Usage

Use spacing and radius variables:

#### Before (Hardcoded)
```html
class="rounded px-4 py-2"
```

#### After (CSS Variables)
```html
class="rounded-md px-4 py-2" style="border-radius: var(--radius)"
```

### 4. Shadow Variable Usage

Use shadow variables:

#### Before (Hardcoded)
```html
class="shadow-md"
```

#### After (CSS Variables)
```html
class="shadow" style="box-shadow: var(--shadow-md)"
```

## Component-by-Component Review

### Button Component
- [ ] Replace hardcoded colors with `bg-primary`, `text-primary-foreground`, etc.
- [ ] Use `var(--radius)` for border radius
- [ ] Use `var(--ring)` for focus ring

### Input Component
- [ ] Replace hardcoded colors with `border-input`, `bg-background`, etc.
- [ ] Use `var(--radius)` for border radius
- [ ] Use `var(--ring)` for focus ring

### Checkbox Component
- [ ] Replace hardcoded colors with `border-input`, `text-primary`, etc.
- [ ] Use `var(--radius)` for border radius
- [ ] Use `var(--ring)` for focus ring

### Select Component
- [ ] Replace hardcoded colors with `border-input`, `bg-background`, etc.
- [ ] Use `var(--radius)` for border radius
- [ ] Use `var(--ring)` for focus ring

### Textarea Component
- [ ] Replace hardcoded colors with `border-input`, `bg-background`, etc.
- [ ] Use `var(--radius)` for border radius
- [ ] Use `var(--ring)` for focus ring

### Badge Component
- [ ] Replace hardcoded colors with `bg-primary`, `text-primary-foreground`, etc.
- [ ] Use `var(--radius)` for border radius

### Card Component
- [ ] Replace hardcoded colors with `bg-card`, `text-card-foreground`, etc.
- [ ] Use `var(--radius)` for border radius
- [ ] Use `var(--border)` for border color

### Alert Component
- [ ] Replace hardcoded colors with `bg-destructive`, `text-destructive-foreground`, etc.
- [ ] Use `var(--radius)` for border radius
- [ ] Use `var(--border)` for border color

## Migration Strategy

1. Create new components in the `ui` directory with proper CSS variable usage
2. Update existing usage to use new components gradually
3. Deprecate old components after migration
4. Maintain backward compatibility during transition
5. Create a style guide document for future reference

## Style Guide

### Colors
- Primary actions: `var(--primary)` with `var(--primary-foreground)`
- Secondary actions: `var(--secondary)` with `var(--secondary-foreground)`
- Destructive actions: `var(--destructive)` with `var(--destructive-foreground)`
- Backgrounds: `var(--background)` with `var(--foreground)`
- Cards: `var(--card)` with `var(--card-foreground)`
- Popovers: `var(--popover)` with `var(--popover-foreground)`
- Muted elements: `var(--muted)` with `var(--muted-foreground)`
- Borders: `var(--border)`
- Inputs: `var(--input)`
- Focus rings: `var(--ring)`

### Typography
- Primary font: `var(--font-sans)`
- Secondary font: `var(--font-serif)`
- Monospace font: `var(--font-mono)`
- Base size: `text-sm`
- Leading: `leading-none` or `leading-tight`

### Spacing
- Base unit: `var(--spacing)`
- Padding: Multiples of `var(--spacing)`
- Margin: Multiples of `var(--spacing)`

### Radius
- Default: `var(--radius)`
- Small: `calc(var(--radius) - 2px)`
- Large: `calc(var(--radius) + 2px)`

### Shadows
- Small: `var(--shadow-sm)`
- Medium: `var(--shadow-md)`
- Large: `var(--shadow-lg)`

## Testing Plan

1. Verify all components render correctly in light mode
2. Verify all components render correctly in dark mode
3. Check focus states and accessibility features
4. Ensure consistent styling across all components
5. Test responsive behavior