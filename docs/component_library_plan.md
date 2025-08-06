# Component Library Plan

## Overview

This document outlines the plan for implementing a shadcn-style component library for the Riva Ash project. The library will use the existing design system with OKLCH colors and CSS variables.

## Current State Analysis

The project already has:
- A well-structured component system with atoms, molecules, organisms
- Tailwind CSS with CSS variables for styling
- A storybook for component documentation
- Existing components like Button, Input, Checkbox, Select

However, the current implementation has:
- Hardcoded Tailwind classes instead of using CSS variables consistently
- Inconsistent APIs across components
- No unified component library module

## Proposed Structure

### 1. Component Library Module

Create a unified `RivaAshWeb.Components.UI` module that will serve as the main entry point for all UI components, similar to shadcn's approach.

### 2. Component API Standards

All components should follow these API principles:
- Consistent attribute naming (`variant`, `size`, `class`, etc.)
- Support for global attributes
- Proper use of CSS variables
- Accessible by default
- Composable and reusable

### 3. Component Implementation

#### Button Component
- Variants: default, destructive, outline, secondary, ghost, link
- Sizes: sm, md, lg
- States: disabled, loading
- Proper use of CSS variables for colors

#### Input Component
- Variants: default, error, success
- Sizes: sm, md, lg
- States: disabled, readonly
- Proper use of CSS variables for borders, background, text

#### Other Atomic Components
- Checkbox: With label and description support
- Select: With options and prompt support
- Textarea: With validation states
- Badge: With variants and sizes
- Card: With header, content, footer sections
- Alert: With variants (info, warning, error, success)

## CSS Variable Usage

All components should use the CSS variables defined in `app.css`:
- Colors: `--primary`, `--secondary`, `--accent`, `--destructive`, etc.
- Typography: `--font-sans`, `--font-serif`, `--font-mono`
- Spacing: `--spacing`, `--radius`
- Shadows: `--shadow-sm`, `--shadow-md`, `--shadow-lg`

## Implementation Steps

1. Create the `UI` module structure
2. Update existing components to use CSS variables properly
3. Ensure consistent APIs across all components
4. Create missing components as needed
5. Update component stories to showcase the new design
6. Document the component library usage
7. Replace hardcoded styles with atomic component usage throughout the project

## Component Directory Structure

```
lib/riva_ash_web/components/
├── ui/
│   ├── button.ex
│   ├── input.ex
│   ├── checkbox.ex
│   ├── select.ex
│   ├── textarea.ex
│   ├── badge.ex
│   ├── card.ex
│   ├── alert.ex
│   └── ui.ex (main module)
├── atoms/
├── molecules/
├── organisms/
└── core_components.ex
```

## Migration Strategy

1. Implement new components in the `ui` directory
2. Update existing code to use new components gradually
3. Deprecate old components after migration
4. Maintain backward compatibility during transition

## Design Tokens

The following design tokens from the existing system should be used:

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
- Sans-serif: `var(--font-sans)`
- Serif: `var(--font-serif)`
- Monospace: `var(--font-mono)`

### Spacing & Radius
- Radius: `var(--radius)`
- Spacing: `var(--spacing)`

### Shadows
- Small: `var(--shadow-sm)`
- Medium: `var(--shadow-md)`
- Large: `var(--shadow-lg)`