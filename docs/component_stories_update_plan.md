# Component Stories Update Plan

## Overview

This document outlines the plan for updating component stories to showcase the new design system and atomic components.

## Current State Analysis

The project currently has:
1. Existing stories in `packages/riva_ash/storybook/`
2. Stories for atoms like button, input, checkbox, etc.
3. Stories organized by component type
4. Basic variation showcases

## Goals

1. Update stories to showcase new design system
2. Demonstrate all component variants and states
3. Provide clear usage examples
4. Ensure consistent story structure
5. Improve documentation quality

## Story Structure

### Standard Story Format

```elixir
defmodule Storybook.UI.Button do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.UI.Button.button/1

  def doc do
    """
    # Button

    Buttons allow users to take actions, and make choices, with a single tap.

    ## Features

    - Multiple variants: default, destructive, outline, secondary, ghost, link
    - Multiple sizes: sm, default, lg
    - Loading state
    - Disabled state
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{},
        slots: ["Default Button"]
      },
      %Variation{
        id: :destructive,
        attributes: %{variant: "destructive"},
        slots: ["Destructive Button"]
      },
      # ... more variations
    ]
  end

  def examples do
    [
      %Example{
        id: :with_icon,
        description: "Button with icon",
        template: """
        <Button>
          <svg class="h-4 w-4 mr-2" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 6v6m0 0v6m0-6h6m-6 0H6" />
          </svg>
          Button with icon
        </Button>
        """
      }
    ]
  end
end
```

## Component Stories to Update

### 1. Button Story
- [ ] Showcase all variants (default, destructive, outline, secondary, ghost, link)
- [ ] Showcase all sizes (sm, default, lg)
- [ ] Showcase states (loading, disabled)
- [ ] Add examples with icons
- [ ] Add examples with icons only

### 2. Input Story
- [ ] Showcase all variants (default, error, success)
- [ ] Showcase all sizes (sm, default, lg)
- [ ] Showcase states (disabled, readonly)
- [ ] Add examples with labels
- [ ] Add examples with validation

### 3. Checkbox Story
- [ ] Showcase all variants (default, error, success)
- [ ] Showcase all sizes (sm, default, lg)
- [ ] Showcase states (disabled, checked)
- [ ] Add examples with labels and descriptions
- [ ] Add examples in a group

### 4. Select Story
- [ ] Showcase all variants (default, error, success)
- [ ] Showcase all sizes (sm, default, lg)
- [ ] Showcase states (disabled)
- [ ] Add examples with prompts
- [ ] Add examples with grouped options

### 5. Textarea Story
- [ ] Showcase all variants (default, error, success)
- [ ] Showcase all sizes (sm, default, lg)
- [ ] Showcase states (disabled, readonly)
- [ ] Add examples with labels
- [ ] Add examples with validation

### 6. Badge Story
- [ ] Showcase all variants (default, secondary, destructive, outline)
- [ ] Showcase all sizes (sm, default, lg)
- [ ] Add examples with different content

### 7. Card Story
- [ ] Showcase basic card
- [ ] Showcase card with header
- [ ] Showcase card with footer
- [ ] Showcase card with all sections

### 8. Alert Story
- [ ] Showcase all variants (default, destructive, success, warning)
- [ ] Showcase with titles
- [ ] Showcase with descriptions
- [ ] Showcase with titles and descriptions

## New Stories to Create

### 1. UI Module Story
Create a story that showcases how to use the UI module:

```elixir
defmodule Storybook.UI do
  use PhoenixStorybook.Story, :sample

  def doc do
    """
    # UI Components

    This is the main entry point for all UI components in the Riva Ash design system.

    ## Usage

    To use any UI component, import the UI module:

    ```elixir
    import RivaAshWeb.Components.UI
    ```

    Then use any component:

    ```heex
    <Button variant="primary">Click me</Button>
    <Input placeholder="Enter text" />
    ```
    """
  end

  def template do
    """
    <div class="space-y-4">
      <h1 class="text-2xl font-bold">UI Components</h1>
      <p class="text-muted-foreground">A collection of accessible, reusable components.</p>
      
      <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
        <Card>
          <Card.Header>
            <Card.Title>Button</Card.Title>
            <Card.Description>Trigger actions and events</Card.Description>
          </Card.Header>
          <Card.Content>
            <div class="flex gap-2">
              <Button>Default</Button>
              <Button variant="secondary">Secondary</Button>
            </div>
          </Card.Content>
        </Card>
        
        <Card>
          <Card.Header>
            <Card.Title>Input</Card.Title>
            <Card.Description>Collect user input</Card.Description>
          </Card.Header>
          <Card.Content>
            <Input placeholder="Enter text" />
          </Card.Content>
        </Card>
      </div>
    </div>
    """
  end
end
```

## Story Organization

### Directory Structure
```
storybook/
├── _root.index.exs
├── ui.index.exs
├── atoms.index.exs
├── molecules.index.exs
├── business.index.exs
├── welcome.story.exs
├── ui/
│   ├── button.story.exs
│   ├── input.story.exs
│   ├── checkbox.story.exs
│   ├── select.story.exs
│   ├── textarea.story.exs
│   ├── badge.story.exs
│   ├── card.story.exs
│   └── alert.story.exs
├── atoms/
│   ├── button.story.exs
│   ├── input.story.exs
│   └── ...
└── molecules/
    └── ...
```

## Implementation Plan

### Phase 1: Foundation
1. Create `ui.index.exs` file
2. Create stories for new UI components
3. Update existing stories to use new components

### Phase 2: Core Components
1. Update button story
2. Update input story
3. Update checkbox story
4. Update select story

### Phase 3: New Components
1. Create textarea story
2. Create badge story
3. Create card story
4. Create alert story

### Phase 4: Documentation
1. Create UI module story
2. Update index files
3. Add cross-references
4. Improve documentation quality

## Quality Assurance

### Consistency Checks
- [ ] All stories follow the same structure
- [ ] All components showcase all variants
- [ ] All components showcase all states
- [ ] All examples are functional
- [ ] All documentation is clear and accurate

### Accessibility Checks
- [ ] All examples are accessible
- [ ] Proper labels and descriptions
- [ ] Keyboard navigation works
- [ ] Screen reader compatibility

### Visual Checks
- [ ] All variations display correctly
- [ ] Consistent styling
- [ ] Proper spacing
- [ ] Responsive behavior

## Migration Strategy

1. Create new stories in `storybook/ui/` directory
2. Update existing stories to reference new components
3. Deprecate old stories after migration
4. Maintain backward compatibility during transition
5. Update index files to reflect new organization

## Timeline

### Week 1: Foundation
- Create story structure
- Create UI module story
- Set up documentation standards

### Week 2: Core Components
- Update button story
- Update input story
- Update checkbox story
- Update select story

### Week 3: New Components
- Create textarea story
- Create badge story
- Create card story
- Create alert story

### Week 4: Documentation
- Update index files
- Add cross-references
- Final quality checks