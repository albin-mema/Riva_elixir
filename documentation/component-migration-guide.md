# Component Migration Guide

This guide provides comprehensive instructions for migrating from atom components to the canonical UI system.

## Overview

The Riva Ash application is migrating to a canonical UI component system where:

- **UI Components** (`RivaAshWeb.Components.UI.*`) are the single source of truth for design system primitives
- **Atom Components** (`RivaAshWeb.Components.Atoms.*`) are compatibility wrappers that delegate to UI components
- **Molecule Components** (`RivaAshWeb.Components.Molecules.*`) use UI components internally while preserving their composed APIs

## Migration Patterns

### 1. Button Migration

**Before (Legacy Atom):**
```elixir
import RivaAshWeb.Components.Atoms.Button

def my_component(assigns) do
  ~H"""
  <.button variant="primary" size="md" disabled={@loading}>
    <%= if @loading, do: "Loading...", else: "Save" %>
  </.button>
  """
end
```

**After (Canonical UI):**
```elixir
alias RivaAshWeb.Components.UI.Button, as: UIButton

def my_component(assigns) do
  ~H"""
  <UIButton.button variant="default" size="default" loading={@loading}>
    Save
  </UIButton.button>
  """
end
```

**Key Changes:**
- `variant="primary"` → `variant="default"`
- `size="md"` → `size="default"`
- Manual loading text → `loading={@loading}` attribute

### 2. Input Migration

**Before (Legacy Atom):**
```elixir
import RivaAshWeb.Components.Atoms.Input

def form_component(assigns) do
  ~H"""
  <.input 
    type="email"
    field={@form[:email]}
    placeholder="Enter email"
    size="lg"
    variant={if @form[:email].errors != [], do: "error", else: "default"}
  />
  """
end
```

**After (Canonical UI):**
```elixir
alias RivaAshWeb.Components.UI.Input, as: UIInput

def form_component(assigns) do
  ~H"""
  <UIInput.input 
    type="email"
    field={@form[:email]}
    placeholder="Enter email"
    size="lg"
  />
  """
end
```

**Key Changes:**
- Error variant is automatically applied when field has errors
- No need to manually check field errors for variant

### 3. Form Field Migration

**Before (Manual composition):**
```elixir
import RivaAshWeb.Components.Atoms.Input
import RivaAshWeb.Components.Atoms.Text

def form_field(assigns) do
  ~H"""
  <div class="space-y-2">
    <.text variant="label"><%= @label %></.text>
    <.input field={@field} />
    <div :if={@field.errors != []} class="text-red-600 text-sm">
      <%= for error <- @field.errors do %>
        <div><%= translate_error(error) %></div>
      <% end %>
    </div>
  </div>
  """
end
```

**After (Using Molecule):**
```elixir
import RivaAshWeb.Components.Molecules.FormField

def my_form(assigns) do
  ~H"""
  <.form_field 
    field={@form[:email]}
    label="Email Address"
    helper_text="We'll never share your email"
  />
  """
end
```

**Key Changes:**
- Error handling is built into the molecule component
- Consistent styling and behavior across all form fields
- Less boilerplate code

## Component-Specific Migration

### Button Component

| Legacy Attribute | UI Attribute | Notes |
|------------------|--------------|-------|
| `variant="primary"` | `variant="default"` | Primary is now default |
| `variant="secondary"` | `variant="secondary"` | No change |
| `variant="danger"` | `variant="destructive"` | Renamed for consistency |
| `size="sm"` | `size="sm"` | No change |
| `size="md"` | `size="default"` | Medium becomes default |
| `size="lg"` | `size="lg"` | No change |
| Manual loading state | `loading={boolean}` | Built-in loading spinner |

### Input Component

| Legacy Attribute | UI Attribute | Notes |
|------------------|--------------|-------|
| `size="sm"` | `size="sm"` | No change |
| `size="md"` | `size="default"` | Medium becomes default |
| `size="lg"` | `size="lg"` | No change |
| `variant="error"` | Automatic | Applied when field has errors |
| `variant="success"` | `variant="success"` | No change |

### Text Component

| Legacy Attribute | UI Attribute | Notes |
|------------------|--------------|-------|
| `variant="heading"` | `variant="h3"` | More specific heading levels |
| `variant="body"` | `variant="p"` | Semantic HTML elements |
| `variant="caption"` | `variant="small"` | Consistent with HTML semantics |

## Testing Migration

### Before (Testing Atoms)
```elixir
test "renders button with correct variant" do
  html = render_component(&Atoms.Button.button/1, %{variant: "primary"})
  assert html =~ "bg-blue-600"
end
```

### After (Testing UI Components)
```elixir
test "renders button with correct variant" do
  html = render_component(&UI.Button.button/1, %{variant: "default"})
  assert html =~ "bg-primary"
end
```

### Compatibility Wrapper Tests
```elixir
test "atom button delegates to UI button" do
  html = render_component(&Atoms.Button.button/1, %{variant: "primary", size: "md"})
  # Should contain UI.Button classes, not legacy classes
  assert html =~ "bg-primary"
  assert html =~ "h-10" # default size classes
end
```

## Common Pitfalls

### 1. Size Mapping Confusion
```elixir
# ❌ Wrong - using old size names
<UIButton.button size="md">Save</UIButton.button>

# ✅ Correct - use "default" for medium
<UIButton.button size="default">Save</UIButton.button>
```

### 2. Variant Name Changes
```elixir
# ❌ Wrong - old variant names
<UIButton.button variant="primary">Save</UIButton.button>
<UIButton.button variant="danger">Delete</UIButton.button>

# ✅ Correct - new variant names
<UIButton.button variant="default">Save</UIButton.button>
<UIButton.button variant="destructive">Delete</UIButton.button>
```

### 3. Manual Error Handling
```elixir
# ❌ Wrong - manual error variant
<UIInput.input 
  field={@form[:email]}
  variant={if @form[:email].errors != [], do: "error", else: "default"}
/>

# ✅ Correct - automatic error handling
<UIInput.input field={@form[:email]} />
```

## Migration Checklist

- [ ] Replace atom component imports with UI component aliases
- [ ] Update variant names (primary → default, danger → destructive)
- [ ] Update size names (md → default)
- [ ] Remove manual error handling for form inputs
- [ ] Update tests to use UI component classes
- [ ] Update Storybook stories to point to UI components
- [ ] Verify accessibility attributes are preserved
- [ ] Test component behavior in different states

## Next Steps

1. **Phase 1**: Create UI components and compatibility wrappers ✅
2. **Phase 2**: Update molecule components to use UI components internally ✅
3. **Phase 3**: Update organism components and LiveViews
4. **Phase 4**: Remove compatibility wrappers after full migration
5. **Phase 5**: Update documentation and training materials

## Support

For questions about the migration:
- Check this guide first
- Review the UI Guidelines documentation
- Look at existing migrated components for examples
- Ask in the development team chat for clarification
