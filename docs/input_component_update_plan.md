# Input Component Update Plan

## Current Implementation Issues

The current input component in `packages/riva_ash/lib/riva_ash_web/components/atoms/input.ex` has several issues:

1. Hardcoded Tailwind classes instead of using CSS variables
2. Inconsistent API with other components
3. Limited variant support
4. No proper focus states or accessibility features

## Proposed New Implementation

### Component API

```elixir
attr :type, :string, default: "text"
attr :field, Phoenix.HTML.FormField, default: nil
attr :value, :string, default: nil
attr :placeholder, :string, default: ""
attr :disabled, :boolean, default: false
attr :readonly, :boolean, default: false
attr :required, :boolean, default: false
attr :variant, :string, default: "default", values: ~w(default error success)
attr :size, :string, default: "default", values: ~w(default sm lg)
attr :class, :string, default: ""
attr :rest, :global
```

### CSS Classes Structure

The new input should use CSS classes that leverage the CSS variables defined in `app.css`:

#### Base Classes
```
flex w-full rounded-md border border-input bg-background px-3 py-2 text-sm ring-offset-background file:border-0 file:bg-transparent file:text-sm file:font-medium placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50
```

#### Variants

1. **Default**:
   ```
   (no additional classes)
   ```

2. **Error**:
   ```
   border-destructive focus-visible:ring-destructive
   ```

3. **Success**:
   ```
   border-[var(--chart-5)] focus-visible:ring-[var(--chart-5)]
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

### Implementation Plan

1. Create a new `input.ex` file in `packages/riva_ash/lib/riva_ash_web/components/ui/`
2. Implement the new API with proper CSS variable usage
3. Ensure all variants and sizes are supported
4. Add proper focus states and accessibility features
5. Include support for form fields and validation states
6. Update the component story to showcase all variants and states

### Example Implementation

```elixir
defmodule RivaAshWeb.Components.UI.Input do
  use Phoenix.Component

  @doc """
  Renders an input component using the design system.
  """
  attr :type, :string, default: "text"
  attr :field, Phoenix.HTML.FormField, default: nil
  attr :value, :string, default: nil
  attr :placeholder, :string, default: ""
  attr :disabled, :boolean, default: false
  attr :readonly, :boolean, default: false
  attr :required, :boolean, default: false
  attr :variant, :string, default: "default", values: ~w(default error success)
  attr :size, :string, default: "default", values: ~w(default sm lg)
  attr :class, :string, default: ""
  attr :rest, :global

  def input(assigns) do
    # Handle form field integration
    assigns = 
      case assigns.field do
        nil -> assigns
        field -> assign(assigns, :value, assigns.value || field.value)
      end
    
    assigns = assign(assigns, :input_class, input_class(assigns))

    ~H"""
    <input
      type={@type}
      class={@input_class}
      value={@value}
      placeholder={@placeholder}
      disabled={@disabled}
      readonly={@readonly}
      required={@required}
      {@rest}
    />
    """
  end

  defp input_class(assigns) do
    base = "flex w-full rounded-md border border-input bg-background px-3 py-2 text-sm ring-offset-background file:border-0 file:bg-transparent file:text-sm file:font-medium placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50"
    
    variant = variant_classes(assigns.variant)
    size = size_classes(assigns.size)
    
    Enum.join([base, variant, size, assigns.class], " ")
  end

  defp variant_classes(variant) do
    case variant do
      "default" -> ""
      "error" -> "border-destructive focus-visible:ring-destructive"
      "success" -> "border-[var(--chart-5)] focus-visible:ring-[var(--chart-5)]"
      _ -> ""
    end
  end

  defp size_classes(size) do
    case size do
      "default" -> "h-10 px-3"
      "sm" -> "h-9 px-2 text-xs"
      "lg" -> "h-11 px-4 text-base"
      _ -> "h-10 px-3"
    end
  end
end
```

### Component Story

Create a corresponding story file at `packages/riva_ash/storybook/ui/input.story.exs` to showcase all variants and states.

### Migration Strategy

1. Create the new component in the `ui` directory
2. Update existing usage to use the new component
3. Deprecate the old component after migration
4. Maintain backward compatibility during transition