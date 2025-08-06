# Button Component Update Plan

## Current Implementation Issues

The current button component in `packages/riva_ash/lib/riva_ash_web/components/atoms/button.ex` has several issues:

1. Hardcoded Tailwind classes instead of using CSS variables
2. Inconsistent API with other components
3. Limited variant support
4. No proper focus states or accessibility features

## Proposed New Implementation

### Component API

```elixir
attr :type, :string, default: "button"
attr :variant, :string, default: "default", values: ~w(default destructive outline secondary ghost link)
attr :size, :string, default: "default", values: ~w(default sm lg)
attr :loading, :boolean, default: false
attr :class, :string, default: ""
attr :disabled, :boolean, default: false
attr :rest, :global
```

### CSS Classes Structure

The new button should use CSS classes that leverage the CSS variables defined in `app.css`:

#### Base Classes
```
inline-flex items-center justify-center rounded-md text-sm font-medium transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:opacity-50 disabled:pointer-events-none ring-offset-background
```

#### Variants

1. **Default**:
   ```
   bg-primary text-primary-foreground hover:bg-primary/90
   ```

2. **Destructive**:
   ```
   bg-destructive text-destructive-foreground hover:bg-destructive/90
   ```

3. **Outline**:
   ```
   border border-input hover:bg-accent hover:text-accent-foreground
   ```

4. **Secondary**:
   ```
   bg-secondary text-secondary-foreground hover:bg-secondary/80
   ```

5. **Ghost**:
   ```
   hover:bg-accent hover:text-accent-foreground
   ```

6. **Link**:
   ```
   underline-offset-4 hover:underline text-primary
   ```

#### Sizes

1. **Default**:
   ```
   h-10 py-2 px-4
   ```

2. **Small**:
   ```
   h-9 px-3 rounded-md
   ```

3. **Large**:
   ```
   h-11 px-8 rounded-md
   ```

### Loading State

When `loading` is true:
- Add `opacity-50 pointer-events-none` classes
- Optionally render a spinner component

### Implementation Plan

1. Create a new `button.ex` file in `packages/riva_ash/lib/riva_ash_web/components/ui/`
2. Implement the new API with proper CSS variable usage
3. Ensure all variants and sizes are supported
4. Add proper focus states and accessibility features
5. Include loading state support
6. Update the component story to showcase all variants and states

### Example Implementation

```elixir
defmodule RivaAshWeb.Components.UI.Button do
  use Phoenix.Component

  @doc """
  Renders a button component using the design system.
  """
  attr :type, :string, default: "button"
  attr :variant, :string, default: "default", values: ~w(default destructive outline secondary ghost link)
  attr :size, :string, default: "default", values: ~w(default sm lg)
  attr :loading, :boolean, default: false
  attr :class, :string, default: ""
  attr :disabled, :boolean, default: false
  attr :rest, :global, include: ~w(phx-click phx-disable-with phx-value phx-value-id)

  slot :inner_block, required: true

  def button(assigns) do
    assigns = assign(assigns, :button_class, button_class(assigns))

    ~H"""
    <button
      type={@type}
      class={@button_class}
      disabled={@disabled || @loading}
      {@rest}
    >
      <%= if @loading do %>
        <!-- Loading spinner -->
        <div class="mr-2 h-4 w-4 animate-spin">
          <svg class="h-4 w-4" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M12 2V6" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M12 18V22" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M4.93 4.93L7.76 7.76" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M16.24 16.24L19.07 19.07" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M2 12H6" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M18 12H22" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M4.93 19.07L7.76 16.24" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M16.24 7.76L19.07 4.93" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
          </svg>
        </div>
      <% end %>
      <%= render_slot(@inner_block) %>
    </button>
    """
  end

  defp button_class(assigns) do
    base = "inline-flex items-center justify-center rounded-md text-sm font-medium transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:opacity-50 disabled:pointer-events-none ring-offset-background"
    
    variant = variant_classes(assigns.variant)
    size = size_classes(assigns.size)
    loading = if assigns.loading, do: "opacity-50 pointer-events-none", else: ""
    
    Enum.join([base, variant, size, loading, assigns.class], " ")
  end

  defp variant_classes(variant) do
    case variant do
      "default" -> "bg-primary text-primary-foreground hover:bg-primary/90"
      "destructive" -> "bg-destructive text-destructive-foreground hover:bg-destructive/90"
      "outline" -> "border border-input hover:bg-accent hover:text-accent-foreground"
      "secondary" -> "bg-secondary text-secondary-foreground hover:bg-secondary/80"
      "ghost" -> "hover:bg-accent hover:text-accent-foreground"
      "link" -> "underline-offset-4 hover:underline text-primary"
      _ -> "bg-primary text-primary-foreground hover:bg-primary/90"
    end
  end

  defp size_classes(size) do
    case size do
      "default" -> "h-10 py-2 px-4"
      "sm" -> "h-9 px-3 rounded-md"
      "lg" -> "h-11 px-8 rounded-md"
      _ -> "h-10 py-2 px-4"
    end
  end
end
```

### Component Story

Create a corresponding story file at `packages/riva_ash/storybook/ui/button.story.exs` to showcase all variants and states.

### Migration Strategy

1. Create the new component in the `ui` directory
2. Update existing usage to use the new component
3. Deprecate the old component after migration
4. Maintain backward compatibility during transition