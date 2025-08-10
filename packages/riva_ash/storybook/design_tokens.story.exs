defmodule RivaAsh.Storybook.DesignTokens do
  use Phoenix.Component
  use Storybook.Story, title: "Design Tokens", category: :foundations

  parameters do
    [
      chromatic: [
        modes: [
          light: [theme: "Light"],
          dark: [theme: "Dark"]
        ]
      ],
      axe: [
        rules: [
          "color-contrast": [enabled: true],
          "duplicate-id": [enabled: false]
        ]
      ]
    ]
  end

  controls do
    boolean("Dark Mode", :dark_mode, false)
    select("Theme", :theme, ["Light", "Dark"], "Light")
  end

  def story(assigns) do
    ~H"""
    <div class={if(@theme == "Dark", do: "dark", else: "")} data-theme={@theme}>
      <.docs>
        # Design Tokens
        The canonical reference for all UI development. All components should use these tokens instead of hard-coded values.

        ## Usage Guidelines
        - **Foundation tokens** are atomic values (e.g., `oklch(0.52 0.18 275)`)
        - **Semantic tokens** provide meaning (e.g., `--primary`)
        - Always use Tailwind classes or CSS variables instead of hard-coded values
        - For motion, use the `transition` classes with duration/easing modifiers

        ## Accessibility
        All color combinations meet WCAG AA/AAA standards. Verified with axe-core:
        ```js
        axe.run('.design-tokens', { rules: { 'color-contrast': { enabled: true } } })
        ```

        ## Visual Regression
        Baseline snapshots captured for both Light and Dark themes. Run visual tests with:
        ```bash
        npx percy exec -- storybook build
        ```
      </.docs>

      <.container>
        <.section title="Colors">
          <.color_palette title="Core" colors={["blue", "teal", "amber", "red"]} />
          <.color_palette title="Semantic" colors={["primary", "secondary", "accent", "destructive"]} />
          <.color_palette title="Surface" colors={["background", "card", "popover"]} />
        </.section>

        <.section title="Spacing">
          <.spacing_grid />
        </.section>

        <.section title="Typography">
          <.typography_samples />
        </.section>

        <.section title="Radii">
          <.radii_samples />
        </.section>

        <.section title="Shadows">
          <.shadow_samples />
        </.section>

        <.section title="Motion">
          <.motion_samples />
        </.section>
      </.container>
    </div>
    """
  end

  attr :title, :string, required: true

  def section(assigns) do
    ~H"""
    <div class="mb-12">
      <h2 class="mb-6 pb-2 border-b font-bold text-2xl"><%= @title %></h2>
      <%= render_slot(@inner_block) %>
    </div>
    """
  end

  attr :title, :string, required: true
  attr :colors, :list, required: true

  def color_palette(assigns) do
    ~H"""
    <div class="mb-8">
      <h3 class="mb-4 font-semibold text-xl"><%= @title %></h3>
      <div class="gap-4 grid grid-cols-4">
        <%= for color <- @colors do %>
          <div class="flex flex-col items-center">
            <div class={"w-16 h-16 rounded-[var(--radius-md)] border border-border bg-#{color}"} />
            <span class="mt-2 text-sm"><%= color %></span>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  def spacing_grid(assigns) do
    ~H"""
    <div class="gap-4 grid grid-cols-7">
      <%= for {token, value} <- [
            {"space-0", "0"},
            {"space-1", "0.25rem"},
            {"space-2", "0.5rem"},
            {"space-3", "0.75rem"},
            {"space-4", "1rem"},
            {"space-5", "1.25rem"},
            {"space-6", "1.5rem"}
          ] do %>
        <div class="flex flex-col items-center">
          <div class={"h-16 w-16 bg-muted rounded-[var(--radius-md)] flex items-center justify-center transition-all duration-normal ease-in-out hover:scale-110"}
               style={"margin: #{value}"}>
            <span class="text-muted-foreground text-xs">#{token}</span>
          </div>
          <span class="mt-2 text-muted-foreground text-xs">#{value}</span>
        </div>
      <% end %>
    </div>
    """
  end

  def typography_samples(assigns) do
    ~H"""
    <div class="space-y-6">
      <%= for {token, value} <- [
            {"text-xs", "0.75rem/1.25"},
            {"text-sm", "0.875rem/1.5"},
            {"text-base", "1rem/1.5"},
            {"text-lg", "1.125rem/1.75"},
            {"text-xl", "1.25rem/1.75"},
            {"text-2xl", "1.5rem/2"}
          ] do %>
        <div>
          <p class={"#{token} font-sans text-foreground"}>The quick brown fox jumps over the lazy dog</p>
          <span class="block mt-1 text-muted-foreground text-xs">#{token} • #{value}</span>
        </div>
      <% end %>
    </div>
    """
  end

  def radii_samples(assigns) do
    ~H"""
    <div class="gap-4 grid grid-cols-4">
      <%= for {token, value} <- [
            {"radius-sm", "var(--radius-sm)"},
            {"radius-md", "var(--radius-md)"},
            {"radius-lg", "var(--radius-lg)"},
            {"radius-xl", "var(--radius-xl)"}
          ] do %>
        <div class="flex flex-col items-center">
          <div class={"w-16 h-16 bg-card border border-border transition-all duration-normal ease-in-out hover:rotate-6"}
               style={"border-radius: #{value}"}>
          </div>
          <span class="mt-2 text-muted-foreground text-xs">#{token}</span>
        </div>
      <% end %>
    </div>
    """
  end

  def shadow_samples(assigns) do
    ~H"""
    <div class="gap-4 grid grid-cols-3">
      <%= for {token, value} <- [
            {"shadow-sm", "var(--shadow-sm)"},
            {"shadow-md", "var(--shadow-md)"},
            {"shadow-lg", "var(--shadow-lg)"}
          ] do %>
        <div class="flex flex-col items-center">
          <div class={"w-16 h-16 bg-card border border-border transition-all duration-normal ease-in-out hover:scale-110"}
               style={"box-shadow: #{value}"}>
          </div>
          <span class="mt-2 text-muted-foreground text-xs">#{token}</span>
        </div>
      <% end %>
    </div>
    """
  end

  def motion_samples(assigns) do
    ~H"""
    <div class="space-y-8">
      <div>
        <h3 class="mb-4 font-medium text-lg">Hover Transitions</h3>
        <div class="gap-4 grid grid-cols-3">
          <div class="bg-card p-4 border border-border rounded-md text-center hover:scale-110 transition-all duration-normal ease-in-out cursor-pointer">
            Scale on hover
          </div>
          <div class="bg-card p-4 border border-border rounded-md text-center hover:rotate-6 transition-all duration-normal ease-in-out cursor-pointer">
            Rotate on hover
          </div>
          <div class="bg-card hover:bg-accent p-4 border border-border rounded-md text-center transition-all duration-normal ease-in-out cursor-pointer">
            Color on hover
          </div>
        </div>
      </div>

      <div>
        <h3 class="mb-4 font-medium text-lg">Focus States</h3>
        <button class="bg-primary px-4 py-2 rounded-md focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2 text-primary-foreground">
          Focus me
        </button>
      </div>

      <div>
        <h3 class="mb-4 font-medium text-lg">Enter/Exit Animations</h3>
        <div phx-click="toggle" class="bg-card p-4 border border-border rounded-md text-center transition-all duration-normal ease-in-out cursor-pointer">
          Click to toggle
        </div>
      </div>
    </div>
    """
  end
end
