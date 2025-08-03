defmodule Storybook.DesignTokens do
  use PhoenixStorybook.Story, :page

  def doc, do: "Global Design Tokens showcase for the atomic design system"

  def render(assigns) do
    ~H"""
    <div class="p-8 space-y-12">
      <div>
        <h1 class="text-4xl font-bold mb-4">Design Tokens</h1>
        <p class="text-lg text-muted-foreground">
          Global design tokens using OKLCH color space for consistent theming across light and dark modes.
        </p>
      </div>

      <!-- Colors -->
      <section>
        <h2 class="text-2xl font-semibold mb-6">Colors</h2>
        
        <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          <!-- Primary Colors -->
          <div class="space-y-3">
            <h3 class="text-lg font-medium">Primary</h3>
            <div class="space-y-2">
              <div class="flex items-center space-x-3">
                <div class="w-12 h-12 rounded-lg bg-primary border"></div>
                <div>
                  <div class="font-mono text-sm">--primary</div>
                  <div class="text-xs text-muted-foreground">Primary brand color</div>
                </div>
              </div>
              <div class="flex items-center space-x-3">
                <div class="w-12 h-12 rounded-lg bg-primary-foreground border"></div>
                <div>
                  <div class="font-mono text-sm">--primary-foreground</div>
                  <div class="text-xs text-muted-foreground">Text on primary</div>
                </div>
              </div>
            </div>
          </div>

          <!-- Secondary Colors -->
          <div class="space-y-3">
            <h3 class="text-lg font-medium">Secondary</h3>
            <div class="space-y-2">
              <div class="flex items-center space-x-3">
                <div class="w-12 h-12 rounded-lg bg-secondary border"></div>
                <div>
                  <div class="font-mono text-sm">--secondary</div>
                  <div class="text-xs text-muted-foreground">Secondary color</div>
                </div>
              </div>
              <div class="flex items-center space-x-3">
                <div class="w-12 h-12 rounded-lg bg-secondary-foreground border"></div>
                <div>
                  <div class="font-mono text-sm">--secondary-foreground</div>
                  <div class="text-xs text-muted-foreground">Text on secondary</div>
                </div>
              </div>
            </div>
          </div>

          <!-- Accent Colors -->
          <div class="space-y-3">
            <h3 class="text-lg font-medium">Accent</h3>
            <div class="space-y-2">
              <div class="flex items-center space-x-3">
                <div class="w-12 h-12 rounded-lg bg-accent border"></div>
                <div>
                  <div class="font-mono text-sm">--accent</div>
                  <div class="text-xs text-muted-foreground">Accent color</div>
                </div>
              </div>
              <div class="flex items-center space-x-3">
                <div class="w-12 h-12 rounded-lg bg-accent-foreground border"></div>
                <div>
                  <div class="font-mono text-sm">--accent-foreground</div>
                  <div class="text-xs text-muted-foreground">Text on accent</div>
                </div>
              </div>
            </div>
          </div>

          <!-- Destructive Colors -->
          <div class="space-y-3">
            <h3 class="text-lg font-medium">Destructive</h3>
            <div class="space-y-2">
              <div class="flex items-center space-x-3">
                <div class="w-12 h-12 rounded-lg bg-destructive border"></div>
                <div>
                  <div class="font-mono text-sm">--destructive</div>
                  <div class="text-xs text-muted-foreground">Error/danger color</div>
                </div>
              </div>
              <div class="flex items-center space-x-3">
                <div class="w-12 h-12 rounded-lg bg-destructive-foreground border"></div>
                <div>
                  <div class="font-mono text-sm">--destructive-foreground</div>
                  <div class="text-xs text-muted-foreground">Text on destructive</div>
                </div>
              </div>
            </div>
          </div>

          <!-- Surface Colors -->
          <div class="space-y-3">
            <h3 class="text-lg font-medium">Surfaces</h3>
            <div class="space-y-2">
              <div class="flex items-center space-x-3">
                <div class="w-12 h-12 rounded-lg bg-background border"></div>
                <div>
                  <div class="font-mono text-sm">--background</div>
                  <div class="text-xs text-muted-foreground">Page background</div>
                </div>
              </div>
              <div class="flex items-center space-x-3">
                <div class="w-12 h-12 rounded-lg bg-card border"></div>
                <div>
                  <div class="font-mono text-sm">--card</div>
                  <div class="text-xs text-muted-foreground">Card background</div>
                </div>
              </div>
              <div class="flex items-center space-x-3">
                <div class="w-12 h-12 rounded-lg bg-muted border"></div>
                <div>
                  <div class="font-mono text-sm">--muted</div>
                  <div class="text-xs text-muted-foreground">Muted background</div>
                </div>
              </div>
            </div>
          </div>

          <!-- Text Colors -->
          <div class="space-y-3">
            <h3 class="text-lg font-medium">Text</h3>
            <div class="space-y-2">
              <div class="flex items-center space-x-3">
                <div class="w-12 h-12 rounded-lg bg-foreground border"></div>
                <div>
                  <div class="font-mono text-sm">--foreground</div>
                  <div class="text-xs text-muted-foreground">Primary text</div>
                </div>
              </div>
              <div class="flex items-center space-x-3">
                <div class="w-12 h-12 rounded-lg bg-muted-foreground border"></div>
                <div>
                  <div class="font-mono text-sm">--muted-foreground</div>
                  <div class="text-xs text-muted-foreground">Secondary text</div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </section>

      <!-- Typography -->
      <section>
        <h2 class="text-2xl font-semibold mb-6">Typography</h2>
        
        <div class="grid grid-cols-1 md:grid-cols-3 gap-8">
          <div>
            <h3 class="text-lg font-medium mb-4">Font Families</h3>
            <div class="space-y-3">
              <div>
                <div class="font-sans text-lg">Sans Serif (Inter)</div>
                <div class="font-mono text-sm text-muted-foreground">font-sans</div>
              </div>
              <div>
                <div class="font-serif text-lg">Serif (Lora)</div>
                <div class="font-mono text-sm text-muted-foreground">font-serif</div>
              </div>
              <div>
                <div class="font-mono text-lg">Monospace (IBM Plex Mono)</div>
                <div class="font-mono text-sm text-muted-foreground">font-mono</div>
              </div>
            </div>
          </div>

          <div>
            <h3 class="text-lg font-medium mb-4">Letter Spacing</h3>
            <div class="space-y-2">
              <div class="tracking-tighter">Tighter spacing</div>
              <div class="tracking-tight">Tight spacing</div>
              <div class="tracking-normal">Normal spacing</div>
              <div class="tracking-wide">Wide spacing</div>
              <div class="tracking-wider">Wider spacing</div>
              <div class="tracking-widest">Widest spacing</div>
            </div>
          </div>

          <div>
            <h3 class="text-lg font-medium mb-4">Font Sizes</h3>
            <div class="space-y-2">
              <div class="text-xs">Extra small (xs)</div>
              <div class="text-sm">Small (sm)</div>
              <div class="text-base">Base (base)</div>
              <div class="text-lg">Large (lg)</div>
              <div class="text-xl">Extra large (xl)</div>
              <div class="text-2xl">2X large (2xl)</div>
            </div>
          </div>
        </div>
      </section>

      <!-- Shadows -->
      <section>
        <h2 class="text-2xl font-semibold mb-6">Shadows</h2>
        
        <div class="grid grid-cols-2 md:grid-cols-4 gap-6">
          <div class="text-center">
            <div class="w-20 h-20 bg-card rounded-lg shadow-2xs mx-auto mb-2"></div>
            <div class="font-mono text-sm">shadow-2xs</div>
          </div>
          <div class="text-center">
            <div class="w-20 h-20 bg-card rounded-lg shadow-xs mx-auto mb-2"></div>
            <div class="font-mono text-sm">shadow-xs</div>
          </div>
          <div class="text-center">
            <div class="w-20 h-20 bg-card rounded-lg shadow-sm mx-auto mb-2"></div>
            <div class="font-mono text-sm">shadow-sm</div>
          </div>
          <div class="text-center">
            <div class="w-20 h-20 bg-card rounded-lg shadow mx-auto mb-2"></div>
            <div class="font-mono text-sm">shadow</div>
          </div>
          <div class="text-center">
            <div class="w-20 h-20 bg-card rounded-lg shadow-md mx-auto mb-2"></div>
            <div class="font-mono text-sm">shadow-md</div>
          </div>
          <div class="text-center">
            <div class="w-20 h-20 bg-card rounded-lg shadow-lg mx-auto mb-2"></div>
            <div class="font-mono text-sm">shadow-lg</div>
          </div>
          <div class="text-center">
            <div class="w-20 h-20 bg-card rounded-lg shadow-xl mx-auto mb-2"></div>
            <div class="font-mono text-sm">shadow-xl</div>
          </div>
          <div class="text-center">
            <div class="w-20 h-20 bg-card rounded-lg shadow-2xl mx-auto mb-2"></div>
            <div class="font-mono text-sm">shadow-2xl</div>
          </div>
        </div>
      </section>

      <!-- Border Radius -->
      <section>
        <h2 class="text-2xl font-semibold mb-6">Border Radius</h2>
        
        <div class="grid grid-cols-2 md:grid-cols-5 gap-6">
          <div class="text-center">
            <div class="w-20 h-20 bg-primary rounded-none mx-auto mb-2"></div>
            <div class="font-mono text-sm">rounded-none</div>
          </div>
          <div class="text-center">
            <div class="w-20 h-20 bg-primary rounded-sm mx-auto mb-2"></div>
            <div class="font-mono text-sm">rounded-sm</div>
          </div>
          <div class="text-center">
            <div class="w-20 h-20 bg-primary rounded mx-auto mb-2"></div>
            <div class="font-mono text-sm">rounded</div>
          </div>
          <div class="text-center">
            <div class="w-20 h-20 bg-primary rounded-lg mx-auto mb-2"></div>
            <div class="font-mono text-sm">rounded-lg</div>
          </div>
          <div class="text-center">
            <div class="w-20 h-20 bg-primary rounded-xl mx-auto mb-2"></div>
            <div class="font-mono text-sm">rounded-xl</div>
          </div>
        </div>
      </section>
    </div>
    """
  end
end
