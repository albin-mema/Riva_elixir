defmodule RivaAsh.Stories.TooltipStories do
  use Surface.Catalogue.Story, "UI Components"

  alias RivaAsh.Components.UI.{Tooltip, Button, IconButton}

  def render(assigns) do
    ~H"""
    <div class="space-y-8">
      <h2 class="font-bold text-2xl">Tooltip Variants</h2>

      <div class="space-y-4">
        <h3 class="font-semibold text-xl">Directions</h3>
        <div class="flex flex-wrap justify-center items-center gap-8 bg-muted p-4 rounded-lg">
          <.tooltip text="Top tooltip" direction={:top}>
            <.button>Top</.button>
          </.tooltip>

          <.tooltip text="Right tooltip" direction={:right}>
            <.button>Right</.button>
          </.tooltip>

          <.tooltip text="Bottom tooltip" direction={:bottom}>
            <.button>Bottom</.button>
          </.tooltip>

          <.tooltip text="Left tooltip" direction={:left}>
            <.button>Left</.button>
          </.tooltip>
        </div>
      </div>

      <div class="space-y-4">
        <h3 class="font-semibold text-xl">Collision Detection</h3>
        <div class="flex flex-col items-start gap-4 bg-muted p-4 rounded-lg">
          <div class="p-4 border w-full">
            <.tooltip text="This tooltip should automatically adjust from top to bottom due to space constraints" direction={:top}>
              <.button class="w-full">Collision Test (Top)</.button>
            </.tooltip>
          </div>

          <div class="p-4 border w-full h-20 overflow-hidden">
            <.tooltip text="This tooltip should automatically adjust from bottom to top due to space constraints" direction={:bottom}>
              <.button class="w-full">Collision Test (Bottom)</.button>
            </.tooltip>
          </div>
        </div>
      </div>

      <div class="space-y-4">
        <h3 class="font-semibold text-xl">Delay Configuration</h3>
        <div class="flex flex-wrap justify-center items-center gap-4 bg-muted p-4 rounded-lg">
          <.tooltip text="200ms delay (default)" delay={200}>
            <.button>Default Delay</.button>
          </.tooltip>

          <.tooltip text="500ms delay" delay={500}>
            <.button>Long Delay</.button>
          </.tooltip>

          <.tooltip text="Instant appearance" delay={0}>
            <.button>No Delay</.button>
          </.tooltip>
        </div>
      </div>

      <div class="space-y-4">
        <h3 class="font-semibold text-xl">Integration with Other Components</h3>
        <div class="flex flex-wrap justify-center items-center gap-4 bg-muted p-4 rounded-lg">
          <.tooltip text="Action button">
            <.button>With Button</.button>
          </.tooltip>

          <.tooltip text="Navigation icon">
            <.icon_button icon="home" />
          </.tooltip>

          <.tooltip text="Disabled element" disabled={true}>
            <.button disabled>Disabled</.button>
          </.tooltip>
        </div>
      </div>

      <div class="space-y-4">
        <h3 class="font-semibold text-xl">Mobile Touch Interactions</h3>
        <div class="flex flex-col items-center gap-4 bg-muted p-4 rounded-lg">
          <.tooltip text="Tap to dismiss" delay={0}>
            <.button>Touch Target</.button>
          </.tooltip>

          <p class="text-muted-foreground text-sm">
            On touch devices, tooltips appear on tap and dismiss when tapping elsewhere
          </p>
        </div>
      </div>
    </div>
    """
  end
end
