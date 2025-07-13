defmodule Storybook.Atoms.Icon do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Atoms.Icon.icon/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          name: :home
        }
      },
      %Variation{
        id: :common_icons,
        template: """
        <div class="gap-4 grid grid-cols-6 p-4">
          <div class="flex flex-col items-center gap-2">
            <.icon name={:home} />
            <span class="text-xs">home</span>
          </div>
          <div class="flex flex-col items-center gap-2">
            <.icon name={:user} />
            <span class="text-xs">user</span>
          </div>
          <div class="flex flex-col items-center gap-2">
            <.icon name={:cog_6_tooth} />
            <span class="text-xs">settings</span>
          </div>
          <div class="flex flex-col items-center gap-2">
            <.icon name={:bell} />
            <span class="text-xs">bell</span>
          </div>
          <div class="flex flex-col items-center gap-2">
            <.icon name={:envelope} />
            <span class="text-xs">envelope</span>
          </div>
          <div class="flex flex-col items-center gap-2">
            <.icon name={:calendar} />
            <span class="text-xs">calendar</span>
          </div>
        </div>
        """
      },
      %Variation{
        id: :sizes,
        template: """
        <div class="flex items-center gap-4">
          <.icon name={:star} size="xs" />
          <.icon name={:star} size="sm" />
          <.icon name={:star} size="md" />
          <.icon name={:star} size="lg" />
          <.icon name={:star} size="xl" />
        </div>
        """
      },
      %Variation{
        id: :variants,
        template: """
        <div class="flex items-center gap-4">
          <.icon name={:heart} variant="outline" />
          <.icon name={:heart} variant="solid" />
          <.icon name={:heart} variant="mini" />
        </div>
        """
      }
    ]
  end
end
