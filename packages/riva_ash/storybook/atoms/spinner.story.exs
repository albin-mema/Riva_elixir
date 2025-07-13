defmodule Storybook.Atoms.Spinner do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Atoms.Spinner.spinner/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{}
      },
      %Variation{
        id: :with_label,
        attributes: %{
          show_label: true,
          label: "Loading..."
        }
      },
      %Variation{
        id: :sizes,
        template: """
        <div class="flex items-center gap-8">
          <div class="text-center">
            <.spinner size="xs" />
            <div class="mt-2 text-xs">XS</div>
          </div>
          <div class="text-center">
            <.spinner size="sm" />
            <div class="mt-2 text-xs">SM</div>
          </div>
          <div class="text-center">
            <.spinner size="md" />
            <div class="mt-2 text-xs">MD</div>
          </div>
          <div class="text-center">
            <.spinner size="lg" />
            <div class="mt-2 text-xs">LG</div>
          </div>
          <div class="text-center">
            <.spinner size="xl" />
            <div class="mt-2 text-xs">XL</div>
          </div>
        </div>
        """
      },
      %Variation{
        id: :variants,
        template: """
        <div class="flex items-center gap-8">
          <div class="text-center">
            <.spinner variant="default" />
            <div class="mt-2 text-xs">Default</div>
          </div>
          <div class="text-center">
            <.spinner variant="primary" />
            <div class="mt-2 text-xs">Primary</div>
          </div>
          <div class="text-center">
            <.spinner variant="secondary" />
            <div class="mt-2 text-xs">Secondary</div>
          </div>
        </div>
        """
      },
      %Variation{
        id: :inline_with_text,
        template: """
        <div class="flex items-center gap-2">
          <.spinner size="sm" />
          <span>Loading data...</span>
        </div>
        """
      }
    ]
  end
end
