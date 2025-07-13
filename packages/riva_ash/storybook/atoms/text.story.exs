defmodule Storybook.Atoms.Text do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Atoms.Text.text/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{},
        slots: ["This is default text"]
      },
      %Variation{
        id: :headings,
        slots: [
          """
          <div class="space-y-4">
            <.text variant="h1">Heading 1</.text>
            <.text variant="h2">Heading 2</.text>
            <.text variant="h3">Heading 3</.text>
            <.text variant="h4">Heading 4</.text>
            <.text variant="h5">Heading 5</.text>
            <.text variant="h6">Heading 6</.text>
          </div>
          """
        ]
      },
      %Variation{
        id: :body_text,
        slots: [
          """
          <div class="space-y-4">
            <.text variant="body-lg">Large body text for important content</.text>
            <.text variant="body">Regular body text for most content</.text>
            <.text variant="body-sm">Small body text for secondary information</.text>
            <.text variant="caption">Caption text for labels and metadata</.text>
          </div>
          """
        ]
      },
      %Variation{
        id: :colors,
        slots: [
          """
          <div class="space-y-2">
            <.text color="default">Default color text</.text>
            <.text color="muted">Muted color text</.text>
            <.text color="primary">Primary color text</.text>
            <.text color="secondary">Secondary color text</.text>
            <.text color="success">Success color text</.text>
            <.text color="warning">Warning color text</.text>
            <.text color="destructive">Destructive color text</.text>
          </div>
          """
        ]
      },
      %Variation{
        id: :weights,
        slots: [
          """
          <div class="space-y-2">
            <.text weight="light">Light weight text</.text>
            <.text weight="normal">Normal weight text</.text>
            <.text weight="medium">Medium weight text</.text>
            <.text weight="semibold">Semibold weight text</.text>
            <.text weight="bold">Bold weight text</.text>
          </div>
          """
        ]
      },
      %Variation{
        id: :alignment,
        slots: [
          """
          <div class="space-y-4">
            <.text align="left">Left aligned text</.text>
            <.text align="center">Center aligned text</.text>
            <.text align="right">Right aligned text</.text>
            <.text align="justify">Justified text that spans multiple lines to demonstrate how text justification works with longer content.</.text>
          </div>
          """
        ]
      },
      %Variation{
        id: :truncation,
        slots: [
          """
          <div class="space-y-4 max-w-xs">
            <.text truncate={true}>This is a very long text that will be truncated when it exceeds the container width</.text>
            <.text clamp={2}>This is a longer text that will be clamped to exactly two lines when it exceeds that limit, showing an ellipsis at the end</.text>
          </div>
          """
        ]
      }
    ]
  end
end
