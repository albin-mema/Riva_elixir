defmodule Storybook.Atoms.Button do
  use PhoenixStorybook.Story, :component

  def component, do: &RivaAshWeb.Components.Atoms.Button.button/1

  def doc do
    """
    # Button (Atoms - Deprecated)

    ⚠️ **DEPRECATED**: This is a compatibility wrapper around the canonical UI.Button component.

    **For new code, use `RivaAshWeb.Components.UI.Button` directly.**

    This wrapper maintains backward compatibility during the migration period and will be removed in a future version.

    ## Migration Guide

    - `variant: :primary` → `variant: "default"`
    - `variant: :danger` → `variant: "destructive"`
    - `size: :md` → `size: "default"`

    See the [UI.Button story](/storybook/ui/button) for the canonical component.
    """
  end

  def template do
    """
    <div class="sb-p-4">
      <.button variant={@variant} {@rest}>
        <%= @label %>
      </.button>
    </div>
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          label: "Default Button",
          variant: "default"
        }
      },
      %Variation{
        id: :secondary,
        attributes: %{
          label: "Secondary Button",
          variant: "secondary"
        }
      },
      %Variation{
        id: :destructive,
        attributes: %{
          label: "Destructive Button",
          variant: "destructive"
        }
      },
      %Variation{
        id: :disabled,
        attributes: %{
          label: "Disabled Button",
          variant: "default",
          disabled: true
        }
      },
      %Variation{
        id: :loading,
        attributes: %{
          label: "Loading Button",
          variant: "default",
          loading: true
        }
      }
    ]
  end
end
