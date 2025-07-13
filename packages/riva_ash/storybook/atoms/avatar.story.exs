defmodule Storybook.Atoms.Avatar do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Atoms.Avatar.avatar/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          initials: "JD",
          name: "John Doe"
        }
      },
      %Variation{
        id: :with_image,
        attributes: %{
          src: "https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80",
          alt: "Profile picture",
          name: "John Doe"
        }
      },
      %Variation{
        id: :small,
        attributes: %{
          size: "sm",
          initials: "SM"
        }
      },
      %Variation{
        id: :large,
        attributes: %{
          size: "lg",
          initials: "LG"
        }
      },
      %Variation{
        id: :circle,
        attributes: %{
          shape: "circle",
          initials: "CI"
        }
      },
      %Variation{
        id: :square,
        attributes: %{
          shape: "square",
          initials: "SQ"
        }
      },
      %Variation{
        id: :fallback_icon,
        attributes: %{
          name: "No Image User"
        }
      }
    ]
  end
end
