defmodule Storybook.Organisms.Header do
  use PhoenixStorybook.Story, :component

  def component, do: &RivaAshWeb.Components.Organisms.Header.header/1

  def template do
    """
    <div class="sb-p-4">
      <.header current_user={@current_user}>
        <:nav_item path="#">Home</:nav_item>
        <:nav_item path="#">Reservations</:nav_item>
        <:nav_item path="#">Inventory</:nav_item>
      </.header>
    </div>
    """
  end

  def variations do
    [
      %Variation{
        id: :logged_in,
        attributes: %{
          current_user: %{name: "John Doe", email: "john@example.com"}
        }
      },
      %Variation{
        id: :logged_out,
        attributes: %{
          current_user: nil
        }
      }
    ]
  end
end
