defmodule RivaAshWeb.Components.Molecules.UserProfile do
  @moduledoc """
  UserProfile molecule that combines Avatar, Text, and Button atoms.
  
  Provides a complete user profile display with avatar, name, and actions.
  """
  use Phoenix.Component

  alias RivaAshWeb.Components.UIWrapped.{Avatar, Text, Button, Icon}

  @doc """
  Renders a user profile component with avatar and user information.
  """
  attr :name, :string, required: true, doc: "User's display name"
  attr :email, :string, doc: "User's email address"
  attr :avatar_src, :string, doc: "Avatar image source URL"
  attr :size, :string,
    default: "default",
    values: ~w(default sm),
    doc: "Profile size variant"

  attr :show_actions, :boolean, default: true, doc: "Whether to show action buttons"
  attr :class, :string, default: ""
  attr :rest, :global

  slot :actions, doc: "Additional action buttons to display"

  @spec user_profile(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def user_profile(assigns) do
    assigns
    |> build_user_profile_attrs()
    |> validate_user_profile_attrs()
    |> render_user_profile()
  end

  @spec build_user_profile_attrs(assigns :: map()) :: map()
  defp build_user_profile_attrs(assigns), do: assigns

  @spec validate_user_profile_attrs(assigns :: map()) :: map()
  defp validate_user_profile_attrs(assigns) do
    with :ok <- validate_name(assigns[:name]),
         :ok <- validate_email(assigns[:email]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid user profile attributes: #{reason}"
    end
  end

  @spec validate_name(String.t()) :: :ok | {:error, String.t()}
  defp validate_name(name) when is_binary(name) and name != "", do: :ok
  defp validate_unmatchedname(_unmatched), do: {:error, "name must be a non-empty string"}

  @spec validate_email(String.t() | nil) :: :ok | {:error, String.t()}
  defp validate_email(nil), do: :ok
  defp validate_email(email) when is_binary(email), do: :ok
  defp validate_unmatchedemail(_unmatched), do: {:error, "email must be a string or nil"}

  @spec render_user_profile(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_user_profile(assigns) do
    ~H"""
    <div class={["user-profile", @class]}>
      <div class="flex items-center space-x-3">
        <Avatar.avatar 
          name={@name} 
          src={@avatar_src} 
          size={@size}
          {@rest}
        />
        
        <div class="flex-1 min-w-0">
          <Text.text variant="lead" class="font-medium truncate">
            <%= @name %>
          </Text.text>
          
          <%= if @email do %>
            <Text.text variant="muted" class="text-sm truncate">
              <%= @email %>
            </Text.text>
          <% end %>
        </div>
        
        <%= if @show_actions do %>
          <div class="flex items-center space-x-2">
            <%= render_slot(@actions) %>
            
            <Button.button variant="ghost" size="sm">
              <Icon.icon name={:cog_6_tooth} size="sm" />
            </Button.button>
          </div>
        <% end %>
      </div>
    </div>
    """
  end
end