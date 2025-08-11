defmodule RivaAshWeb.Components.UIWrapped.Avatar do
  @moduledoc """
  App-level Avatar wrapper around SaladUI.Avatar.
  
  Provides user avatar components with fallback options.
  """
  use Phoenix.Component

  @doc """
  Renders a user avatar with image fallback.
  """
  attr :name, :string, doc: "User name for fallback text"
  attr :email, :string, doc: "User email for generating avatar"
  attr :src, :string, doc: "Image source URL"
  attr :size, :string,
    default: "default",
    values: ~w(default sm lg),
    doc: "Avatar size"

  attr :class, :string, default: ""
  attr :rest, :global

  def avatar(assigns) do
    assigns =
      assigns
      |> assign_new(:_salad_size, fn -> size_to_salad(assigns.size) end)
      |> assign_new(:fallback_text, fn ->
        case assigns.name do
          nil -> String.first(assigns.email || "")
          name -> String.first(String.trim(name))
        end
      end)

    ~H"""
    <SaladUI.Avatar.avatar
      src={@src}
      size={@_salad_size}
      class={[
        case @size do
          "sm" -> "w-8 h-8"
          "lg" -> "w-12 h-12"
          _ -> "w-10 h-10"
        end,
        @class
      ]}
      {@rest}
    >
      <%= if @src do %>
        <img src={@src} alt={@name || "User"} class="aspect-square h-full w-full" />
      <% else %>
        <span class="aspect-square h-full w-full flex items-center justify-center text-sm font-medium text-muted-foreground">
          <%= @fallback_text %>
        </span>
      <% end %>
    </SaladUI.Avatar.avatar>
    """
  end

  # Map our stable API to SaladUI expected props
  defp size_to_salad("default"), do: "default"
  defp size_to_salad(s) when s in ["sm", "lg"], do: s
  defp size_to_salad(_), do: "default"
end