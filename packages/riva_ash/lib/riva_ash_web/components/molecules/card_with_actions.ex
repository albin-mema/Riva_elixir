defmodule RivaAshWeb.Components.Molecules.CardWithActions do
  @moduledoc """
  CardWithActions molecule that combines Card, Text, Button, and Icon atoms.
  
  Provides a complete card component with content and action buttons.
  """
  use Phoenix.Component

  alias RivaAshWeb.Components.UIWrapped.{Card, Text, Button, Icon}

  @doc """
  Renders a card with content and action buttons.
  """
  attr :title, :string, doc: "Card title"
  attr :description, :string, doc: "Card description"
  attr :icon, :atom, doc: "Card icon"
  attr :variant, :string,
    default: "default",
    values: ~w(default outline),
    doc: "Card visual variant"

  attr :actions, :list,
    default: [],
    doc: "List of action buttons with :label, :variant, :on_click, and optional :icon keys"

  attr :class, :string, default: ""
  attr :rest, :global

  slot :content, doc: "Additional content to render in the card"

  @spec card_with_actions(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def card_with_actions(assigns) do
    assigns
    |> build_card_with_actions_attrs()
    |> validate_card_with_actions_attrs()
    |> render_card_with_actions()
  end

  @spec build_card_with_actions_attrs(assigns :: map()) :: map()
  defp build_card_with_actions_attrs(assigns), do: assigns

  @spec validate_card_with_actions_attrs(assigns :: map()) :: map()
  defp validate_card_with_actions_attrs(assigns) do
    with :ok <- validate_title(assigns[:title]),
         :ok <- validate_description(assigns[:description]),
         :ok <- validate_actions(assigns[:actions]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid card with actions attributes: #{reason}"
    end
  end

  @spec validate_title(String.t() | nil) :: :ok | {:error, String.t()}
  defp validate_title(nil), do: :ok
  defp validate_title(title) when is_binary(title), do: :ok
  defp validate_unmatchedtitle(_unmatched), do: {:error, "title must be a string or nil"}

  @spec validate_description(String.t() | nil) :: :ok | {:error, String.t()}
  defp validate_description(nil), do: :ok
  defp validate_description(description) when is_binary(description), do: :ok
  defp validate_unmatchedsdescription(_unmatched), do: {:error, "description must be a string or nil"}

  @spec validate_actions(list(map())) :: :ok | {:error, String.t()}
  defp validate_actions(actions) when is_list(actions) do
    case Enum.all?(actions, &valid_action?/1) do
      true -> :ok
      false -> {:error, "All actions must be maps with :label and :variant keys"}
    end
  end

  defp validate_unmatchedactions(_unmatched), do: {:error, "actions must be a list"}

  @spec valid_action?(map()) :: boolean()
  defp valid_action?(action) do
    is_map(action) and
      is_binary(action[:label]) and
      is_binary(action[:variant]) and
      (is_function(action[:on_click]) or is_nil(action[:on_click])) and
      (is_atom(action[:icon]) or is_nil(action[:icon]))
  end

  @spec render_card_with_actions(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_card_with_actions(assigns) do
    ~H"""
    <Card.card variant={@variant} class={["card-with-actions", @class]} {@rest}>
      <%= if @title || @icon do %>
        <Card.card_header>
          <%= if @icon do %>
            <div class="flex items-center space-x-2">
              <Icon.icon name={@icon} size="lg" />
              <%= if @title do %>
                <Text.text variant="lead" class="font-semibold">
                  <%= @title %>
                </Text.text>
              <% end %>
            </div>
          <% else %>
            <%= if @title do %>
              <Text.text variant="lead" class="font-semibold">
                <%= @title %>
              </Text.text>
            <% end %>
          <% end %>
          
          <%= if @description do %>
            <Text.text variant="muted" class="text-sm">
              <%= @description %>
            </Text.text>
          <% end %>
        </Card.card_header>
      <% end %>
      
      <%= if @content do %>
        <Card.card_content>
          <%= render_slot(@content) %>
        </Card.card_content>
      <% end %>
      
      <%= if @actions && length(@actions) > 0 do %>
        <Card.card_footer>
          <div class="flex justify-end space-x-2">
            <%= for action <- @actions do %>
              <Button.button 
                variant={action.variant} 
                phx-click={action.on_click}
                class={[
                  case action.variant do
                    "primary" -> "bg-primary text-primary-foreground hover:bg-primary/90"
                    "secondary" -> "bg-secondary text-secondary-foreground hover:bg-secondary/80"
                    "outline" -> "border border-input bg-background hover:bg-accent hover:text-accent-foreground"
                    "ghost" -> "hover:bg-accent hover:text-accent-foreground"
                    "destructive" -> "bg-destructive text-destructive-foreground hover:bg-destructive/90"
                    _ -> ""
                  end
                ]}
              >
                <%= if action.icon do %>
                  <Icon.icon name={action.icon} size="sm" class="mr-1" />
                <% end %>
                <%= action.label %>
              </Button.button>
            <% end %>
          </div>
        </Card.card_footer>
      <% end %>
    </Card.card>
    """
  end
end