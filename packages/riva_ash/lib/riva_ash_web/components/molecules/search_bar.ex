defmodule RivaAshWeb.Components.Molecules.SearchBar do
  @moduledoc """
  SearchBar molecule that combines Input, Button, and Icon atoms.
  
  Provides a complete search interface with consistent styling and behavior.
  """
  use Phoenix.Component

  alias RivaAshWeb.Components.UIWrapped.{Input, Button, Icon}

  @doc """
  Renders a search bar with input field and search button.
  """
  attr :placeholder, :string,
    default: "Search...",
    doc: "Placeholder text for the search input"

  attr :value, :string,
    default: "",
    doc: "Current search value"

  attr :disabled, :boolean, default: false, doc: "Whether the search bar is disabled"
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, doc: "Additional content to render in the search bar"

  @spec search_bar(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def search_bar(assigns) do
    assigns
    |> build_search_bar_attrs()
    |> validate_search_bar_attrs()
    |> render_search_bar()
  end

  @spec build_search_bar_attrs(assigns :: map()) :: map()
  defp build_search_bar_attrs(assigns), do: assigns

  @spec validate_search_bar_attrs(assigns :: map()) :: map()
  defp validate_search_bar_attrs(assigns) do
    with :ok <- validate_placeholder(assigns[:placeholder]),
         :ok <- validate_value(assigns[:value]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid search bar attributes: #{reason}"
    end
  end

  @spec validate_placeholder(String.t()) :: :ok | {:error, String.t()}
  defp validate_placeholder(placeholder) when is_binary(placeholder), do: :ok
  defp validate_unmatchedplaceholder(_unmatched), do: {:error, "placeholder must be a string"}

  @spec validate_value(String.t()) :: :ok | {:error, String.t()}
  defp validate_value(value) when is_binary(value), do: :ok
  defp validate_unmatchedvalue(_unmatched), do: {:error, "value must be a string"}

  @spec render_search_bar(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_search_bar(assigns) do
    ~H"""
    <div class={["search-bar", @class]}>
      <div class="relative">
        <div class="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
          <Icon.icon name={:search} size="sm" class="text-muted-foreground" />
        </div>
        
        <Input.input
          type="text"
          placeholder={@placeholder}
          value={@value}
          disabled={@disabled}
          class={[
            "pl-10",
            "w-full",
            @disabled && "opacity-50 cursor-not-allowed"
          ]}
          {@rest}
        />
      </div>
      
      <%= if @disabled do %>
        <Button.button variant="secondary" disabled={true} class="ml-2">
          Search
        </Button.button>
      <% else %>
        <Button.button variant="primary" class="ml-2">
          Search
        </Button.button>
      <% end %>
      
      <%= render_slot(@inner_block) %>
    </div>
    """
  end
end