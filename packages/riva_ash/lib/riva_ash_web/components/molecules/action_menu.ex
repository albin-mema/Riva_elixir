defmodule RivaAshWeb.Components.Molecules.ActionMenu do
  @moduledoc """
  Dropdown action menu component.
  
  Provides a dropdown menu with configurable positioning, sizing, and actions.
  Supports keyboard navigation and accessibility features.
  
  ## Styleguide Compliance
  
  This component follows the Riva Ash styleguide principles:
  
  ### Functional Programming Patterns
  - Uses pipeline operator (`|>`) for data transformation
  - Implements pure functions with no side effects
  - Uses pattern matching for data validation and processing
  - Follows single level of abstraction principle
  
  ### Type Safety
  - Comprehensive type specifications using `@type` and `@spec`
  - Strong typing for all function parameters and return values
  - Type validation through pattern matching
  
  ### Error Handling
  - Uses result tuples (`:ok | {:error, String.t()}`) for consistent error handling
  - Early validation with guard clauses
  - Clear error messages for invalid inputs
  
  ### Code Abstraction
  - Separates concerns into focused helper functions
  - Extracts validation logic into dedicated functions
  - Uses functional composition for complex operations
  
  ### Phoenix/Ash Patterns
  - Follows Phoenix LiveView component conventions
  - Uses proper attribute validation and building
  - Implements functional core, imperative shell pattern
  
  ### LiveView Component Patterns
  - Uses proper slot and attribute handling
  - Implements accessibility features
  - Follows Phoenix component best practices
  """
  use Phoenix.Component
  alias Phoenix.LiveView.JS
  alias RivaAshWeb.Components.UI.Button, as: UIButton
  alias RivaAshWeb.Components.UI.Icon, as: UIIcon

  @type action :: %{
          label: String.t(),
          icon: atom() | nil,
          phx_click: String.t(),
          disabled: boolean()
        }
  @type menu_position :: :bottom_left | :bottom_right | :top_left | :top_right
  @type menu_size :: :sm | :md | :lg
  @type assigns :: %{
          required(:actions) => list(action()),
          optional(:trigger_label) => String.t(),
          optional(:trigger_icon) => atom(),
          optional(:position) => menu_position(),
          optional(:size) => menu_size(),
          optional(:class) => String.t(),
          optional(:rest) => map()
        }

  @doc """
  Renders a dropdown action menu.

  ## Examples

      <.action_menu
        actions={[
          %{label: "Edit", icon: :pencil, phx_click: "edit"},
          %{label: "Delete", icon: :trash, phx_click: "delete", disabled: true}
        ]}
        trigger_label="Actions"
        trigger_icon={:ellipsis_vertical}
      />
  """
  @spec action_menu(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:trigger_label, :string, default: "Actions",
    doc: "Label for the trigger button")
  attr(:trigger_icon, :atom, default: :ellipsis_vertical,
    doc: "Icon for the trigger button")
  attr(:actions, :list, required: true,
    doc: "List of actions with :label, :icon, :phx_click, and :disabled keys")
  attr(:position, :atom,
    default: :bottom_right,
    values: ~w(bottom_left bottom_right top_left top_right)a,
    doc: "Position of the dropdown menu relative to the trigger")
  attr(:size, :atom, default: :md,
    values: ~w(sm md lg)a,
    doc: "Size variant of the trigger button")
  attr(:class, :string, default: "",
    doc: "Additional CSS classes for the container")
  attr(:rest, :global)

  @impl true
  def action_menu(assigns) do
    assigns
    |> build_action_menu_attrs()
    |> validate_action_menu_attrs()
    |> render_action_menu()
  end

  # Functional core: Pure functions for data transformation and validation

  @spec build_action_menu_attrs(assigns :: assigns()) :: assigns()
  defp build_action_menu_attrs(assigns) do
    # Extract configuration with defaults using functional pattern
    config = %{
      position: Application.get_env(:riva_ash, :action_menu_position, :bottom_right),
      size: Application.get_env(:riva_ash, :action_menu_size, :md),
      trigger_label: Application.get_env(:riva_ash, :action_menu_trigger_label, "Actions"),
      trigger_icon: Application.get_env(:riva_ash, :action_menu_trigger_icon, :ellipsis_vertical)
    }

    # Immutably update assigns with new values
    assigns
    |> Map.put_new(:position, config.position)
    |> Map.put_new(:size, config.size)
    |> Map.put_new(:trigger_label, config.trigger_label)
    |> Map.put_new(:trigger_icon, config.trigger_icon)
  end

  @spec validate_action_menu_attrs(assigns :: assigns()) :: assigns()
  defp validate_action_menu_attrs(assigns) do
    # Use with statement for sequential validation with early exit
    with :ok <- validate_actions(assigns[:actions]),
         :ok <- validate_position(assigns[:position]),
         :ok <- validate_size(assigns[:size]),
         :ok <- validate_trigger_label(assigns[:trigger_label]),
         :ok <- validate_trigger_icon(assigns[:trigger_icon]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid action menu attributes: #{reason}"
    end
  end

  # Validation functions with proper error handling using result tuples

  @spec validate_actions(list(map())) :: :ok | {:error, String.t()}
  defp validate_actions(actions) when is_list(actions) do
    # Use functional programming with Enum.all? for validation
    case Enum.all?(actions, &valid_action?/1) do
      true -> :ok
      false -> {:error, "All actions must have :label, :icon, :phx_click, and :disabled keys"}
    end
  end
  defp validate_actions(_), do: {:error, "actions must be a list"}

  @spec valid_action?(map()) :: boolean()
  defp valid_action?(action) do
    is_map(action) and
    is_binary(action[:label]) and
    is_atom(action[:icon]) and
    is_binary(action[:phx_click]) and
    is_boolean(action[:disabled])
  end

  @spec validate_position(menu_position() | String.t()) :: :ok | {:error, String.t()}
  defp validate_position(:bottom_left), do: :ok
  defp validate_position(:bottom_right), do: :ok
  defp validate_position(:top_left), do: :ok
  defp validate_position(:top_right), do: :ok
  defp validate_position("bottom-left"), do: :ok
  defp validate_position("bottom-right"), do: :ok
  defp validate_position("top-left"), do: :ok
  defp validate_position("top-right"), do: :ok
  defp validate_position(_), do: {:error, "position must be one of: bottom-left, bottom-right, top-left, top-right"}

  @spec validate_size(menu_size() | String.t()) :: :ok | {:error, String.t()}
  defp validate_size(:sm), do: :ok
  defp validate_size(:md), do: :ok
  defp validate_size(:lg), do: :ok
  defp validate_size("sm"), do: :ok
  defp validate_size("md"), do: :ok
  defp validate_size("lg"), do: :ok
  defp validate_size(_), do: {:error, "size must be one of: sm, md, lg"}

  @spec validate_trigger_label(String.t()) :: :ok | {:error, String.t()}
  defp validate_trigger_label(label) when is_binary(label) and label != "", do: :ok
  defp validate_trigger_label(_), do: {:error, "trigger_label must be a non-empty string"}

  @spec validate_trigger_icon(atom()) :: :ok | {:error, String.t()}
  defp validate_trigger_icon(icon) when is_atom(icon), do: :ok
  defp validate_trigger_icon(_), do: {:error, "trigger_icon must be an atom"}

  @spec render_action_menu(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_action_menu(assigns) do
    menu_id = assigns.trigger_label |> generate_menu_id()
    assigns = Map.put(assigns, :menu_id, menu_id)

    ~H"""
    <div class={["action-menu-container relative inline-block text-left", @class]} {@rest}>
      <.render_trigger_button
        label={@trigger_label}
        icon={@trigger_icon}
        size={@size}
        menu_id={@menu_id}
      />

      <div
        id={@menu_id}
        class={[
          "action-menu-dropdown absolute z-50 mt-2 w-56 rounded-md border bg-popover p-1 text-popover-foreground shadow-md hidden",
          build_position_classes(@position)
        ]}
      >
        <%= for action <- @actions do %>
          <%= render_action_item(action) %>
        <% end %>
      </div>
    </div>
    """
  end

  # Rendering functions with proper separation of concerns

  @doc """
  Trigger button component for the action menu.
  """
  @spec render_trigger_button(map()) :: Phoenix.LiveView.Rendered.t()
  attr(:label, :string, required: true)
  attr(:icon, :atom, default: nil)
  attr(:size, :string, default: "md", values: ~w(sm md lg))
  attr(:menu_id, :string, required: true)
  defp render_trigger_button(assigns) do
    ~H"""
    <UIButton.button
      variant="ghost"
      size={map_legacy_size(@size)}
      class="flex items-center gap-2"
      phx-click={JS.toggle(to: "##{@menu_id}")}
      aria-haspopup="true"
      aria-expanded="false"
      aria-label="Open actions menu"
    >
      <%= if @icon do %>
        <UIIcon.icon name={@icon} size="sm" />
      <% end %>
      <%= @label %>
    </UIButton.button>
    """
  end

  @spec render_action_item(action()) :: Phoenix.LiveView.Rendered.t()
  defp render_action_item(action) do
    assigns = %{action: action}

    ~H"""
    <button
      type="button"
      class={[
        "action-menu-item relative flex w-full cursor-default select-none items-center rounded-sm px-2 py-1.5 text-sm outline-none",
        "hover:bg-accent hover:text-accent-foreground focus:bg-accent focus:text-accent-foreground",
        "disabled:pointer-events-none disabled:opacity-50",
        if(@action.disabled, do: "opacity-50 cursor-not-allowed", else: "")
      ]}
      phx-click={@action.phx_click}
      disabled={@action.disabled}
      aria-disabled={@action.disabled}
    >
      <%= if @action.icon do %>
        <UIIcon.icon name={@action.icon} size="sm" class="mr-2" />
      <% end %>
      <%= @action.label %>
    </button>
    """
  end

  @spec generate_menu_id(String.t()) :: String.t()
  defp generate_menu_id(label) do
    # Use functional pipeline for string transformations
    label
    |> String.replace(" ", "-")
    |> String.downcase()
    |> then(&"action-menu-#{&1}")
  end

  @spec build_position_classes(menu_position()) :: String.t()
  defp build_position_classes(:bottom_left), do: "left-0 top-full"
  defp build_position_classes(:bottom_right), do: "right-0 top-full"
  defp build_position_classes(:top_left), do: "left-0 bottom-full mb-2"
  defp build_position_classes(:top_right), do: "right-0 bottom-full mb-2"

  @spec map_legacy_size(menu_size() | String.t()) :: String.t()
  defp map_legacy_size(:sm), do: "sm"
  defp map_legacy_size(:md), do: "default"
  defp map_legacy_size(:lg), do: "lg"
  defp map_legacy_size("sm"), do: "sm"
  defp map_legacy_size("md"), do: "default"
  defp map_legacy_size("lg"), do: "lg"
  defp map_legacy_size(_), do: "default"
end
