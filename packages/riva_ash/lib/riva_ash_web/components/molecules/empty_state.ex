defmodule RivaAshWeb.Components.Molecules.EmptyState do
  @moduledoc """
  EmptyState component for displaying when no data is available.
  A molecule component that provides a consistent empty state experience.
  
  Supports multiple variants, sizes, and optional actions for different use cases.
  
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
  alias RivaAshWeb.Components.UI.Text, as: UIText
  alias RivaAshWeb.Components.UI.Icon, as: UIIcon

  @type empty_state_config :: %{
          icon: atom(),
          title: String.t(),
          description: String.t() | nil,
          size: size(),
          variant: variant(),
          class: String.t()
        }
  @type size :: :sm | :md | :lg
  @type variant :: :default | :bordered | :card
  @type inline_empty_state_config :: %{
          text: String.t(),
          icon: atom() | nil,
          class: String.t()
        }
  @type assigns :: %{
          required(:icon) => atom(),
          required(:title) => String.t(),
          optional(:description) => String.t(),
          optional(:size) => size(),
          optional(:variant) => variant(),
          optional(:class) => String.t(),
          optional(:rest) => map()
        }
  @type inline_assigns :: %{
          required(:text) => String.t(),
          optional(:icon) => atom(),
          optional(:class) => String.t(),
          optional(:rest) => map()
        }

  @doc """
  Renders an empty state with icon, title, description, and optional action.

  ## Examples

      <.empty_state
        icon={:building_office_2}
        title="No businesses found"
        description="Create your first business to get started"
      >
        <:action>
          <.button variant="primary" icon_left="lucide-plus">
            Create Business
          </.button>
        </:action>
      </.empty_state>

      <.empty_state
        icon={:magnifying_glass}
        title="No results found"
        description="Try adjusting your search or filters"
      />
  """
  @spec empty_state(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:icon, :atom, required: true,
    doc: "Icon to display in the empty state")
  attr(:title, :string, required: true,
    doc: "Title text for the empty state")
  attr(:description, :string, default: nil,
    doc: "Description text for the empty state")
  attr(:size, :atom, default: :md,
    values: ~w(sm md lg)a,
    doc: "Size variant of the empty state")
  attr(:variant, :atom, default: :default,
    values: ~w(default bordered card)a,
    doc: "Visual variant of the empty state")
  attr(:class, :string, default: "",
    doc: "Additional CSS classes for the container")
  attr(:rest, :global)

  slot(:action)

  @impl true
  def empty_state(assigns) do
    assigns
    |> build_empty_state_attrs()
    |> validate_empty_state_attrs()
    |> assign(:container_class, container_class(assigns))
    |> render_empty_state()
  end

  @spec build_empty_state_attrs(assigns :: assigns()) :: assigns()
  defp build_empty_state_attrs(assigns) do
    # Extract configuration with defaults using functional pattern
    config = %{
      size: Application.get_env(:riva_ash, :empty_state_size, :md),
      variant: Application.get_env(:riva_ash, :empty_state_variant, :default)
    }

    # Immutably update assigns with new values using pipeline
    assigns
    |> Map.put_new(:size, config.size)
    |> Map.put_new(:variant, config.variant)
  end

  @spec validate_empty_state_attrs(assigns :: assigns()) :: assigns()
  defp validate_empty_state_attrs(assigns) do
    with :ok <- validate_icon(assigns[:icon]),
         :ok <- validate_title(assigns[:title]),
         :ok <- validate_description(assigns[:description]),
         :ok <- validate_size(assigns[:size]),
         :ok <- validate_variant(assigns[:variant]),
         :ok <- validate_class(assigns[:class]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid empty state attributes: #{reason}"
    end
  end

  @spec validate_icon(atom()) :: :ok | {:error, String.t()}
  defp validate_icon(icon) when is_atom(icon), do: :ok
  defp validate_icon(_), do: {:error, "icon must be an atom"}

  @spec validate_title(String.t()) :: :ok | {:error, String.t()}
  defp validate_title(title) when is_binary(title) and title != "", do: :ok
  defp validate_title(_), do: {:error, "title must be a non-empty string"}

  @spec validate_description(String.t() | nil) :: :ok | {:error, String.t()}
  defp validate_description(nil), do: :ok
  defp validate_description(description) when is_binary(description) and description != "", do: :ok
  defp validate_description(_), do: {:error, "description must be a non-empty string or nil"}

  @spec validate_size(size() | String.t()) :: :ok | {:error, String.t()}
  defp validate_size(:sm), do: :ok
  defp validate_size(:md), do: :ok
  defp validate_size(:lg), do: :ok
  defp validate_size("sm"), do: :ok
  defp validate_size("md"), do: :ok
  defp validate_size("lg"), do: :ok
  defp validate_size(_), do: {:error, "size must be one of: sm, md, lg"}

  @spec validate_variant(variant() | String.t()) :: :ok | {:error, String.t()}
  defp validate_variant(:default), do: :ok
  defp validate_variant(:bordered), do: :ok
  defp validate_variant(:card), do: :ok
  defp validate_variant("default"), do: :ok
  defp validate_variant("bordered"), do: :ok
  defp validate_variant("card"), do: :ok
  defp validate_variant(_), do: {:error, "variant must be one of: default, bordered, card"}

  @spec validate_class(String.t()) :: :ok | {:error, String.t()}
  defp validate_class(class) when is_binary(class), do: :ok
  defp validate_class(_), do: {:error, "class must be a string"}

  @spec render_empty_state(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_empty_state(assigns) do
    ~H"""
    <div class={@container_class} {@rest}>
      <div class={content_class(@size)}>
        <div class={icon_wrapper_class(@size)}>
          <UIIcon.icon name={@icon} size={icon_size(@size)} class="text-muted-foreground" />
        </div>

        <div class="space-y-2 text-center">
          <UIText.text variant={title_variant(@size)} weight="semibold">
            <%= @title %>
          </UIText.text>

          <%= if @description do %>
            <UIText.text variant={description_variant(@size)} color="muted">
              <%= @description %>
            </UIText.text>
          <% end %>
        </div>

        <%= if @action != [] do %>
          <div class="flex items-center justify-center gap-3 mt-6">
            <%= render_slot(@action) %>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  @doc """
  A simplified empty state for inline use (e.g., in dropdowns or small containers).
  """
  @spec inline_empty_state(inline_assigns :: inline_assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:text, :string, required: true,
    doc: "Text to display in the inline empty state")
  attr(:icon, :atom, default: nil,
    doc: "Optional icon to display")
  attr(:class, :string, default: "",
    doc: "Additional CSS classes for the container")
  attr(:rest, :global)

  @impl true
  def inline_empty_state(assigns) do
    assigns
    |> build_inline_empty_state_attrs()
    |> validate_inline_empty_state_attrs()
    |> render_inline_empty_state()
  end

  @spec build_inline_empty_state_attrs(assigns :: inline_assigns()) :: inline_assigns()
  defp build_inline_empty_state_attrs(assigns) do
    assigns
  end

  @spec validate_inline_empty_state_attrs(assigns :: inline_assigns()) :: inline_assigns()
  defp validate_inline_empty_state_attrs(assigns) do
    with :ok <- validate_text(assigns[:text]),
         :ok <- validate_inline_icon(assigns[:icon]),
         :ok <- validate_inline_class(assigns[:class]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid inline empty state attributes: #{reason}"
    end
  end

  @spec validate_text(String.t()) :: :ok | {:error, String.t()}
  defp validate_text(text) when is_binary(text) and text != "", do: :ok
  defp validate_text(_), do: {:error, "text must be a non-empty string"}

  @spec validate_inline_icon(atom() | nil) :: :ok | {:error, String.t()}
  defp validate_inline_icon(nil), do: :ok
  defp validate_inline_icon(icon) when is_atom(icon), do: :ok
  defp validate_inline_icon(_), do: {:error, "icon must be an atom or nil"}

  @spec validate_inline_class(String.t()) :: :ok | {:error, String.t()}
  defp validate_inline_class(class) when is_binary(class), do: :ok
  defp validate_inline_class(_), do: {:error, "class must be a string"}

  @spec render_inline_empty_state(assigns :: inline_assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_inline_empty_state(assigns) do
    ~H"""
    <div class={["flex items-center justify-center gap-2 py-4 px-6 text-muted-foreground", @class]} {@rest}>
      <%= if @icon do %>
        <UIIcon.icon name={@icon} size="sm" />
      <% end %>
      <UIText.text variant="small" color="muted">
        <%= @text %>
      </UIText.text>
    </div>
    """
  end

  @spec container_class(assigns :: assigns()) :: String.t()
  defp container_class(assigns) do
    # Build container classes using functional composition
    classes = [
      "flex items-center justify-center", # Base classes
      build_variant_classes(assigns.variant),
      assigns.class
    ]
    |> Enum.reject(&(&1 == "")) # Remove empty strings
    |> Enum.join(" ") # Join with spaces

    classes
  end

  # Helper function to build variant-specific classes
  @spec build_variant_classes(variant()) :: String.t()
  defp build_variant_classes(:default), do: "py-12"
  defp build_variant_classes(:bordered), do: "py-12 border-2 border-dashed border-border rounded-lg"
  defp build_variant_classes(:card), do: "py-12 bg-card/50 rounded-lg shadow-sm"
  defp build_variant_classes(_), do: "py-12"

  @spec content_class(size()) :: String.t()
  defp content_class(:sm), do: "max-w-sm mx-auto"
  defp content_class(:md), do: "max-w-md mx-auto"
  defp content_class(:lg), do: "max-w-lg mx-auto"
  defp content_class(_), do: "max-w-md mx-auto"

  @spec icon_wrapper_class(size()) :: String.t()
  defp icon_wrapper_class(size) do
    base = "mx-auto mb-4 flex items-center justify-center rounded-full bg-muted/30"

    size_classes =
      case size do
        :sm -> "h-12 w-12"
        :md -> "h-16 w-16"
        :lg -> "h-20 w-20"
        _ -> "h-16 w-16"
      end

    "#{base} #{size_classes}"
  end

  @spec icon_size(size()) :: String.t()
  defp icon_size(:sm), do: "md"
  defp icon_size(:md), do: "lg"
  defp icon_size(:lg), do: "xl"
  defp icon_size(_), do: "lg"

  @spec title_variant(size()) :: String.t()
  defp title_variant(:sm), do: "h6"
  defp title_variant(:md), do: "h5"
  defp title_variant(:lg), do: "h4"
  defp title_variant(_), do: "h5"

  @spec description_variant(size()) :: String.t()
  defp description_variant(:sm), do: "small"
  defp description_variant(:md), do: "p"
  defp description_variant(:lg), do: "lead"
  defp description_variant(_), do: "p"
end