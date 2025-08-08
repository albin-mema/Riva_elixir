alias RivaAshWeb.Components.Molecules, as: Molecules
alias RivaAshWeb.Components.UI, as: UI
alias Phoenix.LiveView.Rendered, as: Rendered

defmodule RivaAshWeb.Components.Molecules.Card do
  @moduledoc """
  Card component for grouping related content.

  Delegates container rendering to the canonical design-system Card while
  preserving the header/body/footer API for molecules.

  Provides a flexible container with configurable variants, padding, and
  optional header/footer sections for consistent content organization.

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
  alias RivaAshWeb.Components.UI.Card, as: UICard
  alias RivaAshWeb.Components.UI.Text, as: UIText

  @type card_variant :: :elevated | :bordered | :ghost
  @type padding_variant :: :none | :compact | :normal | :spacious
  @type assigns :: %{
          optional(:variant) => card_variant(),
          optional(:padding) => padding_variant(),
          optional(:class) => String.t(),
          optional(:rest) => map(),
          optional(:header) => list(),
          optional(:body) => list(),
          optional(:footer) => list()
        }

  @doc """
  Renders a card container with optional header and footer.

  ## Examples

      <.card>
        <:header>
          <.card_title>Card Title</.card_title>
          <.card_description>Card description</.card_description>
        </:header>
        <:body>
          Card content goes here
        </:body>
        <:footer>
          Footer content
        </:footer>
      </.card>

      <.card variant="bordered">
        <:body>Simple card with just body content</:body>
      </.card>
  """
  @spec card(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:variant, :atom,
    default: :elevated,
    values: ~w(elevated bordered ghost)a,
    doc: "Visual variant of the card"
  )

  attr(:padding, :atom,
    default: :normal,
    values: ~w(none compact normal spacious)a,
    doc: "Internal spacing of the card"
  )

  attr(:class, :string,
    default: "",
    doc: "Additional CSS classes for the card container"
  )

  attr(:rest, :global)

  slot(:header, doc: "Optional header content")
  slot(:body, doc: "Main body content (required)")
  slot(:footer, doc: "Optional footer content")

  @impl true
  def card(assigns) do
    assigns
    |> build_card_attrs()
    |> validate_card_attrs()
    |> render_card()
  end

  # Functional core: Pure functions for data transformation and validation

  @spec build_card_attrs(assigns :: assigns()) :: assigns()
  defp build_card_attrs(assigns) do
    # Extract configuration with defaults using functional pattern
    config = %{
      variant: Application.get_env(:riva_ash, :card_variant, :elevated),
      padding: Application.get_env(:riva_ash, :card_padding, :normal)
    }

    # Immutably update assigns with new values using pipeline
    assigns
    |> Map.put_new(:variant, config.variant)
    |> Map.put_new(:padding, config.padding)
  end

  @spec validate_card_attrs(assigns :: assigns()) :: assigns()
  defp validate_card_attrs(assigns) do
    with :ok <- validate_variant(assigns[:variant]),
         :ok <- validate_padding(assigns[:padding]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid card attributes: #{reason}"
    end
  end

  @spec validate_variant(card_variant() | String.t()) :: :ok | {:error, String.t()}
  defp validate_variant(:elevated), do: :ok
  defp validate_variant(:bordered), do: :ok
  defp validate_variant(:ghost), do: :ok
  defp validate_variant("elevated"), do: :ok
  defp validate_variant("bordered"), do: :ok
  defp validate_variant("ghost"), do: :ok
  defp validate_unmatchedvariant(_unmatched), do: {:error, "Variant must be one of: elevated, bordered, ghost"}

  @spec validate_padding(padding_variant() | String.t()) :: :ok | {:error, String.t()}
  defp validate_padding(:none), do: :ok
  defp validate_padding(:compact), do: :ok
  defp validate_padding(:normal), do: :ok
  defp validate_padding(:spacious), do: :ok
  defp validate_padding("none"), do: :ok
  defp validate_padding("compact"), do: :ok
  defp validate_padding("normal"), do: :ok
  defp validate_padding("spacious"), do: :ok
  defp validate_unmatchedpadding(_unmatched), do: {:error, "Padding must be one of: none, compact, normal, spacious"}

  @spec render_card(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_card(assigns) do
    assigns = assign(assigns, :card_class, build_card_class(assigns))

    ~H"""
    <UICard.card class={@card_class} {@rest}>
      <%= render_card_header(@header, @padding) %>
      <%= render_card_body(@body, @padding) %>
      <%= render_card_footer(@footer, @padding) %>
    </UICard.card>
    """
  end

  @spec render_card_header(list(), padding_variant()) :: Phoenix.LiveView.Rendered.t()
  defp render_card_header([], _padding), do: ""

  defp render_card_header(header, padding) do
    assigns = %{header: header, padding: padding}

    ~H"""
    <div class={build_header_class(padding)}>
      <%= render_slot(header) %>
    </div>
    """
  end

  @spec render_card_body(list(), padding_variant()) :: Phoenix.LiveView.Rendered.t()
  defp render_card_body(body, padding) do
    assigns = %{body: body, padding: padding}

    ~H"""
    <div class={build_body_class(@padding)}>
      <%= render_slot(@body) %>
    </div>
    """
  end

  @spec render_card_footer(list(), padding_variant()) :: Phoenix.LiveView.Rendered.t()
  defp render_card_footer([], _padding), do: ""

  defp render_card_footer(footer, padding) do
    assigns = %{footer: footer, padding: padding}

    ~H"""
    <div class={build_footer_class(@padding)}>
      <%= render_slot(@footer) %>
    </div>
    """
  end

  @spec build_card_class(assigns :: assigns()) :: String.t()
  defp build_card_class(assigns) do
    # Build CSS classes using functional pipeline
    classes =
      [
        # Base classes
        "rounded-lg overflow-hidden",
        build_variant_classes(assigns.variant),
        build_padding_classes(assigns.padding),
        assigns.class
      ]
      # Remove empty strings
      |> Enum.reject(&(&1 == ""))
      # Join with spaces
      |> Enum.join(" ")

    classes
  end

  @spec build_variant_classes(card_variant()) :: String.t()
  defp build_variant_classes(:elevated), do: "bg-card text-card-foreground shadow-sm hover:shadow-md transition-shadow"
  defp build_variant_classes(:bordered), do: "bg-card text-card-foreground border border-border"
  defp build_variant_classes(:ghost), do: "bg-transparent"

  @spec build_header_class(padding_variant()) :: String.t()
  defp build_header_class(padding) do
    # Build header classes using functional composition
    classes =
      [
        # Base header styles
        "border-b border-border",
        # Padding classes
        build_padding_classes(padding)
      ]
      # Join with spaces
      |> Enum.join(" ")

    classes
  end

  @spec build_body_class(padding_variant()) :: String.t()
  defp build_body_class(padding) do
    # Body only needs padding classes
    build_padding_classes(padding)
  end

  @spec build_footer_class(padding_variant()) :: String.t()
  defp build_footer_class(padding) do
    # Build footer classes using functional composition
    classes =
      [
        # Base footer styles
        "border-t border-border bg-muted/50",
        # Padding classes
        build_padding_classes(padding)
      ]
      # Join with spaces
      |> Enum.join(" ")

    classes
  end

  @spec build_padding_classes(padding_variant()) :: String.t()
  defp build_padding_classes(:none), do: ""
  defp build_padding_classes(:compact), do: "p-4"
  defp build_padding_classes(:normal), do: "p-6"
  defp build_padding_classes(:spacious), do: "p-8"

  @doc """
  Card title component for use in card headers.
  """
  @spec card_title(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  attr(:class, :string,
    default: "",
    doc: "Additional CSS classes for the title"
  )

  slot(:inner_block,
    required: true,
    doc: "Title content"
  )

  def card_title(assigns) do
    # Render title using functional composition
    assigns
    |> Map.put_new(:variant, "h3")
    |> Map.put_new(:color, "text-foreground")
    |> render_title_component()
  end

  # Private helper for title rendering
  @spec render_title_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_title_component(assigns) do
    ~H"""
    <UIText.text variant={@variant} class={[@color, @class]}>
      <%= render_slot(@inner_block) %>
    </UIText.text>
    """
  end

  @doc """
  Card description component for use in card headers.
  """
  @spec card_description(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  attr(:class, :string,
    default: "",
    doc: "Additional CSS classes for the description"
  )

  slot(:inner_block,
    required: true,
    doc: "Description content"
  )

  def card_description(assigns) do
    # Render description using functional composition
    assigns
    |> Map.put_new(:variant, "small")
    |> Map.put_new(:color, "muted")
    |> Map.put_new(:margin_class, "mt-1")
    |> render_description_component()
  end

  # Private helper for description rendering
  @spec render_description_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_description_component(assigns) do
    ~H"""
    <UIText.text variant={@variant} color={@color} class={[@margin_class, @class]}>
      <%= render_slot(@inner_block) %>
    </UIText.text>
    """
  end
end
