defmodule RivaAshWeb.Components.Molecules.FormField do
  @moduledoc """
  FormField component that combines label, input, and error messages.
  A molecule component that provides a complete form field experience.
  
  Supports various input types including text, textarea, select, and checkbox
  with consistent validation states, error handling, and accessibility features.
  
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
  alias RivaAshWeb.Components.UI.Input, as: UIInput
  alias RivaAshWeb.Components.UI.Textarea, as: UITextarea
  alias RivaAshWeb.Components.UI.Select, as: UISelect
  alias RivaAshWeb.Components.UI.Checkbox, as: UICheckbox

  @type form_field_type :: :text | :email | :password | :number | :tel | :url
  @type icon_position :: :left | :right
  @type form_field_option :: %{label: String.t(), value: any(), disabled: boolean()}
  @type assigns :: %{
          required(:field) => Phoenix.HTML.FormField.t(),
          optional(:label) => String.t(),
          optional(:type) => form_field_type(),
          optional(:placeholder) => String.t(),
          optional(:helper_text) => String.t(),
          optional(:required) => boolean(),
          optional(:disabled) => boolean(),
          optional(:readonly) => boolean(),
          optional(:icon) => atom(),
          optional(:icon_position) => icon_position(),
          optional(:class) => String.t(),
          optional(:rest) => map()
        }

  @doc """
  Renders a complete form field with label, input, helper text, and error messages.

  ## Examples

      <.form_field
        field={@form[:email]}
        label="Email Address"
        type="email"
        placeholder="user@example.com"
        helper_text="We'll never share your email"
        required={true}
      />

      <.form_field
        field={@form[:password]}
        label="Password"
        type="password"
        icon={:lock_closed}
      />
  """
  @spec form_field(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:field, Phoenix.HTML.FormField, required: true,
    doc: "Phoenix form field with validation and errors")
  attr(:label, :string, default: nil,
    doc: "Label text for the field")
  attr(:type, :atom, default: :text,
    values: ~w(text email password number tel url)a,
    doc: "Input type")
  attr(:placeholder, :string, default: "",
    doc: "Placeholder text for the input")
  attr(:helper_text, :string, default: nil,
    doc: "Helper text displayed below the input")
  attr(:required, :boolean, default: false,
    doc: "Whether the field is required")
  attr(:disabled, :boolean, default: false,
    doc: "Whether the field is disabled")
  attr(:readonly, :boolean, default: false,
    doc: "Whether the field is readonly")
  attr(:icon, :atom, default: nil,
    doc: "Icon to display in the input field")
  attr(:icon_position, :atom, default: :left,
    values: ~w(left right)a,
    doc: "Position of the icon")
  attr(:class, :string, default: "",
    doc: "Additional CSS classes for the field wrapper")
  attr(:rest, :global)

  @impl true
  def form_field(assigns) do
    assigns
    |> build_form_field_attrs()
    |> validate_form_field_attrs()
    |> render_form_field()
  end

  @spec build_form_field_attrs(assigns :: assigns()) :: assigns()
  defp build_form_field_attrs(assigns) do
    # Extract configuration with defaults using functional pattern
    config = %{
      icon_position: Application.get_env(:riva_ash, :form_field_icon_position, :left)
    }

    # Immutably update assigns with new values using pipeline
    assigns
    |> Map.put_new(:icon_position, config.icon_position)
  end

  @spec validate_form_field_attrs(assigns :: assigns()) :: assigns()
  defp validate_form_field_attrs(assigns) do
    with :ok <- validate_field(assigns[:field]),
         :ok <- validate_icon_position(assigns[:icon_position]),
         :ok <- validate_input_type(assigns[:type]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid form field attributes: #{reason}"
    end
  end

  @spec validate_field(Phoenix.HTML.FormField.t()) :: :ok | {:error, String.t()}
  defp validate_field(%{name: name, errors: errors}) when is_binary(name) do
    if is_list(errors) do
      :ok
    else
      {:error, "field.errors must be a list"}
    end
  end
  defp validate_field(_), do: {:error, "field must be a valid Phoenix.HTML.FormField"}

  @spec validate_icon_position(icon_position() | String.t()) :: :ok | {:error, String.t()}
  defp validate_icon_position(:left), do: :ok
  defp validate_icon_position(:right), do: :ok
  defp validate_icon_position("left"), do: :ok
  defp validate_icon_position("right"), do: :ok
  defp validate_icon_position(_), do: {:error, "icon_position must be 'left' or 'right'"}

  @spec validate_input_type(form_field_type() | String.t()) :: :ok | {:error, String.t()}
  defp validate_input_type(:text), do: :ok
  defp validate_input_type(:email), do: :ok
  defp validate_input_type(:password), do: :ok
  defp validate_input_type(:number), do: :ok
  defp validate_input_type(:tel), do: :ok
  defp validate_input_type(:url), do: :ok
  defp validate_input_type(type) when type in ~w(text email password number tel url), do: :ok
  defp validate_input_type(_), do: {:error, "type must be a valid input type"}

  @spec render_form_field(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_form_field(assigns) do
    ~H"""
    <div class={build_field_wrapper_class(@class)}>
      <%= render_form_field_label(@label, @required, @field) %>
      
      <div class="relative">
        <%= render_form_field_icon(@icon, @icon_position) %>
        
        <UIInput.input
          type={@type}
          field={@field}
          placeholder={@placeholder}
          disabled={@disabled}
          readonly={@readonly}
          variant={determine_field_variant(@field)}
          class={build_input_class(@icon, @icon_position)}
          {@rest}
        />
      </div>

      <%= render_form_field_helper_text(@helper_text, @field) %>
      <%= render_form_field_errors(@field) %>
    </div>
    """
  end

  @spec render_form_field_label(String.t() | nil, boolean(), Phoenix.HTML.FormField.t()) :: Phoenix.LiveView.Rendered.t()
  defp render_form_field_label(nil, _required, _field), do: ""
  defp render_form_field_label(label, required, field) do
    assigns = %{label: label, required: required, field: field}

    ~H"""
    <UIText.text variant="label" required={@required || @field.required} class="mb-2">
      <%= @label %>
    </UIText.text>
    """
  end

  @spec render_form_field_icon(atom(), icon_position() | String.t()) :: Phoenix.LiveView.Rendered.t()
  defp render_form_field_icon(nil, _position), do: ""
  defp render_form_field_icon(icon, "left") do
    assigns = %{icon: icon}

    ~H"""
    <div class="absolute left-3 top-1/2 -translate-y-1/2 pointer-events-none">
      <UIIcon.icon name={@icon} size="sm" class="text-muted-foreground" />
    </div>
    """
  end
  defp render_form_field_icon(icon, :left) do
    assigns = %{icon: icon}
    ~H"""
    <div class="absolute left-3 top-1/2 -translate-y-1/2 pointer-events-none">
      <UIIcon.icon name={@icon} size="sm" class="text-muted-foreground" />
    </div>
    """
  end
  defp render_form_field_icon(icon, :right) do
    assigns = %{icon: icon}

    ~H"""
    <div class="absolute right-3 top-1/2 -translate-y-1/2 pointer-events-none">
      <UIIcon.icon name={@icon} size="sm" class="text-muted-foreground" />
    </div>
    """
  end

  @spec determine_field_variant(Phoenix.HTML.FormField.t()) :: String.t()
  defp determine_field_variant(field) do
    if field.errors != [], do: "error", else: "default"
  end

  @spec build_input_class(atom() | nil, icon_position() | String.t()) :: String.t()
  defp build_input_class(nil, _position), do: ""
  defp build_input_class(_icon, :left), do: "pl-10"
  defp build_input_class(_icon, "left"), do: "pl-10"
  defp build_input_class(_icon, :right), do: "pr-10"
  defp build_input_class(_icon, "right"), do: "pr-10"

  @spec render_form_field_helper_text(String.t() | nil, Phoenix.HTML.FormField.t()) :: Phoenix.LiveView.Rendered.t()
  defp render_form_field_helper_text(nil, _field), do: ""
  defp render_form_field_helper_text(helper_text, field) do
    if field.errors == [] do
      assigns = %{helper_text: helper_text}

      ~H"""
      <UIText.text variant="small" color="muted" class="mt-1">
        <%= @helper_text %>
      </UIText.text>
      """
    else
      ""
    end
  end

  @spec render_form_field_errors(Phoenix.HTML.FormField.t()) :: Phoenix.LiveView.Rendered.t()
  defp render_form_field_errors(field) do
    if field.errors != [] do
      assigns = %{errors: field.errors}

      ~H"""
      <div class="mt-1 space-y-1">
        <%= for error <- @errors do %>
          <div class="flex items-center gap-1">
            <UIIcon.icon name={:x_mark} size="xs" class="text-destructive" />
            <UIText.text variant="small" color="destructive">
              <%= translate_error(error) %>
            </UIText.text>
          </div>
        <% end %>
      </div>
      """
    else
      ""
    end
  end

  @spec build_field_wrapper_class(String.t()) :: String.t()
  defp build_field_wrapper_class(class) do
    # Build field wrapper classes using functional composition
    classes = [
      "form-field-wrapper",
      "space-y-1",
      class
    ]
    |> Enum.reject(&(&1 == "")) # Remove empty strings
    |> Enum.join(" ") # Join with spaces

    classes
  end

  @doc """
  Renders a textarea form field.
  """
  @spec textarea_field(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  attr(:field, Phoenix.HTML.FormField, required: true,
    doc: "Phoenix form field with validation and errors")
  attr(:label, :string, default: nil,
    doc: "Label text for the field")
  attr(:placeholder, :string, default: "",
    doc: "Placeholder text for the textarea")
  attr(:helper_text, :string, default: nil,
    doc: "Helper text displayed below the textarea")
  attr(:required, :boolean, default: false,
    doc: "Whether the field is required")
  attr(:disabled, :boolean, default: false,
    doc: "Whether the field is disabled")
  attr(:readonly, :boolean, default: false,
    doc: "Whether the field is readonly")
  attr(:rows, :integer, default: 4,
    doc: "Number of rows for the textarea")
  attr(:class, :string, default: "",
    doc: "Additional CSS classes for the field wrapper")
  attr(:rest, :global)

  def textarea_field(assigns) do
    assigns
    |> build_textarea_attrs()
    |> validate_textarea_attrs()
    |> render_textarea_field()
  end

  @spec build_textarea_attrs(assigns :: map()) :: assigns()
  defp build_textarea_attrs(assigns), do: assigns

  @spec validate_textarea_attrs(assigns :: map()) :: assigns()
  defp validate_textarea_attrs(assigns) do
    with :ok <- validate_field(assigns[:field]),
         :ok <- validate_rows(assigns[:rows]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid textarea field attributes: #{reason}"
    end
  end

  @spec validate_rows(integer()) :: :ok | {:error, String.t()}
  defp validate_rows(rows) when is_integer(rows) and rows > 0, do: :ok
  defp validate_rows(_), do: {:error, "rows must be a positive integer"}

  @spec render_textarea_field(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_textarea_field(assigns) do
    ~H"""
    <div class={build_field_wrapper_class(@class)}>
      <%= render_form_field_label(@label, @required, @field) %>

      <UITextarea.textarea
        field={@field}
        placeholder={@placeholder}
        disabled={@disabled}
        readonly={@readonly}
        rows={@rows}
        variant={determine_field_variant(@field)}
        {@rest}
      />

      <%= render_form_field_helper_text(@helper_text, @field) %>
      <%= render_form_field_errors(@field) %>
    </div>
    """
  end

  @doc """
  Renders a select form field.
  """
  @spec select_field(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  attr(:field, Phoenix.HTML.FormField, required: true,
    doc: "Phoenix form field with validation and errors")
  attr(:label, :string, default: nil,
    doc: "Label text for the field")
  attr(:options, :list, required: true,
    doc: "List of options with :label and :value keys")
  attr(:prompt, :string, default: nil,
    doc: "Prompt text shown when no option is selected")
  attr(:helper_text, :string, default: nil,
    doc: "Helper text displayed below the select")
  attr(:required, :boolean, default: false,
    doc: "Whether the field is required")
  attr(:disabled, :boolean, default: false,
    doc: "Whether the field is disabled")
  attr(:class, :string, default: "",
    doc: "Additional CSS classes for the field wrapper")
  attr(:rest, :global)

  def select_field(assigns) do
    assigns
    |> build_select_attrs()
    |> validate_select_attrs()
    |> render_select_field()
  end

  @spec build_select_attrs(assigns :: map()) :: assigns()
  defp build_select_attrs(assigns), do: assigns

  @spec validate_select_attrs(assigns :: map()) :: assigns()
  defp validate_select_attrs(assigns) do
    with :ok <- validate_field(assigns[:field]),
         :ok <- validate_options(assigns[:options]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid select field attributes: #{reason}"
    end
  end

  @spec validate_options(list(map())) :: :ok | {:error, String.t()}
  defp validate_options(options) when is_list(options) do
    case Enum.all?(options, &valid_option?/1) do
      true -> :ok
      false -> {:error, "All options must have :label and :value keys"}
    end
  end
  defp validate_options(_), do: {:error, "options must be a list"}

  @spec valid_option?(map()) :: boolean()
  defp valid_option?(option) do
    is_map(option) and 
    is_binary(option[:label]) and 
    (is_binary(option[:value]) or is_integer(option[:value]))
  end

  @spec render_select_field(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_select_field(assigns) do
    ~H"""
    <div class={build_field_wrapper_class(@class)}>
      <%= render_form_field_label(@label, @required, @field) %>

      <UISelect.select
        field={@field}
        options={@options}
        prompt={@prompt}
        disabled={@disabled}
        variant={determine_field_variant(@field)}
        {@rest}
      />

      <%= render_form_field_helper_text(@helper_text, @field) %>
      <%= render_form_field_errors(@field) %>
    </div>
    """
  end

  @doc """
  Renders a checkbox form field.
  """
  @spec checkbox_field(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  attr(:field, Phoenix.HTML.FormField, required: true,
    doc: "Phoenix form field with validation and errors")
  attr(:label, :string, required: true,
    doc: "Label text for the checkbox")
  attr(:helper_text, :string, default: nil,
    doc: "Helper text displayed below the checkbox")
  attr(:disabled, :boolean, default: false,
    doc: "Whether the field is disabled")
  attr(:class, :string, default: "",
    doc: "Additional CSS classes for the field wrapper")
  attr(:rest, :global)

  def checkbox_field(assigns) do
    assigns
    |> build_checkbox_attrs()
    |> validate_checkbox_attrs()
    |> render_checkbox_field()
  end

  @spec build_checkbox_attrs(assigns :: map()) :: assigns()
  defp build_checkbox_attrs(assigns), do: assigns

  @spec validate_checkbox_attrs(assigns :: map()) :: assigns()
  defp validate_checkbox_attrs(assigns) do
    with :ok <- validate_field(assigns[:field]),
         :ok <- validate_label(assigns[:label]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid checkbox field attributes: #{reason}"
    end
  end

  @spec validate_label(String.t()) :: :ok | {:error, String.t()}
  defp validate_label(label) when is_binary(label) and label != "", do: :ok
  defp validate_label(_), do: {:error, "label must be a non-empty string"}

  @spec render_checkbox_field(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_checkbox_field(assigns) do
    ~H"""
    <div class={build_field_wrapper_class(@class)}>
      <UICheckbox.checkbox
        field={@field}
        label={@label}
        description={@helper_text}
        disabled={@disabled}
        variant={determine_field_variant(@field)}
        {@rest}
      />

      <%= render_form_field_errors(@field) %>
    </div>
    """
  end

  @spec translate_error({String.t(), list()} | String.t()) :: String.t()
  defp translate_error({msg, opts}) do
    # This is a simplified version. In a real app, you'd use Gettext
    Enum.reduce(opts, msg, fn {key, value}, acc ->
      String.replace(acc, "%{#{key}}", to_string(value))
    end)
  end

  defp translate_error(msg) when is_binary(msg), do: msg
end