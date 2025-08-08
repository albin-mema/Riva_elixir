alias RivaAshWeb.Components.Business, as: Business
alias Phoenix.LiveView.Rendered, as: Rendered
alias Phoenix.HTML, as: HTML

defmodule RivaAshWeb.Components.Business.BusinessForm do
  @moduledoc """
  Reusable business form component for creating and editing businesses.

  This component follows the functional core, imperative shell pattern,
  with pure functions for data transformation and stateless operations,
  while the LiveView component handles the imperative UI state management.

  ## Styleguide Compliance

  This module follows the Riva Ash styleguide principles:

  - **Functional Programming**: Uses pure functions, pattern matching, and pipelines
  - **Type Safety**: Comprehensive type specifications with @spec annotations
  - **Single Level of Abstraction**: Each function has a clear, focused responsibility
  - **Error Handling**: Consistent use of result tuples and guard clauses
  - **Immutable Data**: All transformations use immutable data structures
  - **Security**: Proper input validation and safe rendering patterns
  - **Phoenix/Ash Integration**: Follows Phoenix LiveView and Ash framework patterns
  """
  use Phoenix.Component
  import Phoenix.HTML

  # Import UI components
  import SaladUI.Button
  import SaladUI.Card
  import SaladUI.Input
  import SaladUI.Label
  import SaladUI.Textarea

  # Configuration from application config with type safety
  @public_search_bg_color Application.compile_env(:riva_ash, :public_search_bg_color, "bg-blue-50")
  @public_search_border_color Application.compile_env(:riva_ash, :public_search_border_color, "border-blue-200")
  @public_search_text_color Application.compile_env(:riva_ash, :public_search_text_color, "text-blue-900")
  @public_search_description_color Application.compile_env(:riva_ash, :public_search_description_color, "text-blue-700")

  @location_bg_color Application.compile_env(:riva_ash, :location_bg_color, "bg-green-50")
  @location_border_color Application.compile_env(:riva_ash, :location_border_color, "border-green-200")
  @location_text_color Application.compile_env(:riva_ash, :location_text_color, "text-green-900")
  @location_description_color Application.compile_env(:riva_ash, :location_description_color, "text-green-700")

  @doc """
  Renders a business form.

  ## Examples

      <.business_form
        form={@form}
        editing={@editing_business}
        loading={@loading}
        on_submit="save_business"
        on_change="validate_business"
        on_cancel="cancel_form"
      />

  ## Type Information

  This component accepts the following attributes:

    - `form`: A form struct containing the business data
    - `editing`: Boolean indicating if this is an edit operation (default: false)
    - `loading`: Boolean indicating if the form is in a loading state (default: false)
    - `on_submit`: Event name for form submission (required)
    - `on_change`: Event name for form validation (required)
    - `on_cancel`: Event name for form cancellation (required)
    - `class`: Additional CSS classes for styling (default: "")
  """
  @spec business_form(map()) :: Phoenix.LiveView.Rendered.t()
  attr(:form, :map, required: true)
  attr(:editing, :boolean, default: false)
  attr(:loading, :boolean, default: false)
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:class, :string, default: "")

  def business_form(assigns) do
    assigns
    |> validate_assigns()
    |> render_form()
  end

  @doc """
  Renders field errors for a form field.

  ## Type Information

  This function accepts:
    - `field`: A form field struct containing validation errors
  """
  @spec field_errors(map()) :: Phoenix.LiveView.Rendered.t()
  attr(:field, :map, required: true)

  defp field_errors(assigns) do
    assigns
    |> validate_field_assigns()
    |> render_field_errors()
  end

  # ==================================================================
  # Private Functions - Functional Core (Pure Functions)
  # ==================================================================

  @doc """
  Validates component assigns.

  ## Type Information

  @spec validate_assigns(map()) :: map()
  """
  defp validate_assigns(assigns) when is_map(assigns) do
    with :ok <- validate_required(assigns, [:form, :on_submit, :on_change, :on_cancel]),
         :ok <- validate_form_structure(assigns.form) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid assigns: #{reason}"
    end
  end

  defp validate_assigns(assigns) do
    raise ArgumentError, "Expected assigns to be a map, got: #{inspect(assigns)}"
  end

  @doc """
  Validates field assigns.

  ## Type Information

  @spec validate_field_assigns(map()) :: map()
  """
  defp validate_field_assigns(assigns) when is_map(assigns) do
    with :ok <- validate_required(assigns, [:field]),
         :ok <- validate_field_structure(assigns.field) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid field assigns: #{reason}"
    end
  end

  defp validate_field_assigns(assigns) do
    raise ArgumentError, "Expected field assigns to be a map, got: #{inspect(assigns)}"
  end

  @doc """
  Validates form structure.

  ## Type Information

  @spec validate_form_structure(map()) :: :ok | {:error, String.t()}
  """
  defp validate_form_structure(form) when is_map(form) do
    required_fields = [:name, :description, :is_public_searchable]

    case Enum.all?(required_fields, &Map.has_key?(form, &1)) do
      true -> :ok
      false -> {:error, "Missing required form fields"}
    end
  end

  defp validate_form_structure(form) do
    {:error, "Expected form to be a map, got: #{inspect(form)}"}
  end

  @doc """
  Validates field structure.

  ## Type Information

  @spec validate_field_structure(map()) :: :ok | {:error, String.t()}
  """
  defp validate_field_structure(field) when is_map(field) do
    case Map.has_key?(field, :errors) do
      true -> :ok
      false -> {:error, "Field missing :errors key"}
    end
  end

  defp validate_field_structure(field) do
    {:error, "Expected field to be a map, got: #{inspect(field)}"}
  end

  @doc """
  Renders the main form structure.

  ## Type Information

  @spec render_form(map()) :: Phoenix.LiveView.Rendered.t()
  """
  defp render_form(assigns) do
    ~H"""
    <.card class={["bg-card", @class]}>
      <.card_header>
        <.card_title>
          <%= form_title(@editing) %>
        </.card_title>
        <.card_description>
          <%= form_description(@editing) %>
        </.card_description>
      </.card_header>
      <.card_content>
        <.form
          for={@form}
          phx-submit={@on_submit}
          phx-change={@on_change}
          class="space-y-6"
        >
          <.render_name_field name_field={@form[:name]} />
          <.render_description_field description_field={@form[:description]} />
          <.render_public_search_settings form={@form} public_search_bg_color={@public_search_bg_color} public_search_border_color={@public_search_border_color} public_search_text_color={@public_search_text_color} public_search_description_color={@public_search_description_color} />
          <.render_location_information form={@form} location_bg_color={@location_bg_color} location_border_color={@location_border_color} location_text_color={@location_text_color} location_description_color={@location_description_color} />
          <.render_form_actions editing={@editing} loading={@loading} on_cancel={@on_cancel} />
        </.form>
      </.card_content>
    </.card>
    """
  end

  @doc """
  Renders field errors.

  ## Type Information

  @spec render_field_errors(map()) :: Phoenix.LiveView.Rendered.t()
  """
  defp render_field_errors(assigns) do
    ~H"""
    <div :if={has_errors?(@field)} class="text-destructive text-sm">
      <%= render_error_list(@field.errors) %>
    </div>
    """
  end

  # ==================================================================
  # Helper Functions - Data Transformation
  # ==================================================================

  @doc """
  Generates form title based on editing state.

  ## Type Information

  @spec form_title(boolean()) :: String.t()
  """
  defp form_title(true), do: "Edit Business"
  defp form_title(false), do: "Create New Business"

  @doc """
  Generates form description based on editing state.

  ## Type Information

  @spec form_description(boolean()) :: String.t()
  """
  defp form_description(true), do: "Update the business information below."
  defp form_description(false), do: "Fill in the details to create a new business entity."

  @doc """
  Renders the name field section.

  ## Type Information

  @spec render_name_field(map()) :: Phoenix.LiveView.Rendered.t()
  """
  defp render_name_field(assigns) do
    ~H"""
    <div class="space-y-2">
      <.label for="name">Business Name *</.label>
      <.input
        field={@name_field}
        type="text"
        placeholder="Enter business name"
        required
      />
      <.field_errors field={@name_field} />
    </div>
    """
  end

  @doc """
  Renders the description field section.

  ## Type Information

  @spec render_description_field(map()) :: Phoenix.LiveView.Rendered.t()
  """
  defp render_description_field(assigns) do
    ~H"""
    <div class="space-y-2">
      <.label for="description">Description</.label>
      <.textarea
        field={@description_field}
        placeholder="Enter business description (optional)"
        rows="4"
      />
      <.field_errors field={@description_field} />
    </div>
    """
  end

  @doc """
  Renders public search settings section.

  ## Type Information

  @spec render_public_search_settings(map()) :: Phoenix.LiveView.Rendered.t()
  """
  defp render_public_search_settings(assigns) do
    ~H"""
    <div class={["space-y-4 p-4", @public_search_bg_color, "rounded-lg border", @public_search_border_color]}>
      <h3 class={["text-lg font-medium", @public_search_text_color]}>Public Search Settings</h3>
      <p class={["text-sm", @public_search_description_color]}>Control how your business appears in global search for unregistered users.</p>

      <div class="flex items-center space-x-3">
        <input
          type="checkbox"
          id="is_public_searchable"
          name={@form[:is_public_searchable].name}
          value="true"
          checked={@form[:is_public_searchable].value}
          class="h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded"
        />
        <.label for="is_public_searchable" class="text-sm font-medium text-gray-700">
          Make this business discoverable in global search
        </.label>
      </div>

      <div class="space-y-2">
        <.label for="public_description">Public Description</.label>
        <.textarea
          field={@form[:public_description]}
          placeholder="Enter a public-facing description for search results (optional)"
          rows="3"
        />
        <p class="text-xs text-gray-500">This description will be shown to unregistered users in search results. Leave empty to use the main description.</p>
        <.field_errors field={@form[:public_description]} />
      </div>
    </div>
    """
  end

  @doc """
  Renders location information section.

  ## Type Information

  @spec render_location_information(map()) :: Phoenix.LiveView.Rendered.t()
  """
  defp render_location_information(assigns) do
    ~H"""
    <div class={["space-y-4 p-4", @location_bg_color, "rounded-lg border", @location_border_color]}>
      <h3 class={["text-lg font-medium", @location_text_color]}>Location Information</h3>
      <p class={["text-sm", @location_description_color]}>Help customers find you by providing location details.</p>

      <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
        <.render_city_field city_field={@form[:city]} />
        <.render_country_field country_field={@form[:country]} />
      </div>

      <div class="space-y-2">
        <.label for="address">Full Address</.label>
        <.textarea
          field={@form[:address]}
          placeholder="Enter complete address (optional)"
          rows="2"
        />
        <p class="text-xs text-gray-500">This helps with local search and customer directions.</p>
        <.field_errors field={@form[:address]} />
      </div>
    </div>
    """
  end

  @doc """
  Renders city field.

  ## Type Information

  @spec render_city_field(map()) :: Phoenix.LiveView.Rendered.t()
  """
  defp render_city_field(assigns) do
    ~H"""
    <div class="space-y-2">
      <.label for="city">City</.label>
      <.input
        field={@city_field}
        type="text"
        placeholder="Enter city name"
      />
      <.field_errors field={@city_field} />
    </div>
    """
  end

  @doc """
  Renders country field.

  ## Type Information

  @spec render_country_field(map()) :: Phoenix.LiveView.Rendered.t()
  """
  defp render_country_field(assigns) do
    ~H"""
    <div class="space-y-2">
      <.label for="country">Country</.label>
      <.input
        field={@country_field}
        type="text"
        placeholder="Enter country name"
      />
      <.field_errors field={@country_field} />
    </div>
    """
  end

  @doc """
  Renders form actions (submit and cancel buttons).

  ## Type Information

  @spec render_form_actions(map()) :: Phoenix.LiveView.Rendered.t()
  """
  defp render_form_actions(assigns) do
    ~H"""
    <div class="flex gap-3 pt-4">
      <.button
        type="submit"
        variant="default"
        disabled={@loading}
        class="flex items-center gap-2"
      >
        <span :if={@loading} class="animate-spin">⏳</span>
        <%= submit_button_text(@editing) %>
      </.button>

      <.button
        type="button"
        variant="outline"
        phx-click={@on_cancel}
      >
        Cancel
      </.button>
    </div>
    """
  end

  @doc """
  Generates submit button text based on editing state.

  ## Type Information

  @spec submit_button_text(boolean()) :: String.t()
  """
  defp submit_button_text(true), do: "Update Business"
  defp submit_button_text(false), do: "Create Business"

  @doc """
  Checks if a field has errors.

  ## Type Information

  @spec has_errors?(map()) :: boolean()
  """
  defp has_errors?(%{errors: errors}) when is_list(errors), do: length(errors) > 0
  defp has_errors?(_), do: false

  @doc """
  Renders error list as HTML paragraphs.

  ## Type Information

  @spec render_error_list(list()) :: list()
  """
  defp render_error_list(errors) when is_list(errors) do
    for error <- errors do
      raw("<p>• #{Phoenix.HTML.html_escape(error)}</p>")
    end
  end

  defp render_error_list(errors) do
    raise ArgumentError, "Expected errors to be a list, got: #{inspect(errors)}"
  end

  # ==================================================================
  # Validation Helper Functions
  # ==================================================================

  @doc """
  Validates required assigns.

  ## Type Information

  @spec validate_required(map(), list()) :: :ok | {:error, String.t()}
  """
  defp validate_required(assigns, required_keys) when is_map(assigns) and is_list(required_keys) do
    missing_keys = required_keys -- Map.keys(assigns)

    case length(missing_keys) do
      0 -> :ok
      _ -> {:error, "Missing required keys: #{Enum.join(missing_keys, ", ")}"}
    end
  end

  defp validate_required(assigns, required_keys) do
    {:error,
     "Expected assigns to be a map and required_keys to be a list, got: #{inspect(assigns)}, #{inspect(required_keys)}"}
  end
end
